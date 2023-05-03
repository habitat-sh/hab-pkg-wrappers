use std::{
    collections::HashMap,
    hash::Hash,
    io::Write,
    path::{Path, PathBuf},
};

use exec::Command;
use hab_pkg_wrappers::{env::CommonEnvironment, opts_parser, util::PrefixedArg};
use path_absolutize::Absolutize;

struct LDEnvironment {
    pub common: CommonEnvironment,
    pub dynamic_linker: Option<PathBuf>,
    pub ld_run_path: Vec<PathBuf>,
    pub prefix: Option<PathBuf>,
    pub enforce_purity: bool,
    pub tmp: PathBuf,
    pub tmpdir: PathBuf,
    pub temp: PathBuf,
    pub tempdir: PathBuf,
}

impl Default for LDEnvironment {
    fn default() -> Self {
        Self {
            common: CommonEnvironment::default(),
            dynamic_linker: std::env::var("HAB_DYNAMIC_LINKER").ok().map(PathBuf::from),
            ld_run_path: std::env::var("HAB_LD_RUN_PATH")
                .map(|value| {
                    value
                        .split(":")
                        .map(PathBuf::from)
                        .collect::<Vec<PathBuf>>()
                })
                .unwrap_or_default(),
            prefix: std::env::var("PREFIX").ok().map(PathBuf::from),
            enforce_purity: std::env::var("HAB_ENFORCE_PURITY")
                .map(|value| value == "1")
                .unwrap_or(true),
            tmp: std::env::var("TMP")
                .map(PathBuf::from)
                .unwrap_or(std::env::temp_dir()),
            tmpdir: std::env::var("TMPDIR")
                .map(PathBuf::from)
                .unwrap_or(std::env::temp_dir()),
            temp: std::env::var("TEMP")
                .map(PathBuf::from)
                .unwrap_or(std::env::temp_dir()),
            tempdir: std::env::var("TEMPDIR")
                .map(PathBuf::from)
                .unwrap_or(std::env::temp_dir()),
        }
    }
}

trait LDPathArg {
    fn is_bad_path(&self, env: &LDEnvironment) -> bool;
    fn is_pkg_path(&self, env: &LDEnvironment) -> bool;
    fn absolute_path(&self, env: &LDEnvironment) -> PathBuf;
    fn is_shared_library(&self) -> bool;
    fn is_static_library(&self) -> bool;
}

impl<T> LDPathArg for T
where
    T: AsRef<Path>,
{
    // Checks if a path is a bad path
    fn is_bad_path(&self, env: &LDEnvironment) -> bool {
        if !env.enforce_purity {
            return false;
        }
        let path = self
            .as_ref()
            .absolutize_from(&env.common.cwd)
            .unwrap()
            .to_path_buf();
        !(path.starts_with(env.common.fs_root.join("hab").join("pkgs"))
            || path.starts_with(env.common.fs_root.join("hab").join("cache"))
            || path.starts_with(
                env.common
                    .fs_root
                    .join(&env.tmp.components().skip(1).collect::<PathBuf>()),
            )
            || path.starts_with(
                env.common
                    .fs_root
                    .join(&env.tmpdir.components().skip(1).collect::<PathBuf>()),
            )
            || path.starts_with(
                env.common
                    .fs_root
                    .join(&env.temp.components().skip(1).collect::<PathBuf>()),
            )
            || path.starts_with(
                env.common
                    .fs_root
                    .join(&env.tempdir.components().skip(1).collect::<PathBuf>()),
            ))
    }
    fn is_pkg_path(&self, env: &LDEnvironment) -> bool {
        let path = self.as_ref();
        path.starts_with(env.common.fs_root.join("hab").join("pkgs"))
    }

    fn absolute_path(&self, env: &LDEnvironment) -> PathBuf {
        self.as_ref()
            .absolutize_from(&env.common.cwd)
            .unwrap()
            .to_path_buf()
    }

    fn is_shared_library(&self) -> bool {
        self.as_ref()
            .file_name()
            .and_then(|p| p.to_str())
            .map(|p| p.ends_with(".so") || p.contains(".so."))
            .unwrap_or(false)
    }

    fn is_static_library(&self) -> bool {
        self.as_ref().ends_with(".a")
    }
}

#[derive(Debug, Hash, PartialEq, Eq)]
enum LibraryReference {
    Name(String),
    FileName(PathBuf),
    FilePath(PathBuf),
}

#[derive(Debug)]
struct LibraryReferenceState {
    linker_state: LinkerState,
    library_name: Option<String>,
    results: Vec<LibraryLinkResult>,
}

#[derive(Debug, PartialEq, Eq)]
enum LibraryLinkResult {
    StaticLibraryLinked(PathBuf),
    SharedLibraryLinked(PathBuf),
    SharedLibraryLinkAsNeeded(PathBuf),
}

#[derive(Debug)]
enum LDArgument<'a> {
    LinkArgsFile(&'a str),
    LibrarySearchPath(&'a str, bool),
    LibraryName(&'a str, bool),
    LibraryFileName(&'a str, bool),
    LibraryFilePath(&'a str),
    RPath(&'a str, bool),
    DynamicLinker(&'a str, bool),
    Output(&'a str, bool),
    SOName(&'a str, bool),
    Plugin(&'a str, bool),
    PluginOpt(&'a str, bool),
    LinkStatic,
    LinkDynamic,
    LinkAsNeeded,
    NoLinkAsNeeded,
    CopyDTNeededEntries,
    NoCopyDTNeededEntries,
    PushState,
    PopState,
}

impl<'a> LDArgument<'a> {
    fn parse(
        previous_argument: Option<&'a str>,
        current_argument: &'a str,
    ) -> Option<LDArgument<'a>> {
        match (previous_argument, current_argument) {
            // Push State
            (_, "-push-state" | "--push-state") => Some(LDArgument::PushState),
            // Pop State
            (_, "-pop-state" | "--pop-state") => Some(LDArgument::PopState),
            // Link as needed
            (_, "-as-needed" | "--as-needed") => Some(LDArgument::LinkAsNeeded),
            // No link as needed
            (_, "-no-as-needed" | "--no-as-needed") => Some(LDArgument::NoLinkAsNeeded),
            // Static link mode
            (_, "-Bstatic" | "-dn" | "-non_shared" | "--non_shared" | "-static" | "--static") => {
                Some(LDArgument::LinkStatic)
            }
            // Dynamic link mode
            (_, "-Bdynamic" | "-dy" | "-call_shared" | "--call_shared") => {
                Some(LDArgument::LinkDynamic)
            }
            // Copy DT needed entries
            (_, "-copy-dt-needed-entries" | "--copy-dt-needed-entries") => {
                Some(LDArgument::CopyDTNeededEntries)
            }
            // No Copy DT needed entries
            (_, "-no-copy-dt-needed-entries" | "--no-copy-dt-needed-entries") => {
                Some(LDArgument::NoCopyDTNeededEntries)
            }
            // File with linker arguments
            (_, current_value) if current_value.is_prefixed_with("@") => Some(
                LDArgument::LinkArgsFile(current_value.strip_prefix("@").unwrap()),
            ),
            // Output argument, the single dash form of '-output' is not allowed.
            // If the argument is '-output' it is recognized as '--output utput'
            (Some("-o" | "--output"), current_value) => {
                Some(LDArgument::Output(current_value, false))
            }
            (_, current_value) if current_value.is_prefixed_with("-o") => Some(LDArgument::Output(
                current_value.strip_prefix("-o").unwrap(),
                true,
            )),
            (_, current_value) if current_value.is_prefixed_with("--output=") => Some(
                LDArgument::Output(current_value.strip_prefix("--output=").unwrap(), true),
            ),
            // SO name argument
            (Some("-h" | "-soname" | "--soname"), current_value) => {
                Some(LDArgument::SOName(current_value, false))
            }
            (_, current_value) if current_value.is_prefixed_with("-h") => Some(LDArgument::SOName(
                current_value.strip_prefix("-h").unwrap(),
                true,
            )),
            (_, current_value) if current_value.is_prefixed_with("-soname=") => Some(
                LDArgument::SOName(current_value.strip_prefix("-soname=").unwrap(), true),
            ),
            (_, current_value) if current_value.is_prefixed_with("--soname=") => Some(
                LDArgument::SOName(current_value.strip_prefix("--soname=").unwrap(), true),
            ),
            // Plugin argument
            (Some("-plugin" | "--plugin"), current_value) => {
                Some(LDArgument::Plugin(current_value, false))
            }
            (_, current_value) if current_value.is_prefixed_with("-plugin=") => Some(
                LDArgument::Plugin(current_value.strip_prefix("-plugin=").unwrap(), true),
            ),
            (_, current_value) if current_value.is_prefixed_with("--plugin=") => Some(
                LDArgument::Plugin(current_value.strip_prefix("--plugin=").unwrap(), true),
            ),
            // Plugin opt
            (Some("-plugin-opt" | "--plugin-opt"), current_value) => {
                Some(LDArgument::PluginOpt(current_value, false))
            }
            (_, current_value) if current_value.is_prefixed_with("-plugin-opt=") => Some(
                LDArgument::PluginOpt(current_value.strip_prefix("-plugin-opt=").unwrap(), true),
            ),
            (_, current_value) if current_value.is_prefixed_with("--plugin-opt=") => Some(
                LDArgument::PluginOpt(current_value.strip_prefix("--plugin-opt=").unwrap(), true),
            ),
            // Dynamic Linker arguments
            (Some("-I" | "-dynamic-linker" | "--dynamic-linker"), current_value) => {
                Some(LDArgument::DynamicLinker(current_value, false))
            }
            (_, current_value) if current_value.is_prefixed_with("-I") => Some(
                LDArgument::DynamicLinker(current_value.strip_prefix("-I").unwrap(), true),
            ),
            (_, current_value) if current_value.is_prefixed_with("-dynamic-linker=") => {
                Some(LDArgument::DynamicLinker(
                    current_value.strip_prefix("-dynamic-linker=").unwrap(),
                    true,
                ))
            }
            (_, current_value) if current_value.is_prefixed_with("--dynamic-linker=") => {
                Some(LDArgument::DynamicLinker(
                    current_value.strip_prefix("--dynamic-linker=").unwrap(),
                    true,
                ))
            }
            // Library Search Path arguments
            (Some("-L" | "-Y" | "-library-path" | "--library-path"), current_value) => {
                Some(LDArgument::LibrarySearchPath(current_value, false))
            }
            (_, current_value) if current_value.is_prefixed_with("-L") => Some(
                LDArgument::LibrarySearchPath(current_value.strip_prefix("-L").unwrap(), true),
            ),
            (_, current_value) if current_value.is_prefixed_with("-Y") => Some(
                LDArgument::LibrarySearchPath(current_value.strip_prefix("-Y").unwrap(), true),
            ),
            (_, current_value) if current_value.is_prefixed_with("-library-path=") => {
                Some(LDArgument::LibrarySearchPath(
                    current_value.strip_prefix("-library-path=").unwrap(),
                    true,
                ))
            }
            (_, current_value) if current_value.is_prefixed_with("--library-path=") => {
                Some(LDArgument::LibrarySearchPath(
                    current_value.strip_prefix("--library-path=").unwrap(),
                    true,
                ))
            }

            // RPath arguments
            (Some("-rpath" | "--rpath"), current_value) => {
                Some(LDArgument::RPath(current_value, false))
            }
            (_, current_value) if current_value.is_prefixed_with("-rpath=") => Some(
                LDArgument::RPath(current_value.strip_prefix("-rpath=").unwrap(), true),
            ),
            (_, current_value) if current_value.is_prefixed_with("--rpath=") => Some(
                LDArgument::RPath(current_value.strip_prefix("--rpath=").unwrap(), true),
            ),
            // RPath compatibility arguments
            (Some("-R" | "-just-symbols" | "--just-symbols"), current_value) => {
                if !PathBuf::from(current_value).is_file() {
                    Some(LDArgument::RPath(current_value, false))
                } else {
                    None
                }
            }
            (_, current_value) if current_value.is_prefixed_with("-R") => {
                let path = current_value.strip_prefix("-R").unwrap();
                if !PathBuf::from(path).is_file() {
                    Some(LDArgument::RPath(current_value, true))
                } else {
                    None
                }
            }
            (_, current_value) if current_value.is_prefixed_with("-just-symbols=") => {
                let path = current_value.strip_prefix("-just-symbols=").unwrap();
                if !PathBuf::from(path).is_file() {
                    Some(LDArgument::RPath(current_value, true))
                } else {
                    None
                }
            }
            (_, current_value) if current_value.is_prefixed_with("--just-symbols=") => {
                let path = current_value.strip_prefix("--just-symbols=").unwrap();
                if !PathBuf::from(path).is_file() {
                    Some(LDArgument::RPath(current_value, true))
                } else {
                    None
                }
            }
            // Library
            (Some("-l" | "-library" | "--library"), current_value) => {
                if current_value.is_prefixed_with(":") {
                    Some(LDArgument::LibraryFileName(
                        current_value.strip_prefix(":").unwrap(),
                        false,
                    ))
                } else {
                    Some(LDArgument::LibraryName(current_value, false))
                }
            }
            (_, current_value) if current_value.is_prefixed_with("-l:") => Some(
                LDArgument::LibraryFileName(current_value.strip_prefix("-l:").unwrap(), true),
            ),
            (_, current_value) if current_value.is_prefixed_with("-l") => Some(
                LDArgument::LibraryName(current_value.strip_prefix("-l").unwrap(), true),
            ),
            (_, current_value) if current_value.is_prefixed_with("-library=:") => {
                Some(LDArgument::LibraryFileName(
                    current_value.strip_prefix("-library=:").unwrap(),
                    true,
                ))
            }
            (_, current_value) if current_value.is_prefixed_with("-library=") => Some(
                LDArgument::LibraryName(current_value.strip_prefix("-library=").unwrap(), true),
            ),
            (_, current_value) if current_value.is_prefixed_with("--library=:") => {
                Some(LDArgument::LibraryFileName(
                    current_value.strip_prefix("--library=:").unwrap(),
                    true,
                ))
            }
            (_, current_value) if current_value.is_prefixed_with("--library=") => Some(
                LDArgument::LibraryName(current_value.strip_prefix("--library=").unwrap(), true),
            ),
            // Direct Library reference
            (_, current_value) if current_value.is_shared_library() => {
                Some(LDArgument::LibraryFilePath(current_value))
            }
            _ => None,
        }
    }
}

#[derive(Debug, Default, Clone, Copy)]
struct LinkerState {
    is_static: bool,
    is_as_needed: bool,
    is_copy_dt_needed_entries: bool,
}

fn effective_library_name(library_reference: &LibraryReference) -> Option<String> {
    match library_reference {
        LibraryReference::Name(library_name) => Some(library_name.to_string()),
        LibraryReference::FileName(library_file_name)
        | LibraryReference::FilePath(library_file_name) => library_file_name
            .file_name()
            .and_then(|file_name| file_name.to_str())
            .and_then(|file_name| file_name.strip_prefix("lib"))
            .and_then(|file_name| file_name.split_once("."))
            .map(|(library_name, _)| library_name.to_string()),
    }
}
fn parse_linker_arguments(
    arguments: impl Iterator<Item = String>,
    env: &LDEnvironment,
) -> Vec<String> {
    let mut linker_state_stack = vec![];
    let mut current_linker_state = LinkerState::default();
    let mut filtered_arguments = vec![];
    let mut previous_argument: Option<String> = None;
    let mut bad_plugin = false;

    let mut library_search_paths = vec![];
    let mut rpaths: Vec<PathBuf> = vec![];
    let mut additional_rpaths: Vec<PathBuf> = vec![];
    let mut library_references = Vec::new();
    let mut libraries_linked = HashMap::new();

    for argument in arguments {
        let mut skip_argument = false;
        let mut skip_prev_argument = false;
        if let Some(known_argument) =
            LDArgument::parse(previous_argument.as_ref().map(|x| x.as_str()), &argument)
        {
            match known_argument {
                LDArgument::Output(_, _) | LDArgument::SOName(_, _) => {
                    // Nothing to do for these arguments
                }
                LDArgument::LinkArgsFile(_) => {}
                LDArgument::LibrarySearchPath(search_path, is_prefixed) => {
                    if search_path.is_bad_path(env) {
                        skip_argument = true;
                        skip_prev_argument = !is_prefixed;
                    } else {
                        let search_path = search_path.absolute_path(env);
                        if !library_search_paths.contains(&search_path) {
                            library_search_paths.push(search_path);
                        }
                    }
                }
                LDArgument::LibraryName(library_name, _) => {
                    let library_reference = LibraryReference::Name(library_name.to_string());
                    let library_name = effective_library_name(&library_reference);
                    library_references.push((
                        library_reference,
                        LibraryReferenceState {
                            linker_state: current_linker_state,
                            library_name,
                            results: Vec::new(),
                        },
                    ));
                }
                LDArgument::LibraryFileName(library_file_name, _) => {
                    let library_reference = LibraryReference::FileName(library_file_name.into());
                    let library_name = effective_library_name(&library_reference);
                    library_references.push((
                        library_reference,
                        LibraryReferenceState {
                            linker_state: current_linker_state,
                            library_name,
                            results: Vec::new(),
                        },
                    ));
                }
                LDArgument::LibraryFilePath(library_file_path) => {
                    let library_file_path = library_file_path.absolute_path(env);
                    if library_file_path.is_bad_path(env) {
                        skip_argument = true;
                    } else {
                        let library_reference = LibraryReference::FilePath(library_file_path);
                        let library_name = effective_library_name(&library_reference);
                        library_references.push((
                            library_reference,
                            LibraryReferenceState {
                                linker_state: current_linker_state,
                                library_name,
                                results: Vec::new(),
                            },
                        ));
                    }
                }
                LDArgument::RPath(rpath, is_prefixed) => {
                    if rpath.is_bad_path(env) {
                        skip_argument = true;
                        skip_prev_argument = !is_prefixed;
                    } else {
                        let rpath = rpath.absolute_path(env);
                        if !rpaths.contains(&rpath) {
                            rpaths.push(rpath);
                        }
                    }
                }
                LDArgument::DynamicLinker(dynamic_linker_path, is_prefixed) => {
                    if let Some(env_dynamic_linker_path) = env.dynamic_linker.as_ref() {
                        if dynamic_linker_path.is_bad_path(env) {
                            skip_argument = true;
                            if !is_prefixed {
                                filtered_arguments.pop();
                            }
                            filtered_arguments.push(format!(
                                "-dynamic-linker={}",
                                env_dynamic_linker_path.display()
                            ));
                        }
                    } else {
                        if dynamic_linker_path.is_bad_path(env) {
                            skip_argument = true;
                            skip_prev_argument = !is_prefixed;
                        }
                    }
                }

                LDArgument::Plugin(path, is_prefixed) => {
                    if path.is_bad_path(env) {
                        skip_argument = true;
                        skip_prev_argument = !is_prefixed;
                        bad_plugin = true
                    } else {
                        bad_plugin = false
                    }
                }
                LDArgument::PluginOpt(_, is_prefixed) => {
                    if bad_plugin {
                        skip_argument = true;
                        skip_prev_argument = !is_prefixed;
                    }
                }
                LDArgument::LinkStatic => {
                    current_linker_state.is_static = true;
                }
                LDArgument::LinkDynamic => {
                    current_linker_state.is_static = false;
                }
                LDArgument::LinkAsNeeded => {
                    current_linker_state.is_as_needed = true;
                }
                LDArgument::NoLinkAsNeeded => {
                    current_linker_state.is_as_needed = false;
                }
                LDArgument::CopyDTNeededEntries => {
                    current_linker_state.is_copy_dt_needed_entries = true;
                }
                LDArgument::NoCopyDTNeededEntries => {
                    current_linker_state.is_copy_dt_needed_entries = false;
                }
                LDArgument::PushState => {
                    linker_state_stack.push(current_linker_state.clone());
                    current_linker_state = LinkerState::default();
                }
                LDArgument::PopState => {
                    if let Some(state) = linker_state_stack.pop() {
                        current_linker_state = state;
                    }
                }
            }
        }
        if skip_prev_argument {
            filtered_arguments.pop();
        }
        if !skip_argument {
            filtered_arguments.push(argument.clone());
        }
        previous_argument = Some(argument);
    }
    // Search for libraries in library search paths
    for library_search_path in library_search_paths.iter() {
        for (library_reference, library_state) in library_references.iter_mut() {
            match library_reference {
                LibraryReference::Name(library_name) => {
                    if library_state.linker_state.is_static {
                        let library_file_path =
                            library_search_path.join(format!("lib{}.a", library_name));
                        if library_file_path.is_file() {
                            library_state
                                .results
                                .push(LibraryLinkResult::StaticLibraryLinked(library_file_path));
                        }
                    } else {
                        let library_file_path =
                            library_search_path.join(format!("lib{}.so", library_name));
                        if library_file_path.is_file() {
                            if library_state.linker_state.is_as_needed {
                                library_state.results.push(
                                    LibraryLinkResult::SharedLibraryLinkAsNeeded(
                                        library_search_path.to_path_buf(),
                                    ),
                                );
                            } else {
                                library_state
                                    .results
                                    .push(LibraryLinkResult::SharedLibraryLinked(
                                        library_search_path.to_path_buf(),
                                    ));
                            }
                        } else {
                            let library_file_path =
                                library_search_path.join(format!("lib{}.a", library_name));
                            if library_file_path.is_file() {
                                library_state
                                    .results
                                    .push(LibraryLinkResult::StaticLibraryLinked(
                                        library_file_path,
                                    ));
                            }
                        }
                    }
                }
                LibraryReference::FileName(library_file_name) => {
                    let library_file_path = library_search_path.join(library_file_name);
                    if library_file_path.is_shared_library() {
                        if library_file_path.is_file() {
                            if library_state.linker_state.is_as_needed {
                                library_state.results.push(
                                    LibraryLinkResult::SharedLibraryLinkAsNeeded(
                                        library_search_path.to_path_buf(),
                                    ),
                                );
                            } else {
                                library_state
                                    .results
                                    .push(LibraryLinkResult::SharedLibraryLinked(
                                        library_search_path.to_path_buf(),
                                    ));
                            }
                        }
                    } else if library_file_path.is_static_library() {
                        if library_file_path.is_file() {
                            library_state
                                .results
                                .push(LibraryLinkResult::StaticLibraryLinked(library_file_path));
                        }
                    }
                }
                LibraryReference::FilePath(library_file_path) => {
                    // File's can only be found once irrespective of search path
                    if !library_state.results.is_empty() {
                        continue;
                    }
                    let library_search_path: PathBuf = library_file_path
                        .absolute_path(env)
                        .parent()
                        .unwrap()
                        .to_path_buf();
                    if library_file_path.is_shared_library() {
                        if library_file_path.is_file() {
                            if library_state.linker_state.is_as_needed {
                                library_state.results.push(
                                    LibraryLinkResult::SharedLibraryLinkAsNeeded(
                                        library_search_path.to_path_buf(),
                                    ),
                                );
                            } else {
                                library_state
                                    .results
                                    .push(LibraryLinkResult::SharedLibraryLinked(
                                        library_search_path.to_path_buf(),
                                    ));
                            }
                        }
                    } else if library_file_path.is_static_library() {
                        if library_file_path.is_file() {
                            library_state
                                .results
                                .push(LibraryLinkResult::StaticLibraryLinked(
                                    library_file_path.to_path_buf(),
                                ));
                        }
                    }
                }
            }
        }
    }
    let mut all_needed_libraries_will_link = true;
    for (library_reference, library_state) in library_references.iter() {
        let mut library_will_link = false;
        let mut library_is_needed = true;
        let mut library_is_available_at_runtime = false;
        if let Some(library_name) = library_state.library_name.as_ref() {
            if libraries_linked.contains_key(library_name) {
                continue;
            }
            let mut final_link_result = None;
            for result in library_state.results.iter() {
                match result {
                    LibraryLinkResult::StaticLibraryLinked(_) => {
                        library_will_link = true;
                    }
                    LibraryLinkResult::SharedLibraryLinked(library_search_path) => {
                        if library_search_path.is_pkg_path(env) {
                            if env.ld_run_path.contains(&library_search_path) {
                                library_is_available_at_runtime = true;
                            }
                            library_will_link = true;
                        }
                    }
                    LibraryLinkResult::SharedLibraryLinkAsNeeded(library_search_path) => {
                        if library_search_path.is_pkg_path(env) {
                            if env.ld_run_path.contains(&library_search_path) {
                                library_is_available_at_runtime = true;
                                library_will_link = true;
                            } else {
                                library_is_needed = false;
                            }
                        }
                    }
                }
                if library_will_link && library_is_needed {
                    if final_link_result.is_none() {
                        final_link_result = Some(result.clone());
                    } else if library_is_available_at_runtime {
                        final_link_result = Some(result.clone());
                    }
                    if library_is_available_at_runtime {
                        break;
                    }
                }
            }
            if !library_will_link && library_is_needed {
                all_needed_libraries_will_link = false;
            }
            if let Some(result) = final_link_result {
                match result {
                    LibraryLinkResult::StaticLibraryLinked(_) => {}
                    LibraryLinkResult::SharedLibraryLinked(library_search_path)
                    | LibraryLinkResult::SharedLibraryLinkAsNeeded(library_search_path) => {
                        if !rpaths.contains(&library_search_path)
                            && !additional_rpaths.contains(&library_search_path)
                        {
                            additional_rpaths.push(library_search_path.clone())
                        }
                    }
                }
            }
            libraries_linked.insert(library_name, (library_reference, final_link_result));
        }
    }

    if !all_needed_libraries_will_link {
        if let Some(prefix) = env.prefix.as_ref() {
            for run_path in env.ld_run_path.iter() {
                if run_path.starts_with(prefix) {
                    if !rpaths.contains(&run_path) && !additional_rpaths.contains(&run_path) {
                        additional_rpaths.push(run_path.clone())
                    }
                }
            }
        }
    }

    for additional_rpath in additional_rpaths.iter() {
        filtered_arguments.push(format!("-rpath={}", additional_rpath.display()));
    }

    if env.common.is_debug {
        if let Some(debug_log_file) = env.common.debug_log_file.as_ref() {
            let mut file = std::fs::OpenOptions::new()
                .write(true)
                .append(true)
                .create(true)
                .open(debug_log_file)
                .expect("Failed to open debug output log file");
            write!(&mut file, "prefix: {:?}\n", env.prefix).unwrap();
            write!(&mut file, "ld_run_path: {:#?}\n", env.ld_run_path).unwrap();
            write!(
                &mut file,
                "library_search_paths: {:#?}\n",
                library_search_paths
            )
            .unwrap();
            write!(&mut file, "library_references: {:#?}\n", library_references).unwrap();
            write!(&mut file, "rpaths: {:#?}\n", rpaths).unwrap();
            write!(&mut file, "additional_rpaths: {:#?}\n", additional_rpaths).unwrap();
            write!(&mut file, "libraries_linked: {:#?}\n", libraries_linked).unwrap();
            write!(
                &mut file,
                "all_needed_libraries_will_link: {}\n",
                all_needed_libraries_will_link
            )
            .unwrap();
            write!(
                &mut file,
                "filtered_ld_arguments: {:#?}\n",
                filtered_arguments
            )
            .unwrap();
        } else {
            let mut file = std::io::stderr().lock();
            write!(&mut file, "prefix: {:?}\n", env.prefix).unwrap();
            write!(&mut file, "ld_run_path: {:#?}\n", env.ld_run_path).unwrap();
            write!(
                &mut file,
                "library_search_paths: {:#?}\n",
                library_search_paths
            )
            .unwrap();
            write!(&mut file, "library_references: {:#?}\n", library_references).unwrap();
            write!(&mut file, "rpaths: {:#?}\n", rpaths).unwrap();
            write!(&mut file, "additional_rpaths: {:#?}\n", additional_rpaths).unwrap();
            write!(&mut file, "libraries_linked: {:#?}\n", libraries_linked).unwrap();
            write!(
                &mut file,
                "all_needed_libraries_will_link: {}\n",
                all_needed_libraries_will_link
            )
            .unwrap();
            write!(
                &mut file,
                "filtered_ld_arguments: {:#?}\n",
                filtered_arguments
            )
            .unwrap();
        };
    }
    filtered_arguments
}

fn main() {
    let env = LDEnvironment::default();
    let mut args = std::env::args();
    let executable = args.next().unwrap();
    let program = args.next().expect("Wrapped program must be specified");

    let parsed_arguments = parse_linker_arguments(
        args.flat_map(|argument| {
            if argument.is_prefixed_with("@") {
                opts_parser::expand_argument(&argument, &env.common)
            } else {
                vec![argument]
            }
        })
        .into_iter(),
        &env,
    );

    // Unset the LD_RUN_PATH so it doesn't affect the wrapped linker
    std::env::remove_var("LD_RUN_PATH");

    let mut command = Command::new(&program);
    command.args(&parsed_arguments);

    if env.common.is_debug {
        if let Some(debug_log_file) = env.common.debug_log_file.as_ref() {
            let mut file = std::fs::OpenOptions::new()
                .write(true)
                .append(true)
                .create(true)
                .open(debug_log_file)
                .expect("Failed to open debug output log file");
            write!(
                &mut file,
                "work_dir: {}\n",
                std::env::current_dir().unwrap().display()
            )
            .unwrap();
            write!(
                &mut file,
                "original: {}\n",
                std::env::args().skip(1).collect::<Vec<String>>().join(" ")
            )
            .unwrap();
            write!(
                &mut file,
                "wrapped: {} {}\n",
                program,
                parsed_arguments.join(" ")
            )
            .unwrap();
        } else {
            let mut file = std::io::stderr().lock();
            write!(
                &mut file,
                "work_dir: {}\n",
                std::env::current_dir().unwrap().display()
            )
            .unwrap();
            write!(
                &mut file,
                "original: {}\n",
                std::env::args().skip(1).collect::<Vec<String>>().join(" ")
            )
            .unwrap();
            write!(
                &mut file,
                "wrapped: {} {}\n",
                program,
                parsed_arguments.join(" ")
            )
            .unwrap();
        };
    }

    // Replace the current process with the new program
    let error = command.exec();

    // If we reach this point, there was an error
    eprintln!("{}: {}", executable, error);
    std::process::exit(126);
}

#[cfg(test)]
mod tests {
    use hab_pkg_wrappers::env::CommonEnvironment;
    use std::fs::File;
    use std::path::Path;
    use tempdir::TempDir;

    use crate::{parse_linker_arguments, LDEnvironment};

    fn touch(path: impl AsRef<Path>) {
        if let Some(parent_dir) = path.as_ref().parent() {
            std::fs::create_dir_all(parent_dir).unwrap();
        }
        File::create(path).unwrap();
    }

    // This is the normal scenario when linking in libraries dynamically from other packages
    // An rpath entry to the library search path is added to ensure the library is found at runtime
    #[test]
    fn basic_dynamic_linking() {
        let temp_dir = TempDir::new("ld-wrapper-test").unwrap();
        let libc_search_path = temp_dir
            .path()
            .join("hab")
            .join("pkgs")
            .join("core")
            .join("glibc")
            .join("version")
            .join("release")
            .join("lib");
        let libc_shared = libc_search_path.join("libc.so");
        touch(libc_shared);

        let raw_link_arguments = format!("-lc -L {}", libc_search_path.display());
        let link_arguments = raw_link_arguments
            .split(" ")
            .map(|x| x.to_string())
            .collect::<Vec<String>>();
        let env = LDEnvironment {
            common: CommonEnvironment {
                fs_root: temp_dir.path().to_path_buf(),
                ..Default::default()
            },
            ..Default::default()
        };
        let result = parse_linker_arguments(link_arguments.into_iter(), &env);
        assert_eq!(
            result.join(" "),
            format!(
                "{} -rpath={}",
                raw_link_arguments,
                libc_search_path.display()
            )
        );
    }

    // This is the scenario when linking in libraries statically from other packages
    // No rpath entry is added since the library is statically linked
    #[test]
    fn basic_static_linking() {
        let temp_dir = TempDir::new("ld-wrapper-test").unwrap();
        let libc_search_path = temp_dir
            .path()
            .join("hab")
            .join("pkgs")
            .join("core")
            .join("glibc")
            .join("version")
            .join("release")
            .join("lib");
        let libc_static = libc_search_path.join("libc.a");
        touch(libc_static);

        let raw_link_arguments = format!("-lc -L {}", libc_search_path.display());
        let link_arguments = raw_link_arguments
            .split(" ")
            .map(|x| x.to_string())
            .collect::<Vec<String>>();
        let env = LDEnvironment {
            common: CommonEnvironment {
                fs_root: temp_dir.path().to_path_buf(),
                ..Default::default()
            },
            ..Default::default()
        };
        let result = parse_linker_arguments(link_arguments.into_iter(), &env);
        assert_eq!(result.join(" "), raw_link_arguments);
    }

    // This is the scenario when linking in libraries dynamically from other packages
    // which also have a static library
    // An rpath entry to the library search path is added to ensure the library is found at runtime
    #[test]
    fn shared_lib_preferred_over_static_lib() {
        let temp_dir = TempDir::new("ld-wrapper-test").unwrap();
        let libc_search_path = temp_dir
            .path()
            .join("hab")
            .join("pkgs")
            .join("core")
            .join("glibc")
            .join("version")
            .join("release")
            .join("lib");
        let libc_static = libc_search_path.join("libc.a");
        touch(libc_static);
        let libc_shared = libc_search_path.join("libc.so");
        touch(libc_shared);

        let raw_link_arguments = format!("-lc -L {}", libc_search_path.display());
        let link_arguments = raw_link_arguments
            .split(" ")
            .map(|x| x.to_string())
            .collect::<Vec<String>>();
        let env = LDEnvironment {
            common: CommonEnvironment {
                fs_root: temp_dir.path().to_path_buf(),
                ..Default::default()
            },
            ..Default::default()
        };
        let result = parse_linker_arguments(link_arguments.into_iter(), &env);
        assert_eq!(
            result.join(" "),
            format!(
                "{} -rpath={}",
                raw_link_arguments,
                libc_search_path.display()
            )
        );
    }

    // This is the scenario when linking in libraries statically from other packages
    // which also have a dynamic library present
    // No rpath entry is added since the library is statically linked
    #[test]
    fn forced_static_lib_linking() {
        let temp_dir = TempDir::new("ld-wrapper-test").unwrap();
        let libc_search_path = temp_dir
            .path()
            .join("hab")
            .join("pkgs")
            .join("core")
            .join("glibc")
            .join("version")
            .join("release")
            .join("lib");
        let libc_static = libc_search_path.join("libc.a");
        touch(libc_static);
        let libc_shared = libc_search_path.join("libc.so");
        touch(libc_shared);

        let raw_link_arguments = format!("-Bstatic -lc -L {}", libc_search_path.display());
        let link_arguments = raw_link_arguments
            .split(" ")
            .map(|x| x.to_string())
            .collect::<Vec<String>>();
        let env = LDEnvironment {
            common: CommonEnvironment {
                fs_root: temp_dir.path().to_path_buf(),
                ..Default::default()
            },
            ..Default::default()
        };
        let result = parse_linker_arguments(link_arguments.into_iter(), &env);
        assert_eq!(result.join(" "), raw_link_arguments);
    }

    #[test]
    fn dynamic_linking_with_specified_rpath() {
        let temp_dir = TempDir::new("ld-wrapper-test").unwrap();
        let libc_search_path = temp_dir
            .path()
            .join("hab")
            .join("pkgs")
            .join("core")
            .join("glibc")
            .join("version")
            .join("release")
            .join("lib");
        let libc_shared = libc_search_path.join("libc.so");
        touch(libc_shared);

        let raw_link_arguments = format!("-lc -L {0} -rpath {0}", libc_search_path.display());
        let link_arguments = raw_link_arguments
            .split(" ")
            .map(|x| x.to_string())
            .collect::<Vec<String>>();
        let env = LDEnvironment {
            common: CommonEnvironment {
                fs_root: temp_dir.path().to_path_buf(),
                ..Default::default()
            },
            ..Default::default()
        };
        let result = parse_linker_arguments(link_arguments.into_iter(), &env);
        assert_eq!(result.join(" "), raw_link_arguments,);
    }

    #[test]
    fn dynamic_linking_with_indirect_search_path() {
        let temp_dir = TempDir::new("ld-wrapper-test").unwrap();
        let libc_search_path = temp_dir
            .path()
            .join("hab")
            .join("pkgs")
            .join("core")
            .join("glibc")
            .join("version")
            .join("release")
            .join("lib");
        let libc_shared = libc_search_path.join("libc.so");
        touch(libc_shared);

        let raw_link_arguments = format!("-lc -L {}/../lib", libc_search_path.display());
        let link_arguments = raw_link_arguments
            .split(" ")
            .map(|x| x.to_string())
            .collect::<Vec<String>>();
        let env = LDEnvironment {
            common: CommonEnvironment {
                fs_root: temp_dir.path().to_path_buf(),
                ..Default::default()
            },
            ..Default::default()
        };
        let result = parse_linker_arguments(link_arguments.into_iter(), &env);
        assert_eq!(
            result.join(" "),
            format!(
                "{} -rpath={}",
                raw_link_arguments,
                libc_search_path.display()
            )
        );
    }

    #[test]
    fn dynamic_linking_with_indirect_search_path_and_indirect_specified_rpath() {
        let temp_dir = TempDir::new("ld-wrapper-test").unwrap();
        let libc_search_path = temp_dir
            .path()
            .join("hab")
            .join("pkgs")
            .join("core")
            .join("glibc")
            .join("version")
            .join("release")
            .join("lib");
        let libc_shared = libc_search_path.join("libc.so");
        touch(libc_shared);

        let raw_link_arguments = format!(
            "-lc -L {0}/../lib -rpath {0}/../lib",
            libc_search_path.display()
        );
        let link_arguments = raw_link_arguments
            .split(" ")
            .map(|x| x.to_string())
            .collect::<Vec<String>>();
        let env = LDEnvironment {
            common: CommonEnvironment {
                fs_root: temp_dir.path().to_path_buf(),
                ..Default::default()
            },
            ..Default::default()
        };
        let result = parse_linker_arguments(link_arguments.into_iter(), &env);
        assert_eq!(result.join(" "), raw_link_arguments,);
    }

    // This is the case when a binary/library links dynamically against one of it's own package's libraries
    // An rpath entry to the install path is added to ensure the library is found at runtime
    #[test]
    fn link_against_shared_library_in_build_package() {
        let temp_dir = TempDir::new("ld-wrapper-test").unwrap();
        let install_prefix_dir = temp_dir
            .path()
            .join("hab")
            .join("pkgs")
            .join("core")
            .join("openssl")
            .join("version")
            .join("release");
        let build_dir = temp_dir
            .path()
            .join("hab")
            .join("cache")
            .join("src")
            .join("openssl");
        let libcrypto_shared = build_dir.join("libcrypto.so");
        touch(&libcrypto_shared);

        // Passed as -l flag
        let raw_link_arguments = String::from("-L. -lcrypto");
        let link_arguments = raw_link_arguments
            .split(" ")
            .map(|x| x.to_string())
            .collect::<Vec<String>>();
        let env = LDEnvironment {
            common: CommonEnvironment {
                fs_root: temp_dir.path().to_path_buf(),
                cwd: build_dir.clone(),
                ..Default::default()
            },
            ld_run_path: vec![install_prefix_dir.join("lib")],
            prefix: Some(install_prefix_dir.clone()),
            ..Default::default()
        };
        let result = parse_linker_arguments(link_arguments.into_iter(), &env);
        assert_eq!(
            result.join(" "),
            format!(
                "{} -rpath={}",
                raw_link_arguments,
                install_prefix_dir.join("lib").display()
            )
        );

        // Passed as absolute path to shared library
        let raw_link_arguments = format!("-L. {}", libcrypto_shared.display());
        let link_arguments = raw_link_arguments
            .split(" ")
            .map(|x| x.to_string())
            .collect::<Vec<String>>();
        let env = LDEnvironment {
            common: CommonEnvironment {
                fs_root: temp_dir.path().to_path_buf(),
                cwd: build_dir.clone(),
                ..Default::default()
            },
            ld_run_path: vec![install_prefix_dir.join("lib")],
            prefix: Some(install_prefix_dir.clone()),
            ..Default::default()
        };
        let result = parse_linker_arguments(link_arguments.into_iter(), &env);
        assert_eq!(
            result.join(" "),
            format!(
                "{} -rpath={}",
                raw_link_arguments,
                install_prefix_dir.join("lib").display()
            )
        );

        // Passed as exact filename to shared library
        let raw_link_arguments = String::from("-L. -l:libcrypto.so");
        let link_arguments = raw_link_arguments
            .split(" ")
            .map(|x| x.to_string())
            .collect::<Vec<String>>();
        let env = LDEnvironment {
            common: CommonEnvironment {
                fs_root: temp_dir.path().to_path_buf(),
                cwd: build_dir,
                ..Default::default()
            },
            ld_run_path: vec![install_prefix_dir.join("lib")],
            prefix: Some(install_prefix_dir.clone()),
            ..Default::default()
        };
        let result = parse_linker_arguments(link_arguments.into_iter(), &env);
        assert_eq!(
            result.join(" "),
            format!(
                "{} -rpath={}",
                raw_link_arguments,
                install_prefix_dir.join("lib").display()
            )
        );
    }

    // This is the case when a binary/library links dynamically against both the old and newly built version of a library
    // in the same package, this happens most importantly in glibc
    // An rpath entry to the install path is added to ensure the library is found at runtime
    #[test]
    fn link_against_newer_version_of_same_shared_library_in_build_package() {
        let temp_dir = TempDir::new("ld-wrapper-test").unwrap();
        let old_install_prefix_dir = temp_dir
            .path()
            .join("hab")
            .join("pkgs")
            .join("core")
            .join("glibc")
            .join("old-version")
            .join("release");
        let old_libc_shared = old_install_prefix_dir.join("libc.so");
        touch(&old_libc_shared);

        let install_prefix_dir = temp_dir
            .path()
            .join("hab")
            .join("pkgs")
            .join("core")
            .join("glibc")
            .join("version")
            .join("release");
        let build_dir = temp_dir
            .path()
            .join("hab")
            .join("cache")
            .join("src")
            .join("glibc");
        let new_libc_shared = build_dir.join("libc.so");
        touch(&new_libc_shared);

        // New library passed via build search path
        let raw_link_arguments = format!(
            "-L{} -L{} {} -lc",
            build_dir.display(),
            old_install_prefix_dir.display(),
            new_libc_shared.display()
        );
        let link_arguments = raw_link_arguments
            .split(" ")
            .map(|x| x.to_string())
            .collect::<Vec<String>>();
        let env = LDEnvironment {
            common: CommonEnvironment {
                fs_root: temp_dir.path().to_path_buf(),
                cwd: build_dir.clone(),
                ..Default::default()
            },
            ld_run_path: vec![install_prefix_dir.join("lib")],
            prefix: Some(install_prefix_dir.clone()),
            ..Default::default()
        };
        let result = parse_linker_arguments(link_arguments.into_iter(), &env);
        assert_eq!(
            result.join(" "),
            format!(
                "{} -rpath={}",
                raw_link_arguments,
                install_prefix_dir.join("lib").display()
            )
        );

        // New library passed as absolute path
        let raw_link_arguments = format!(
            "-L{} {} -lc",
            old_install_prefix_dir.display(),
            new_libc_shared.display()
        );
        let link_arguments = raw_link_arguments
            .split(" ")
            .map(|x| x.to_string())
            .collect::<Vec<String>>();
        let env = LDEnvironment {
            common: CommonEnvironment {
                fs_root: temp_dir.path().to_path_buf(),
                cwd: build_dir.clone(),
                ..Default::default()
            },
            ld_run_path: vec![install_prefix_dir.join("lib")],
            prefix: Some(install_prefix_dir.clone()),
            ..Default::default()
        };
        let result = parse_linker_arguments(link_arguments.into_iter(), &env);
        assert_eq!(
            result.join(" "),
            format!(
                "{} -rpath={}",
                raw_link_arguments,
                install_prefix_dir.join("lib").display()
            )
        );

        let new_libc_shared = build_dir.join("libc.so.new");
        touch(&new_libc_shared);

        // New library passed as exact file name
        let raw_link_arguments =
            format!("-L{} -l:libc.so.new -lc", old_install_prefix_dir.display(),);
        let link_arguments = raw_link_arguments
            .split(" ")
            .map(|x| x.to_string())
            .collect::<Vec<String>>();
        let env = LDEnvironment {
            common: CommonEnvironment {
                fs_root: temp_dir.path().to_path_buf(),
                cwd: build_dir.clone(),
                ..Default::default()
            },
            ld_run_path: vec![install_prefix_dir.join("lib")],
            prefix: Some(install_prefix_dir.clone()),
            ..Default::default()
        };
        let result = parse_linker_arguments(link_arguments.into_iter(), &env);
        assert_eq!(
            result.join(" "),
            format!(
                "{} -rpath={}",
                raw_link_arguments,
                install_prefix_dir.join("lib").display()
            )
        );
    }

    // This is the case when a binary/library links dynamically against one of it's own package's libraries
    // No rpath entry is added since the library is statically linked
    #[test]
    fn link_against_static_library_in_build_package() {
        let temp_dir = TempDir::new("ld-wrapper-test").unwrap();
        let install_prefix_dir = temp_dir
            .path()
            .join("hab")
            .join("pkgs")
            .join("core")
            .join("openssl")
            .join("version")
            .join("release");
        let build_dir = temp_dir
            .path()
            .join("hab")
            .join("cache")
            .join("src")
            .join("openssl");
        let libcrypto_static = build_dir.join("libcrypto.a");
        touch(libcrypto_static);

        let raw_link_arguments = String::from("-L. -lcrypto");
        let link_arguments = raw_link_arguments
            .split(" ")
            .map(|x| x.to_string())
            .collect::<Vec<String>>();
        let env = LDEnvironment {
            common: CommonEnvironment {
                fs_root: temp_dir.path().to_path_buf(),
                cwd: build_dir,
                ..Default::default()
            },
            prefix: Some(install_prefix_dir.clone()),
            ..Default::default()
        };
        let result = parse_linker_arguments(link_arguments.into_iter(), &env);
        assert_eq!(result.join(" "), raw_link_arguments);
    }

    #[test]
    fn link_against_same_library_in_build_and_runtime_dep() {
        let temp_dir = TempDir::new("ld-wrapper-test").unwrap();
        let install_prefix_dir = temp_dir
            .path()
            .join("hab")
            .join("pkgs")
            .join("core")
            .join("openssl")
            .join("version")
            .join("release");
        let install_lib_dir = install_prefix_dir.join("lib");
        let libstdcxx_search_path = temp_dir
            .path()
            .join("hab")
            .join("pkgs")
            .join("core")
            .join("gcc")
            .join("version")
            .join("release")
            .join("lib");
        let libstdcxx_shared = libstdcxx_search_path.join("libstdc++.so");
        touch(&libstdcxx_shared);
        let libstdcxx_libs_search_path = temp_dir
            .path()
            .join("hab")
            .join("pkgs")
            .join("core")
            .join("gcc-libs")
            .join("version")
            .join("release")
            .join("lib");
        let libstdcxx_libs_shared = libstdcxx_libs_search_path.join("libstdc++.so");
        touch(&libstdcxx_libs_shared);

        // This is the case where gcc and gcc-libs are only a build dep
        let raw_link_arguments = format!(
            "-L{} -L{} -lstdc++",
            libstdcxx_search_path.display(),
            libstdcxx_libs_search_path.display()
        );
        let link_arguments = raw_link_arguments
            .split(" ")
            .map(|x| x.to_string())
            .collect::<Vec<String>>();
        let env = LDEnvironment {
            common: CommonEnvironment {
                fs_root: temp_dir.path().to_path_buf(),
                ..Default::default()
            },
            prefix: Some(install_prefix_dir.clone()),
            ..Default::default()
        };
        let result = parse_linker_arguments(link_arguments.into_iter(), &env);
        assert_eq!(
            result.join(" "),
            format!(
                "{} -rpath={}",
                raw_link_arguments,
                libstdcxx_search_path.display()
            )
        );

        // This is the case where gcc is a build dep gcc-libs is a runtime dep
        let raw_link_arguments = format!(
            "-L{} -L{} -lstdc++",
            libstdcxx_search_path.display(),
            libstdcxx_libs_search_path.display()
        );
        let link_arguments = raw_link_arguments
            .split(" ")
            .map(|x| x.to_string())
            .collect::<Vec<String>>();
        let env = LDEnvironment {
            common: CommonEnvironment {
                fs_root: temp_dir.path().to_path_buf(),
                ..Default::default()
            },
            ld_run_path: vec![install_lib_dir.clone(), libstdcxx_libs_search_path.clone()],
            prefix: Some(install_prefix_dir.clone()),
            ..Default::default()
        };
        let result = parse_linker_arguments(link_arguments.into_iter(), &env);
        assert_eq!(
            result.join(" "),
            format!(
                "{} -rpath={}",
                raw_link_arguments,
                libstdcxx_libs_search_path.display()
            )
        );
    }

    #[test]
    fn link_as_needed_without_ld_run_path_hint() {
        let temp_dir = TempDir::new("ld-wrapper-test").unwrap();
        let install_prefix_dir = temp_dir
            .path()
            .join("hab")
            .join("pkgs")
            .join("core")
            .join("openssl")
            .join("version")
            .join("release");
        let install_lib_dir = install_prefix_dir.join("lib");
        let libc_search_path = temp_dir
            .path()
            .join("hab")
            .join("pkgs")
            .join("core")
            .join("glibc")
            .join("version")
            .join("release")
            .join("lib");
        let libc_static = libc_search_path.join("libc.a");
        touch(libc_static);
        let libc_shared = libc_search_path.join("libc.so");
        touch(libc_shared);

        let raw_link_arguments = format!("--as-needed -lc -L {}", libc_search_path.display());
        let link_arguments = raw_link_arguments
            .split(" ")
            .map(|x| x.to_string())
            .collect::<Vec<String>>();
        let env = LDEnvironment {
            common: CommonEnvironment {
                fs_root: temp_dir.path().to_path_buf(),
                ..Default::default()
            },
            ld_run_path: vec![install_lib_dir],
            prefix: Some(install_prefix_dir.clone()),
            ..Default::default()
        };
        let result = parse_linker_arguments(link_arguments.into_iter(), &env);
        assert_eq!(result.join(" "), raw_link_arguments);
    }

    #[test]
    fn link_as_needed_with_ld_run_path_hint() {
        let temp_dir = TempDir::new("ld-wrapper-test").unwrap();
        let install_prefix_dir = temp_dir
            .path()
            .join("hab")
            .join("pkgs")
            .join("core")
            .join("openssl")
            .join("version")
            .join("release");
        let install_lib_dir = install_prefix_dir.join("lib");
        let libc_search_path = temp_dir
            .path()
            .join("hab")
            .join("pkgs")
            .join("core")
            .join("glibc")
            .join("version")
            .join("release")
            .join("lib");
        let libc_static = libc_search_path.join("libc.a");
        touch(libc_static);
        let libc_shared = libc_search_path.join("libc.so");
        touch(libc_shared);

        let raw_link_arguments = format!("--as-needed -lc -L {}", libc_search_path.display());
        let link_arguments = raw_link_arguments
            .split(" ")
            .map(|x| x.to_string())
            .collect::<Vec<String>>();
        let env = LDEnvironment {
            common: CommonEnvironment {
                fs_root: temp_dir.path().to_path_buf(),
                ..Default::default()
            },
            prefix: Some(install_prefix_dir.clone()),
            ld_run_path: vec![install_lib_dir.clone(), libc_search_path.clone()],
            ..Default::default()
        };
        let result = parse_linker_arguments(link_arguments.into_iter(), &env);
        assert_eq!(
            result.join(" "),
            format!(
                "{} -rpath={}",
                raw_link_arguments,
                libc_search_path.display()
            )
        );
    }

    #[test]
    fn push_state_as_needed_linking() {
        let temp_dir = TempDir::new("ld-wrapper-test").unwrap();
        let install_prefix_dir = temp_dir
            .path()
            .join("hab")
            .join("pkgs")
            .join("core")
            .join("openssl")
            .join("version")
            .join("release");
        let install_lib_dir = install_prefix_dir.join("lib");
        let libc_search_path = temp_dir
            .path()
            .join("hab")
            .join("pkgs")
            .join("core")
            .join("glibc")
            .join("version")
            .join("release")
            .join("lib");
        let libc_shared = libc_search_path.join("libc.so");
        touch(&libc_shared);
        let libz_search_path = temp_dir
            .path()
            .join("hab")
            .join("pkgs")
            .join("core")
            .join("zlib")
            .join("version")
            .join("release")
            .join("lib");
        let libz_shared = libz_search_path.join("libz.so");
        touch(&libz_shared);

        // without ld run path hint
        let raw_link_arguments = format!(
            "-L {} -L {} --push-state --as-needed -lc --pop-state -lz",
            libc_search_path.display(),
            libz_search_path.display()
        );
        let link_arguments = raw_link_arguments
            .split(" ")
            .map(|x| x.to_string())
            .collect::<Vec<String>>();
        let env = LDEnvironment {
            common: CommonEnvironment {
                fs_root: temp_dir.path().to_path_buf(),
                ..Default::default()
            },
            prefix: Some(install_prefix_dir.clone()),
            ld_run_path: vec![install_lib_dir.clone()],
            ..Default::default()
        };
        let result = parse_linker_arguments(link_arguments.into_iter(), &env);
        assert_eq!(
            result.join(" "),
            format!(
                "{} -rpath={}",
                raw_link_arguments,
                libz_search_path.display()
            )
        );

        // with ld run path hint
        let raw_link_arguments = format!(
            "-L {} -L {} --push-state --as-needed -lc --pop-state -lz",
            libc_search_path.display(),
            libz_search_path.display()
        );
        let link_arguments = raw_link_arguments
            .split(" ")
            .map(|x| x.to_string())
            .collect::<Vec<String>>();
        let env = LDEnvironment {
            common: CommonEnvironment {
                fs_root: temp_dir.path().to_path_buf(),
                ..Default::default()
            },
            ld_run_path: vec![libc_search_path.clone()],
            ..Default::default()
        };
        let result = parse_linker_arguments(link_arguments.into_iter(), &env);
        assert_eq!(
            result.join(" "),
            format!(
                "{} -rpath={} -rpath={}",
                raw_link_arguments,
                libc_search_path.display(),
                libz_search_path.display()
            )
        );
    }

    #[test]
    fn push_state_static_and_dynamic_linking() {
        let temp_dir = TempDir::new("ld-wrapper-test").unwrap();
        let libc_search_path = temp_dir
            .path()
            .join("hab")
            .join("pkgs")
            .join("core")
            .join("glibc")
            .join("version")
            .join("release")
            .join("lib");
        let libc_shared = libc_search_path.join("libc.so");
        touch(&libc_shared);
        let libz_search_path = temp_dir
            .path()
            .join("hab")
            .join("pkgs")
            .join("core")
            .join("zlib")
            .join("version")
            .join("release")
            .join("lib");
        let libz_shared = libz_search_path.join("libz.so");
        let libz_static = libz_search_path.join("libz.a");
        touch(&libz_shared);
        touch(&libz_static);

        // without ld run path hint
        let raw_link_arguments = format!(
            "-L {} -L {} --push-state -Bstatic -lz --pop-state -lc",
            libc_search_path.display(),
            libz_search_path.display()
        );
        let link_arguments = raw_link_arguments
            .split(" ")
            .map(|x| x.to_string())
            .collect::<Vec<String>>();
        let env = LDEnvironment {
            common: CommonEnvironment {
                fs_root: temp_dir.path().to_path_buf(),
                ..Default::default()
            },
            ..Default::default()
        };
        let result = parse_linker_arguments(link_arguments.into_iter(), &env);
        assert_eq!(
            result.join(" "),
            format!(
                "{} -rpath={}",
                raw_link_arguments,
                libc_search_path.display()
            )
        );
    }

    #[test]
    fn impure_library_search_path_filtering() {
        let temp_dir = TempDir::new("ld-wrapper-test").unwrap();
        let libc_search_path = temp_dir.path().join("usr").join("lib");
        let libc_shared = libc_search_path.join("libc.so");
        touch(libc_shared);

        let raw_link_arguments = format!("-lc -L {0} -L{0} -L{0}/../../usr/lib -L {0}/../../usr/lib -L{0}/../../hab/pkgs/core/glibc/version/release/lib -lm", libc_search_path.display());
        let link_arguments = raw_link_arguments
            .split(" ")
            .map(|x| x.to_string())
            .collect::<Vec<String>>();
        let env = LDEnvironment {
            common: CommonEnvironment {
                fs_root: temp_dir.path().to_path_buf(),
                ..Default::default()
            },
            ..Default::default()
        };
        let result = parse_linker_arguments(link_arguments.into_iter(), &env);
        assert_eq!(
            result.join(" "),
            format!(
                "-lc -L{0}/../../hab/pkgs/core/glibc/version/release/lib -lm",
                libc_search_path.display()
            )
        );
    }

    #[test]
    fn impure_absolute_library_path_filtering() {
        let temp_dir = TempDir::new("ld-wrapper-test").unwrap();
        let impure_libz_search_path = temp_dir.path().join("usr").join("lib");
        let impure_libz_shared = impure_libz_search_path.join("libz.so");
        touch(&impure_libz_shared);

        let libc_search_path = temp_dir
            .path()
            .join("hab")
            .join("pkgs")
            .join("core")
            .join("glibc")
            .join("version")
            .join("release")
            .join("lib");
        let libc_shared = libc_search_path.join("libc.so");
        touch(&libc_shared);

        let raw_link_arguments = format!(
            "-L {} -lc {} {}",
            libc_search_path.display(),
            impure_libz_shared.display(),
            libc_shared.display()
        );
        let link_arguments = raw_link_arguments
            .split(" ")
            .map(|x| x.to_string())
            .collect::<Vec<String>>();
        let env = LDEnvironment {
            common: CommonEnvironment {
                fs_root: temp_dir.path().to_path_buf(),
                ..Default::default()
            },
            ..Default::default()
        };
        let result = parse_linker_arguments(link_arguments.into_iter(), &env);
        assert_eq!(
            result.join(" "),
            format!(
                "-L {} -lc {} -rpath={0}",
                libc_search_path.display(),
                libc_shared.display()
            )
        );
    }

    #[test]
    fn impure_plugin_filtering() {
        let temp_dir = TempDir::new("ld-wrapper-test").unwrap();
        let impure_liblto_plugin = temp_dir
            .path()
            .join("usr")
            .join("lib")
            .join("gcc")
            .join("aarch64-linux-gnu")
            .join("9")
            .join("liblto_plugin.so");
        touch(&impure_liblto_plugin);
        let liblto_plugin = temp_dir
            .path()
            .join("hab")
            .join("pkgs")
            .join("core")
            .join("binutils")
            .join("version")
            .join("release")
            .join("lib")
            .join("liblto_plugin.so");
        touch(&liblto_plugin);

        let raw_link_arguments = format!(
            "-plugin {} -plugin-opt=option1 -plugin-opt=option2 -plugin={} -plugin-opt=option3 -lz -plugin {0} -plugin-opt=option4 -plugin-opt=option5",
            impure_liblto_plugin.display(),
            liblto_plugin.display()
        );
        let link_arguments = raw_link_arguments
            .split(" ")
            .map(|x| x.to_string())
            .collect::<Vec<String>>();
        let env = LDEnvironment {
            common: CommonEnvironment {
                fs_root: temp_dir.path().to_path_buf(),
                ..Default::default()
            },
            ..Default::default()
        };
        let result = parse_linker_arguments(link_arguments.into_iter(), &env);
        assert_eq!(
            result.join(" "),
            format!(
                "-plugin={} -plugin-opt=option3 -lz",
                liblto_plugin.display()
            )
        );
    }

    #[test]
    fn impure_dynamic_linker_replacement() {
        let temp_dir = TempDir::new("ld-wrapper-test").unwrap();
        let libc_search_path = temp_dir
            .path()
            .join("hab")
            .join("pkgs")
            .join("core")
            .join("glibc")
            .join("version")
            .join("release")
            .join("lib");
        let dynamic_linker = libc_search_path.join("ld-linux-aarch64.so.1");
        let dynamic_linker_alternative = libc_search_path.join("ld-linux-musl.so");
        touch(&dynamic_linker);
        touch(&dynamic_linker_alternative);
        let impure_dynamic_linker = temp_dir
            .path()
            .join("usr")
            .join("lib")
            .join("ld-linux-aarch64.so.1");
        touch(&impure_dynamic_linker);

        let raw_link_arguments = format!("-dynamic-linker {}", impure_dynamic_linker.display());
        let link_arguments = raw_link_arguments
            .split(" ")
            .map(|x| x.to_string())
            .collect::<Vec<String>>();
        let env = LDEnvironment {
            common: CommonEnvironment {
                fs_root: temp_dir.path().to_path_buf(),
                ..Default::default()
            },
            dynamic_linker: Some(dynamic_linker.clone()),
            ..Default::default()
        };
        let result = parse_linker_arguments(link_arguments.into_iter(), &env);
        assert_eq!(
            result.join(" "),
            format!("-dynamic-linker={}", dynamic_linker.display())
        );

        // Impure dynamic linker without env is ignored
        let raw_link_arguments = format!("-dynamic-linker {}", impure_dynamic_linker.display());
        let link_arguments = raw_link_arguments
            .split(" ")
            .map(|x| x.to_string())
            .collect::<Vec<String>>();
        let env = LDEnvironment {
            common: CommonEnvironment {
                fs_root: temp_dir.path().to_path_buf(),
                ..Default::default()
            },
            ..Default::default()
        };
        let result = parse_linker_arguments(link_arguments.into_iter(), &env);
        assert_eq!(result.join(" "), String::from(""));

        // Pure dynamic linker is not replaced by specified linker
        let raw_link_arguments = format!("-dynamic-linker {}", dynamic_linker.display());
        let link_arguments = raw_link_arguments
            .split(" ")
            .map(|x| x.to_string())
            .collect::<Vec<String>>();
        let env = LDEnvironment {
            common: CommonEnvironment {
                fs_root: temp_dir.path().to_path_buf(),
                ..Default::default()
            },
            dynamic_linker: Some(dynamic_linker_alternative.clone()),
            ..Default::default()
        };
        let result = parse_linker_arguments(link_arguments.into_iter(), &env);
        assert_eq!(result.join(" "), raw_link_arguments);

        // Pure dynamic linker is not replaced is alternative is not specified
        let raw_link_arguments = format!("-dynamic-linker {}", dynamic_linker.display());
        let link_arguments = raw_link_arguments
            .split(" ")
            .map(|x| x.to_string())
            .collect::<Vec<String>>();
        let env = LDEnvironment {
            common: CommonEnvironment {
                fs_root: temp_dir.path().to_path_buf(),
                ..Default::default()
            },
            ..Default::default()
        };
        let result = parse_linker_arguments(link_arguments.into_iter(), &env);
        assert_eq!(result.join(" "), raw_link_arguments);
    }

    #[test]
    fn ignores_output_shared_library_names() {
        let temp_dir = TempDir::new("ld-wrapper-test").unwrap();
        let libc_search_path = temp_dir
            .path()
            .join("hab")
            .join("pkgs")
            .join("core")
            .join("glibc")
            .join("version")
            .join("release")
            .join("lib");
        let libc_shared = libc_search_path.join("libc.so");
        touch(libc_shared);

        let raw_link_arguments = format!("-L {} -o libc.so -h libc.so", libc_search_path.display());
        let link_arguments = raw_link_arguments
            .split(" ")
            .map(|x| x.to_string())
            .collect::<Vec<String>>();
        let env = LDEnvironment {
            common: CommonEnvironment {
                fs_root: temp_dir.path().to_path_buf(),
                ..Default::default()
            },
            ..Default::default()
        };
        let result = parse_linker_arguments(link_arguments.into_iter(), &env);
        assert_eq!(result.join(" "), raw_link_arguments);
    }
}

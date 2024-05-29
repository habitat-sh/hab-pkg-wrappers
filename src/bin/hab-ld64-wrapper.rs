use std::{
    fmt::Display,
    hash::Hash,
    io::Write,
    path::{Path, PathBuf},
};

use exec::Command;
use hab_pkg_wrappers::{env::CommonEnvironment, opts_parser, util::PrefixedArg};
use path_absolutize::Absolutize;

#[derive(Default, Debug, PartialEq)]
pub enum LinkMode {
    /// Ensures every path in the LD_RUN_PATH environment variable is added to the rpath of binaries
    #[default]
    Complete,
    /// Adds paths in the LD_RUN_PATH environment variable to the rpath of binaries only
    /// if the path actually contains a linked library
    Minimal,
}

impl Display for LinkMode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LinkMode::Complete => write!(f, "complete"),
            LinkMode::Minimal => write!(f, "minimal"),
        }
    }
}

impl LinkMode {
    pub fn maybe_parse(value: impl AsRef<str>) -> Option<LinkMode> {
        match value.as_ref() {
            "complete" => Some(LinkMode::Complete),
            "minimal" => Some(LinkMode::Minimal),
            _ => None,
        }
    }
}

struct LDEnvironment {
    pub common: CommonEnvironment,
    pub allowed_impure_paths: Vec<PathBuf>,
    pub ld_link_mode: LinkMode,
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
            allowed_impure_paths: std::env::var("HAB_ALLOWED_IMPURE_PATHS")
                .map(|value| {
                    value
                        .split(':')
                        .map(PathBuf::from)
                        .collect::<Vec<PathBuf>>()
                })
                .unwrap_or_default(),
            ld_link_mode: LinkMode::maybe_parse(
                std::env::var("HAB_LD_LINK_MODE").unwrap_or_default(),
            )
            .unwrap_or_default(),
            ld_run_path: std::env::var("HAB_LD_RUN_PATH")
                .map(|value| {
                    value
                        .split(':')
                        .filter(|p| !p.is_empty())
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
    fn is_impure_path(&self, env: &LDEnvironment) -> bool;
    fn install_name(&self, env: &LDEnvironment) -> PathBuf;
    fn is_allowed_impure_path(&self, env: &LDEnvironment) -> bool;
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
    fn is_impure_path(&self, env: &LDEnvironment) -> bool {
        if !env.enforce_purity {
            return false;
        }
        let path = self
            .as_ref()
            .absolutize_from(&env.common.cwd)
            .unwrap()
            .to_path_buf();
        for impure_path in env.allowed_impure_paths.iter() {
            if path.starts_with(env.common.fs_root.join(impure_path)) {
                return false;
            }
        }

        !(path.starts_with(env.common.fs_root.join("hab").join("pkgs"))
            || path.starts_with(env.common.fs_root.join("hab").join("cache"))
            || path.starts_with(
                env.common
                    .fs_root
                    .join(env.tmp.components().skip(1).collect::<PathBuf>()),
            )
            || path.starts_with(
                env.common
                    .fs_root
                    .join(env.tmpdir.components().skip(1).collect::<PathBuf>()),
            )
            || path.starts_with(
                env.common
                    .fs_root
                    .join(env.temp.components().skip(1).collect::<PathBuf>()),
            )
            || path.starts_with(
                env.common
                    .fs_root
                    .join(env.tempdir.components().skip(1).collect::<PathBuf>()),
            ))
    }
    fn install_name(&self, env: &LDEnvironment) -> PathBuf {
        let mut path = self
            .as_ref()
            .absolutize_from(&env.common.cwd)
            .unwrap()
            .to_path_buf();
        if path.extension().unwrap() == "tbd" {
            path.set_extension("dylib");
        }
        path
    }

    fn is_allowed_impure_path(&self, env: &LDEnvironment) -> bool {
        let path = self.as_ref();
        for dir in env.allowed_impure_paths.iter() {
            if path.starts_with(dir) {
                return true;
            }
        }
        false
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
            .map(|p| {
                #[cfg(target_os = "linux")]
                return p.ends_with(".so") || p.contains(".so.");
                #[cfg(target_os = "macos")]
                return p.ends_with(".dylib") || p.ends_with(".tbd");
            })
            .unwrap_or(false)
    }

    fn is_static_library(&self) -> bool {
        self.as_ref().extension().is_some_and(|ext| ext == "a")
    }
}

#[derive(Debug, Hash, PartialEq, Eq, Clone)]
enum LibraryReference {
    Name(String),
    FilePath(PathBuf),
    AlternateFilePath(PathBuf, PathBuf),
}

/// The type of output generated by the linker
#[derive(Debug, Hash, PartialEq, Eq, Clone, Copy, Default)]
#[allow(clippy::enum_variant_names)]
enum LinkerOutputFormat {
    #[default]
    MachOExcecutable,
    MachODynamicLibrary,
    MachOBundle,
    MachOObject,
    MachODynamicLinker,
}

/// The type of linking used for the generated mach-o object
#[derive(Debug, Hash, PartialEq, Eq, Clone, Copy, Default)]
enum LinkerOutputLinkType {
    #[default]
    Dynamic,
    Static,
    Preload,
}

/// The type of linking used for the generated mach-o object
#[derive(Debug, Hash, PartialEq, Eq, Clone, Copy, Default)]
enum LinkerSearchOrder {
    #[default]
    SearchPathsFirst,
    SearchDylibsFirst,
}

#[derive(Debug, Default, Clone)]
struct LinkerState {
    syslibroot: Option<PathBuf>,
    format: LinkerOutputFormat,
    link_type: LinkerOutputLinkType,
    search_order: LinkerSearchOrder,
}

#[derive(Debug)]
struct LibraryReferenceState {
    library_name: Option<String>,
    results: Vec<LibraryLinkResult>,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
#[allow(clippy::enum_variant_names)]
enum LibraryLinkType {
    StaticLibraryLinked,
    DynamicLibraryLinked,
    TextBasedDynamicLibraryLinked,
}

#[derive(Debug, PartialEq, Eq, Clone)]
struct LibraryLinkResult {
    link_type: LibraryLinkType,
    install_name: PathBuf,
    library_file: PathBuf,
    library_dir: PathBuf,
    is_allowed_impure_path: bool,
    is_pkg_path: bool,
    is_available_at_runtime: bool,
}

#[derive(Debug, PartialEq, Eq, Clone)]
struct LibraryLinkState {
    library_reference: LibraryReference,
    link_result: Option<LibraryLinkResult>,
}

#[derive(Debug)]
enum LDArgument<'a> {
    LinkArgsFile(&'a str),
    LibrarySearchPath(&'a str, bool),
    SystemLibraryRoot(&'a str),
    LibraryName(&'a str, bool),
    LibraryFilePath(&'a str, bool),
    AlternateLibraryFilePath(&'a str, &'a str),
    RPath(&'a str, bool),
    Output(&'a str),
    LTOLibrary(&'a str),
    InstallName(&'a str),
    OutputMachOExcecutable,
    OutputMachODynamicLibrary,
    OutputMachOBundle,
    OutputMachOObject,
    OutputMachODynamicLinker,
    LinkStatic,
    LinkDynamic,
    LinkPreload,
    SearchPathsFirst,
    SearchDylibsFirst,
}

impl<'a> LDArgument<'a> {
    fn parse(
        previous_argument: Option<&'a str>,
        current_argument: &'a str,
    ) -> Option<LDArgument<'a>> {
        match (previous_argument, current_argument) {
            // Static link mode
            (_, "-static") => Some(LDArgument::LinkStatic),
            // Dynamic link mode
            (_, "-dynamic") => Some(LDArgument::LinkDynamic),
            // Preload link mode, used for firmware or embedded development
            (_, "-preload") => Some(LDArgument::LinkPreload),
            (_, "-execute") => Some(LDArgument::OutputMachOExcecutable),
            (_, "-dylib") => Some(LDArgument::OutputMachODynamicLibrary),
            (_, "-bundle") => Some(LDArgument::OutputMachOBundle),
            (_, "-r") => Some(LDArgument::OutputMachOObject),
            (_, "-dylinker") => Some(LDArgument::OutputMachODynamicLinker),
            // Searchs each directory in the library search path for libx.dylib and then libx.a before moving on
            (_, "-search_paths_first") => Some(LDArgument::SearchPathsFirst),
            // Searchs all directories in the search path for libx.dylib first, then it looks for libx.a in in the search path.
            // This was the behaviour of the linker prior to Xcode4
            (_, "-search_dylibs_first") => Some(LDArgument::SearchDylibsFirst),
            // File with linker arguments
            (_, current_value) if current_value.is_prefixed_with("@") => Some(
                LDArgument::LinkArgsFile(current_value.strip_prefix('@').unwrap()),
            ),
            // Output argument
            (Some("-o"), current_value) => Some(LDArgument::Output(current_value)),
            // Dynamic library install name argument
            (Some("-install_name" | "-dylib_install_name"), current_value) => {
                Some(LDArgument::InstallName(current_value))
            }
            // Library Search Path arguments
            (Some("-L"), current_value) => {
                Some(LDArgument::LibrarySearchPath(current_value, false))
            }
            (_, current_value) if current_value.is_prefixed_with("-L") => Some(
                LDArgument::LibrarySearchPath(current_value.strip_prefix("-L").unwrap(), true),
            ),
            // System Library Root arguments
            (Some("-syslibroot"), current_value) => {
                Some(LDArgument::SystemLibraryRoot(current_value))
            }
            // RPath arguments
            (Some("-rpath"), current_value) => Some(LDArgument::RPath(current_value, false)),
            // Library arguments
            (Some("-dylib_file"), current_value) => {
                let mut parts = current_value.split(':');
                let install_name = parts.next().unwrap();
                let file_name = parts.next().unwrap();
                Some(LDArgument::AlternateLibraryFilePath(
                    install_name,
                    file_name,
                ))
            }
            // Link Time Optimization library argument
            (Some("-lto_library"), current_value) => Some(LDArgument::LTOLibrary(current_value)),
            (Some("-l"), current_value) => Some(LDArgument::LibraryName(current_value, false)),
            (
                Some(
                    "-lazy_library" | "-needed_library" | "-reexport_library" | "-upward_library"
                    | "-weak_library",
                ),
                current_value,
            ) => Some(LDArgument::LibraryFilePath(current_value, false)),
            (Some("-force_load" | "-load_hidden"), current_value) => {
                Some(LDArgument::LibraryFilePath(current_value, false))
            }
            // Ignore these if they are the current argument to prevent it from being mistaken for a -lx library argument
            (_, "-load_hidden" | "-lazy_framework" | "-lazy_library" | "-lto_library") => None,
            (_, current_value) if current_value.is_prefixed_with("-lazy-l") => Some(
                LDArgument::LibraryName(current_value.strip_prefix("-lazy-l").unwrap(), true),
            ),
            (_, current_value) if current_value.is_prefixed_with("-l") => Some(
                LDArgument::LibraryName(current_value.strip_prefix("-l").unwrap(), true),
            ),
            (_, current_value) if current_value.is_prefixed_with("-needed-l") => Some(
                LDArgument::LibraryName(current_value.strip_prefix("-needed-l").unwrap(), true),
            ),
            (_, current_value) if current_value.is_prefixed_with("-reexport-l") => Some(
                LDArgument::LibraryName(current_value.strip_prefix("-reexport-l").unwrap(), true),
            ),
            (_, current_value) if current_value.is_prefixed_with("-upward-l") => Some(
                LDArgument::LibraryName(current_value.strip_prefix("-upward-l").unwrap(), true),
            ),
            (_, current_value) if current_value.is_prefixed_with("-hidden-l") => Some(
                LDArgument::LibraryName(current_value.strip_prefix("-hidden-l").unwrap(), true),
            ),
            (_, current_value) if current_value.is_prefixed_with("-weak-l") => Some(
                LDArgument::LibraryName(current_value.strip_prefix("-weak-l").unwrap(), true),
            ),
            _ => None,
        }
    }
}

fn effective_library_name(library_reference: &LibraryReference) -> Option<String> {
    match library_reference {
        LibraryReference::Name(library_name) => Some(library_name.to_string()),
        LibraryReference::FilePath(library_file_name)
        | LibraryReference::AlternateFilePath(library_file_name, _) => library_file_name
            .file_name()
            .and_then(|file_name| file_name.to_str())
            .and_then(|file_name| file_name.strip_prefix("lib"))
            .and_then(|file_name| file_name.split_once('.'))
            .map(|(library_name, _)| library_name.to_string()),
    }
}
fn parse_linker_arguments(
    arguments: impl Iterator<Item = String>,
    env: &LDEnvironment,
) -> Vec<String> {
    let mut linker_state = LinkerState::default();
    let mut filtered_arguments = vec![];
    let mut previous_argument: Option<String> = None;

    let mut library_search_paths = vec![PathBuf::from("usr/lib"), PathBuf::from("usr/local/lib")];
    let mut rpaths: Vec<PathBuf> = vec![];
    let mut additional_rpaths: Vec<PathBuf> = vec![];
    let mut library_references = Vec::new();
    let mut libraries_linked = Vec::new();

    for argument in arguments {
        let mut skip_argument = false;
        let mut skip_prev_argument = false;
        if let Some(known_argument) =
            LDArgument::parse(previous_argument.as_deref(), &argument)
        {
            match known_argument {
                LDArgument::Output(_) | LDArgument::InstallName(_) | LDArgument::LTOLibrary(_) => {
                    // Nothing to do for these arguments
                }
                LDArgument::LinkArgsFile(_) => {}
                LDArgument::LibrarySearchPath(search_path, is_prefixed) => {
                    if search_path.is_impure_path(env) {
                        skip_argument = true;
                        skip_prev_argument = !is_prefixed;
                    } else {
                        let search_path = search_path.absolute_path(env);
                        if !library_search_paths.contains(&search_path) {
                            library_search_paths.insert(0, search_path);
                        }
                    }
                }
                LDArgument::LibraryName(library_name, _) => {
                    let library_reference = LibraryReference::Name(library_name.to_string());
                    let library_name = effective_library_name(&library_reference);
                    library_references.push((
                        library_reference,
                        LibraryReferenceState {
                            library_name,
                            results: Vec::new(),
                        },
                    ));
                }
                LDArgument::LibraryFilePath(library_file_path, is_prefixed) => {
                    let library_file_path = library_file_path.absolute_path(env);
                    if library_file_path.is_impure_path(env) {
                        skip_argument = true;
                        skip_prev_argument = !is_prefixed;
                    } else {
                        let library_reference = LibraryReference::FilePath(library_file_path);
                        let library_name = effective_library_name(&library_reference);
                        library_references.push((
                            library_reference,
                            LibraryReferenceState {
                                library_name,
                                results: Vec::new(),
                            },
                        ));
                    }
                }
                LDArgument::AlternateLibraryFilePath(install_name_path, actual_file_path) => {
                    let install_name_path = install_name_path.absolute_path(env);
                    let actual_file_path = actual_file_path.absolute_path(env);
                    if install_name_path.is_impure_path(env) {
                        skip_argument = true;
                        skip_prev_argument = true;
                    } else {
                        let library_reference = LibraryReference::AlternateFilePath(
                            install_name_path,
                            actual_file_path,
                        );
                        let library_name = effective_library_name(&library_reference);
                        library_references.push((
                            library_reference,
                            LibraryReferenceState {
                                library_name,
                                results: Vec::new(),
                            },
                        ));
                    }
                }
                LDArgument::RPath(rpath, is_prefixed) => {
                    if rpath.is_impure_path(env) {
                        skip_argument = true;
                        skip_prev_argument = !is_prefixed;
                    } else {
                        let rpath = rpath.absolute_path(env);
                        if !rpaths.contains(&rpath) {
                            rpaths.push(rpath);
                        }
                    }
                }
                LDArgument::SystemLibraryRoot(syslibroot) => {
                    linker_state.syslibroot = Some(PathBuf::from(syslibroot))
                }
                LDArgument::LinkStatic => linker_state.link_type = LinkerOutputLinkType::Static,
                LDArgument::LinkDynamic => linker_state.link_type = LinkerOutputLinkType::Dynamic,
                LDArgument::LinkPreload => linker_state.link_type = LinkerOutputLinkType::Preload,
                LDArgument::OutputMachOExcecutable => {
                    linker_state.format = LinkerOutputFormat::MachOExcecutable
                }
                LDArgument::OutputMachODynamicLibrary => {
                    linker_state.format = LinkerOutputFormat::MachODynamicLibrary
                }
                LDArgument::OutputMachOBundle => {
                    linker_state.format = LinkerOutputFormat::MachOBundle
                }
                LDArgument::OutputMachOObject => {
                    linker_state.format = LinkerOutputFormat::MachOObject
                }
                LDArgument::OutputMachODynamicLinker => {
                    linker_state.format = LinkerOutputFormat::MachODynamicLinker
                }
                LDArgument::SearchPathsFirst => {
                    linker_state.search_order = LinkerSearchOrder::SearchPathsFirst
                }
                LDArgument::SearchDylibsFirst => {
                    linker_state.search_order = LinkerSearchOrder::SearchDylibsFirst
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
    match linker_state.search_order {
        LinkerSearchOrder::SearchPathsFirst => {
            // Search for libraries in library search paths
            for library_search_path in library_search_paths.iter() {
                let library_search_path = linker_state
                    .syslibroot
                    .clone()
                    .unwrap_or(PathBuf::from("/"))
                    .join(library_search_path);
                for (library_reference, library_state) in library_references.iter_mut() {
                    match library_reference {
                        LibraryReference::Name(library_name) => {
                            let (shared_library_exists, library_file_path) = {
                                if cfg!(target_os = "linux") {
                                    let library_file_path =
                                        library_search_path.join(format!("lib{}.so", library_name));
                                    (library_file_path.is_file(), library_file_path)
                                } else if cfg!(target_os = "macos") {
                                    let library_file_path = library_search_path
                                        .join(format!("lib{}.tbd", library_name));
                                    if library_file_path.is_file() {
                                        (true, library_file_path)
                                    } else {
                                        let library_file_path = library_search_path
                                            .join(format!("lib{}.dylib", library_name));
                                        (library_file_path.is_file(), library_file_path)
                                    }
                                } else {
                                    unimplemented!("This OS is not supported yet")
                                }
                            };
                            if shared_library_exists {
                                library_state.results.push(LibraryLinkResult {
                                    link_type: if cfg!(target_os = "macos") {
                                        if library_file_path.extension().unwrap() == "tbd" {
                                            LibraryLinkType::TextBasedDynamicLibraryLinked
                                        } else {
                                            LibraryLinkType::DynamicLibraryLinked
                                        }
                                    } else {
                                        LibraryLinkType::DynamicLibraryLinked
                                    },
                                    install_name: library_file_path.install_name(env),
                                    library_file: library_file_path.clone(),
                                    library_dir: library_search_path.clone(),
                                    is_allowed_impure_path: library_file_path
                                        .is_allowed_impure_path(env),
                                    is_pkg_path: library_file_path.is_pkg_path(env),
                                    is_available_at_runtime: library_file_path
                                        .is_allowed_impure_path(env)
                                        || env.ld_run_path.contains(&library_search_path),
                                });
                            } else {
                                let library_file_path =
                                    library_search_path.join(format!("lib{}.a", library_name));
                                if library_file_path.is_file() {
                                    library_state.results.push(LibraryLinkResult {
                                        link_type: LibraryLinkType::StaticLibraryLinked,
                                        install_name: library_file_path.install_name(env),
                                        library_file: library_file_path.clone(),
                                        library_dir: library_search_path.clone(),
                                        is_allowed_impure_path: library_file_path
                                            .is_allowed_impure_path(env),
                                        is_pkg_path: library_file_path.is_pkg_path(env),
                                        is_available_at_runtime: false,
                                    });
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
                                    library_state.results.push(LibraryLinkResult {
                                        link_type: if cfg!(target_os = "macos") {
                                            if library_file_path.extension().unwrap() == "tbd" {
                                                LibraryLinkType::TextBasedDynamicLibraryLinked
                                            } else {
                                                LibraryLinkType::DynamicLibraryLinked
                                            }
                                        } else {
                                            LibraryLinkType::DynamicLibraryLinked
                                        },
                                        install_name: library_file_path.install_name(env),
                                        library_file: library_file_path.clone(),
                                        library_dir: library_search_path.clone(),
                                        is_allowed_impure_path: library_file_path
                                            .is_allowed_impure_path(env),
                                        is_pkg_path: library_file_path.is_pkg_path(env),
                                        is_available_at_runtime: library_file_path
                                            .is_allowed_impure_path(env)
                                            || env.ld_run_path.contains(&library_search_path),
                                    });
                                }
                            } else if library_file_path.is_static_library() {
                                #[allow(clippy::collapsible_if)]
                                if library_file_path.is_file() {
                                    library_state.results.push(LibraryLinkResult {
                                        link_type: LibraryLinkType::StaticLibraryLinked,
                                        install_name: library_file_path.install_name(env),
                                        library_file: library_file_path.clone(),
                                        library_dir: library_search_path.clone(),
                                        is_allowed_impure_path: library_file_path
                                            .is_allowed_impure_path(env),
                                        is_pkg_path: library_file_path.is_pkg_path(env),
                                        is_available_at_runtime: false,
                                    });
                                }
                            }
                        }
                        LibraryReference::AlternateFilePath(install_name, library_file_path) => {
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
                                    library_state.results.push(LibraryLinkResult {
                                        link_type: if cfg!(target_os = "macos") {
                                            if library_file_path.extension().unwrap() == "tbd" {
                                                LibraryLinkType::TextBasedDynamicLibraryLinked
                                            } else {
                                                LibraryLinkType::DynamicLibraryLinked
                                            }
                                        } else {
                                            LibraryLinkType::DynamicLibraryLinked
                                        },
                                        install_name: install_name.install_name(env),
                                        library_file: library_file_path.clone(),
                                        library_dir: library_search_path.clone(),
                                        is_allowed_impure_path: library_file_path
                                            .is_allowed_impure_path(env),
                                        is_pkg_path: library_file_path.is_pkg_path(env),
                                        is_available_at_runtime: library_file_path
                                            .is_allowed_impure_path(env)
                                            || env.ld_run_path.contains(&library_search_path),
                                    });
                                }
                            } else if library_file_path.is_static_library() {
                                #[allow(clippy::collapsible_if)]
                                if library_file_path.is_file() {
                                    library_state.results.push(LibraryLinkResult {
                                        link_type: LibraryLinkType::StaticLibraryLinked,
                                        install_name: install_name.install_name(env),
                                        library_file: library_file_path.clone(),
                                        library_dir: library_search_path.clone(),
                                        is_allowed_impure_path: library_file_path
                                            .is_allowed_impure_path(env),
                                        is_pkg_path: library_file_path.is_pkg_path(env),
                                        is_available_at_runtime: false,
                                    });
                                }
                            }
                        }
                    }
                }
            }
        }
        LinkerSearchOrder::SearchDylibsFirst => {
            // Search for dylibs first
            for (library_reference, library_state) in library_references.iter_mut() {
                for library_search_path in library_search_paths.iter() {
                    let library_search_path = linker_state
                        .syslibroot
                        .clone()
                        .unwrap_or(PathBuf::from("/"))
                        .join(library_search_path);
                    match library_reference {
                        LibraryReference::Name(library_name) => {
                            let (shared_library_exists, library_file_path) = {
                                if cfg!(target_os = "linux") {
                                    let library_file_path =
                                        library_search_path.join(format!("lib{}.so", library_name));
                                    (library_file_path.is_file(), library_file_path)
                                } else if cfg!(target_os = "macos") {
                                    let library_file_path = library_search_path
                                        .join(format!("lib{}.tbd", library_name));
                                    if library_file_path.is_file() {
                                        (true, library_file_path)
                                    } else {
                                        let library_file_path = library_search_path
                                            .join(format!("lib{}.dylib", library_name));
                                        (library_file_path.is_file(), library_file_path)
                                    }
                                } else {
                                    unimplemented!("This OS is not supported yet")
                                }
                            };
                            if shared_library_exists {
                                library_state.results.push(LibraryLinkResult {
                                    link_type: if cfg!(target_os = "macos") {
                                        if library_file_path.extension().unwrap() == "tbd" {
                                            LibraryLinkType::TextBasedDynamicLibraryLinked
                                        } else {
                                            LibraryLinkType::DynamicLibraryLinked
                                        }
                                    } else {
                                        LibraryLinkType::DynamicLibraryLinked
                                    },
                                    install_name: library_file_path.install_name(env),
                                    library_file: library_file_path.clone(),
                                    library_dir: library_search_path.clone(),
                                    is_allowed_impure_path: library_file_path
                                        .is_allowed_impure_path(env),
                                    is_pkg_path: library_file_path.is_pkg_path(env),
                                    is_available_at_runtime: env
                                        .ld_run_path
                                        .contains(&library_search_path),
                                });
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
                                #[allow(clippy::collapsible_if)]
                                if library_file_path.is_file() {
                                    library_state.results.push(LibraryLinkResult {
                                        link_type: if cfg!(target_os = "macos") {
                                            if library_file_path.extension().unwrap() == "tbd" {
                                                LibraryLinkType::TextBasedDynamicLibraryLinked
                                            } else {
                                                LibraryLinkType::DynamicLibraryLinked
                                            }
                                        } else {
                                            LibraryLinkType::DynamicLibraryLinked
                                        },
                                        install_name: library_file_path.install_name(env),
                                        library_file: library_file_path.clone(),
                                        library_dir: library_search_path.clone(),
                                        is_allowed_impure_path: library_file_path
                                            .is_allowed_impure_path(env),
                                        is_pkg_path: library_file_path.is_pkg_path(env),
                                        is_available_at_runtime: env
                                            .ld_run_path
                                            .contains(&library_search_path),
                                    });
                                }
                            }
                        }
                        LibraryReference::AlternateFilePath(install_name, library_file_path) => {
                            // File's can only be found once irrespective of search path
                            if !library_state.results.is_empty() {
                                continue;
                            }
                            let library_search_path: PathBuf = install_name
                                .absolute_path(env)
                                .parent()
                                .unwrap()
                                .to_path_buf();
                            if library_file_path.is_shared_library() {
                                #[allow(clippy::collapsible_if)]
                                if library_file_path.is_file() {
                                    library_state.results.push(LibraryLinkResult {
                                        link_type: if cfg!(target_os = "macos") {
                                            if library_file_path.extension().unwrap() == "tbd" {
                                                LibraryLinkType::TextBasedDynamicLibraryLinked
                                            } else {
                                                LibraryLinkType::DynamicLibraryLinked
                                            }
                                        } else {
                                            LibraryLinkType::DynamicLibraryLinked
                                        },
                                        install_name: install_name.install_name(env),
                                        library_file: library_file_path.clone(),
                                        library_dir: library_search_path.clone(),
                                        is_allowed_impure_path: library_file_path
                                            .is_allowed_impure_path(env),
                                        is_pkg_path: library_file_path.is_pkg_path(env),
                                        is_available_at_runtime: env
                                            .ld_run_path
                                            .contains(&library_search_path),
                                    });
                                }
                            }
                        }
                    }
                }
            }
            // Search for static libs second
            for (library_reference, library_state) in library_references.iter_mut() {
                for library_search_path in library_search_paths.iter() {
                    let library_search_path = match linker_state.syslibroot {
                        Some(ref syslibroot) => syslibroot.join(library_search_path),
                        None => library_search_path.clone(),
                    };
                    match library_reference {
                        LibraryReference::Name(library_name) => {
                            let library_file_path =
                                library_search_path.join(format!("lib{}.a", library_name));
                            if library_file_path.is_file() {
                                library_state.results.push(LibraryLinkResult {
                                    link_type: LibraryLinkType::StaticLibraryLinked,
                                    install_name: library_file_path.install_name(env),
                                    library_file: library_file_path.clone(),
                                    library_dir: library_search_path.clone(),
                                    is_allowed_impure_path: library_file_path
                                        .is_allowed_impure_path(env),
                                    is_pkg_path: library_file_path.is_pkg_path(env),
                                    is_available_at_runtime: false,
                                });
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
                            if library_file_path.is_static_library() {
                                #[allow(clippy::collapsible_if)]
                                if library_file_path.is_file() {
                                    library_state.results.push(LibraryLinkResult {
                                        link_type: LibraryLinkType::StaticLibraryLinked,
                                        install_name: library_file_path.install_name(env),
                                        library_file: library_file_path.clone(),
                                        library_dir: library_search_path.clone(),
                                        is_allowed_impure_path: library_file_path
                                            .is_allowed_impure_path(env),
                                        is_pkg_path: library_file_path.is_pkg_path(env),
                                        is_available_at_runtime: false,
                                    });
                                }
                            }
                        }
                        LibraryReference::AlternateFilePath(install_name, library_file_path) => {
                            // File's can only be found once irrespective of search path
                            if !library_state.results.is_empty() {
                                continue;
                            }
                            let library_search_path: PathBuf = install_name
                                .absolute_path(env)
                                .parent()
                                .unwrap()
                                .to_path_buf();
                            if library_file_path.is_static_library() {
                                #[allow(clippy::collapsible_if)]
                                if library_file_path.is_file() {
                                    library_state.results.push(LibraryLinkResult {
                                        link_type: LibraryLinkType::StaticLibraryLinked,
                                        install_name: install_name.install_name(env),
                                        library_file: library_file_path.clone(),
                                        library_dir: library_search_path.clone(),
                                        is_allowed_impure_path: library_file_path
                                            .is_allowed_impure_path(env),
                                        is_pkg_path: library_file_path.is_pkg_path(env),
                                        is_available_at_runtime: false,
                                    });
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    let mut all_needed_libraries_will_link = true;
    // We go through the search results for each library reference
    for (library_reference, library_state) in library_references.iter() {
        let mut library_will_link = false;
        let mut library_is_available_at_runtime = false;
        if let Some(library_name) = library_state.library_name.as_ref() {
            let mut library_index = 0;
            if let Some(LibraryLinkState { link_result, .. }) =
                libraries_linked.iter().find_map(|(name, link_state)| {
                    library_index += 1;
                    if name == library_name {
                        Some(link_state)
                    } else {
                        None
                    }
                })
            {
                match link_result {
                    Some(link_result) => match link_result.link_type {
                        LibraryLinkType::StaticLibraryLinked => continue,
                        LibraryLinkType::DynamicLibraryLinked
                        | LibraryLinkType::TextBasedDynamicLibraryLinked => {
                            // We have already found a folder available at runtime with the library
                            if link_result.is_available_at_runtime {
                                continue;
                            }
                        }
                    },
                    None => {
                        continue;
                    }
                }
            } else {
                library_index = 0;
            }

            let mut final_link_result = None;
            for result in library_state.results.iter() {
                match result.link_type {
                    LibraryLinkType::StaticLibraryLinked => {
                        library_will_link = true;
                    }
                    LibraryLinkType::DynamicLibraryLinked
                    | LibraryLinkType::TextBasedDynamicLibraryLinked => {
                        if result.is_pkg_path {
                            library_will_link = true;
                            library_is_available_at_runtime = result.is_available_at_runtime;
                        } else if result.is_allowed_impure_path {
                            library_will_link = true;
                            library_is_available_at_runtime = true;
                        }
                    }
                }
                if library_will_link {
                    #[allow(clippy::if_same_then_else)]
                    if final_link_result.is_none() {
                        final_link_result = Some(result.clone());
                    } else if library_is_available_at_runtime {
                        final_link_result = Some(result.clone());
                    }
                    // If the library is available at runtime no need
                    // to examine further results
                    if library_is_available_at_runtime {
                        break;
                    }
                }
            }
            // This indicates that there is a dependency on a library that is built in
            // the same package
            if !library_will_link {
                all_needed_libraries_will_link = false;
            }
            if library_index >= 1 {
                libraries_linked.remove(library_index - 1);
            }

            libraries_linked.push((
                library_name.to_string(),
                LibraryLinkState {
                    library_reference: library_reference.clone(),
                    link_result: final_link_result,
                },
            ));
            // println!("S | {:?}", libraries_linked);
        }
    }

    // Add the final library directories for all shared libraries to the rpaths
    for (_, link_state) in libraries_linked.iter() {
        if let Some(link_result) = link_state.link_result.as_ref() {
            match link_result.link_type {
                LibraryLinkType::StaticLibraryLinked => {}
                LibraryLinkType::DynamicLibraryLinked
                | LibraryLinkType::TextBasedDynamicLibraryLinked => {
                    if !rpaths.contains(&link_result.library_dir)
                        && !additional_rpaths.contains(&link_result.library_dir)
                        && !link_result.is_allowed_impure_path
                    {
                        additional_rpaths.push(link_result.library_dir.clone())
                    }
                }
            }
        }
    }

    if !all_needed_libraries_will_link {
        if let Some(prefix) = env.prefix.as_ref() {
            for run_path in env.ld_run_path.iter() {
                if run_path.starts_with(prefix) {
                    #[allow(clippy::collapsible_if)]
                    if !rpaths.contains(run_path) && !additional_rpaths.contains(run_path) {
                        additional_rpaths.push(run_path.clone())
                    }
                }
            }
        }
    }
    // Ensure all paths in the LD_RUN_PATH are added to the rpath
    if env.ld_link_mode == LinkMode::Complete {
        for search_dir in env.ld_run_path.iter() {
            if !rpaths.contains(search_dir) && !additional_rpaths.contains(search_dir) {
                additional_rpaths.push(search_dir.clone())
            }
        }
    }

    // Only add addtional rpaths if there is a dynamic linker
    if linker_state.link_type == LinkerOutputLinkType::Dynamic {
        for additional_rpath in additional_rpaths.iter() {
            filtered_arguments.push("-rpath".to_string());
            filtered_arguments.push(additional_rpath.display().to_string());
        }
    }

    if env.common.is_debug {
        if let Some(debug_log_file) = env.common.debug_log_file.as_ref() {
            let mut file = std::fs::OpenOptions::new()
                .write(true)
                .append(true)
                .create(true)
                .open(debug_log_file)
                .expect("Failed to open debug output log file");
            writeln!(&mut file, "prefix: {:?}", env.prefix).unwrap();
            writeln!(&mut file, "ld_run_path: {:#?}", env.ld_run_path).unwrap();
            writeln!(&mut file, "ld_link_mode: {}", env.ld_link_mode).unwrap();
            writeln!(&mut file, "linker_state: {:#?}", linker_state).unwrap();
            writeln!(
                &mut file,
                "library_search_paths: {:#?}",
                library_search_paths
            )
            .unwrap();
            writeln!(&mut file, "library_references: {:#?}", library_references).unwrap();
            writeln!(&mut file, "rpaths: {:#?}", rpaths).unwrap();
            writeln!(&mut file, "additional_rpaths: {:#?}", additional_rpaths).unwrap();
            writeln!(&mut file, "libraries_linked: {:#?}", libraries_linked).unwrap();
            writeln!(
                &mut file,
                "all_needed_libraries_will_link: {}",
                all_needed_libraries_will_link
            )
            .unwrap();
            writeln!(
                &mut file,
                "filtered_ld_arguments: {:#?}",
                filtered_arguments
            )
            .unwrap();
        } else {
            let mut file = std::io::stderr().lock();
            writeln!(&mut file, "prefix: {:?}", env.prefix).unwrap();
            writeln!(&mut file, "ld_run_path: {:#?}", env.ld_run_path).unwrap();
            writeln!(&mut file, "ld_link_mode: {}", env.ld_link_mode).unwrap();
            writeln!(&mut file, "linker_state: {:#?}", linker_state).unwrap();
            writeln!(
                &mut file,
                "library_search_paths: {:#?}",
                library_search_paths
            )
            .unwrap();
            writeln!(&mut file, "library_references: {:#?}", library_references).unwrap();
            writeln!(&mut file, "rpaths: {:#?}", rpaths).unwrap();
            writeln!(&mut file, "additional_rpaths: {:#?}", additional_rpaths).unwrap();
            writeln!(&mut file, "libraries_linked: {:#?}", libraries_linked).unwrap();
            writeln!(
                &mut file,
                "all_needed_libraries_will_link: {}",
                all_needed_libraries_will_link
            )
            .unwrap();
            writeln!(
                &mut file,
                "filtered_ld_arguments: {:#?}",
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
        }),
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
            writeln!(
                &mut file,
                "work_dir: {}",
                std::env::current_dir().unwrap().display()
            )
            .unwrap();
            writeln!(
                &mut file,
                "original: {}",
                std::env::args().skip(1).collect::<Vec<String>>().join(" ")
            )
            .unwrap();
            writeln!(
                &mut file,
                "wrapped: {} {}",
                program,
                parsed_arguments.join(" ")
            )
            .unwrap();
        } else {
            let mut file = std::io::stderr().lock();
            writeln!(
                &mut file,
                "work_dir: {}",
                std::env::current_dir().unwrap().display()
            )
            .unwrap();
            writeln!(
                &mut file,
                "original: {}",
                std::env::args().skip(1).collect::<Vec<String>>().join(" ")
            )
            .unwrap();
            writeln!(
                &mut file,
                "wrapped: {} {}",
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
    fn touch(path: impl AsRef<std::path::Path>) {
        if let Some(parent_dir) = path.as_ref().parent() {
            std::fs::create_dir_all(parent_dir).unwrap();
        }
        std::fs::File::create(path).unwrap();
    }
    fn shared_lib_name(name: impl AsRef<str>) -> String {
        #[cfg(target_os = "macos")]
        {
            return format!("lib{}.dylib", name.as_ref());
        }
        #[cfg(target_os = "linux")]
        {
            return format!("lib{}.so", name.as_ref());
        }
    }

    mod complete_ld64_link_mode {
        use hab_pkg_wrappers::env::CommonEnvironment;
        use tempdir::TempDir;

        use crate::{parse_linker_arguments, LDEnvironment};

        use super::{shared_lib_name, touch};

        // This scenario checks that whatever directory is present in the LD_RUN_PATH will be added
        // as an rpath argument regardless of if the library path is provided to the linker and regardless
        // of if the library is actually linked into the binary. This is the default behaviour as it was
        // behaviour of the previous shell based ld wrapper script.
        #[test]
        fn linking() {
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
            let libc_shared = libc_search_path.join(shared_lib_name("c"));
            touch(libc_shared);
            let libx_search_path = temp_dir
                .path()
                .join("hab")
                .join("pkgs")
                .join("core")
                .join("xlib")
                .join("version")
                .join("release")
                .join("lib");
            let libx_shared = libx_search_path.join(shared_lib_name("x"));
            touch(libx_shared);
            let libz_search_path = temp_dir
                .path()
                .join("hab")
                .join("pkgs")
                .join("core")
                .join("zlib")
                .join("version")
                .join("release")
                .join("lib");
            let libz_shared = libz_search_path.join(shared_lib_name("z"));
            touch(libz_shared);
            let libm_search_path = temp_dir
                .path()
                .join("hab")
                .join("pkgs")
                .join("core")
                .join("libm")
                .join("version")
                .join("release")
                .join("lib");

            // libc is linked with a search path
            // libx is linked with a search path not present in LD_RUN_PATH
            // libz is linked without a search path
            // libm is not linked
            let raw_link_arguments = format!(
                "-lc -L {} -lx -L {} -lz",
                libc_search_path.display(),
                libx_search_path.display()
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
                ld_run_path: vec![
                    libz_search_path.clone(),
                    libc_search_path.clone(),
                    libm_search_path.clone(),
                ],
                ..Default::default()
            };
            let result = parse_linker_arguments(link_arguments.into_iter(), &env);
            assert_eq!(
                result.join(" "),
                format!(
                    "{} -rpath {} -rpath {} -rpath {} -rpath {}",
                    raw_link_arguments,
                    libc_search_path.display(),
                    libx_search_path.display(),
                    libz_search_path.display(),
                    libm_search_path.display()
                )
            );
        }
    }

    mod minimal_ld64_link_mode {
        use super::{shared_lib_name, touch};
        use hab_pkg_wrappers::env::CommonEnvironment;
        use tempdir::TempDir;

        use crate::{parse_linker_arguments, LDEnvironment, LinkMode};

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
            let libc_shared = libc_search_path.join(shared_lib_name("c"));
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
                ld_link_mode: LinkMode::Minimal,
                ..Default::default()
            };
            let result = parse_linker_arguments(link_arguments.into_iter(), &env);
            assert_eq!(
                result.join(" "),
                format!(
                    "{} -rpath {}",
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
                ld_link_mode: LinkMode::Minimal,
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
            let libc_shared = libc_search_path.join(shared_lib_name("c"));
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
                ld_link_mode: LinkMode::Minimal,
                ..Default::default()
            };
            let result = parse_linker_arguments(link_arguments.into_iter(), &env);
            assert_eq!(
                result.join(" "),
                format!(
                    "{} -rpath {}",
                    raw_link_arguments,
                    libc_search_path.display()
                )
            );
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
            let libc_shared = libc_search_path.join(shared_lib_name("c"));
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
                ld_link_mode: LinkMode::Minimal,
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
            let libc_shared = libc_search_path.join(shared_lib_name("c"));
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
                ld_link_mode: LinkMode::Minimal,
                ..Default::default()
            };
            let result = parse_linker_arguments(link_arguments.into_iter(), &env);
            assert_eq!(
                result.join(" "),
                format!(
                    "{} -rpath {}",
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
            let libc_shared = libc_search_path.join(shared_lib_name("c"));
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
                ld_link_mode: LinkMode::Minimal,
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
            let libcrypto_shared = build_dir.join(shared_lib_name("crypto"));
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
                ld_link_mode: LinkMode::Minimal,
                ld_run_path: vec![install_prefix_dir.join("lib")],
                prefix: Some(install_prefix_dir.clone()),
                ..Default::default()
            };
            let result = parse_linker_arguments(link_arguments.into_iter(), &env);
            assert_eq!(
                result.join(" "),
                format!(
                    "{} -rpath {}",
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
                ld_link_mode: LinkMode::Minimal,
                ld_run_path: vec![install_prefix_dir.join("lib")],
                prefix: Some(install_prefix_dir.clone()),
                ..Default::default()
            };
            let result = parse_linker_arguments(link_arguments.into_iter(), &env);
            assert_eq!(
                result.join(" "),
                format!(
                    "{} -rpath {}",
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
                ld_link_mode: LinkMode::Minimal,
                ld_run_path: vec![install_prefix_dir.join("lib")],
                prefix: Some(install_prefix_dir.clone()),
                ..Default::default()
            };
            let result = parse_linker_arguments(link_arguments.into_iter(), &env);
            assert_eq!(
                result.join(" "),
                format!(
                    "{} -rpath {}",
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
            let old_libc_shared = old_install_prefix_dir.join(shared_lib_name("c"));
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
            let new_libc_shared = build_dir.join(shared_lib_name("c"));
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
                ld_link_mode: LinkMode::Minimal,
                ld_run_path: vec![install_prefix_dir.join("lib")],
                prefix: Some(install_prefix_dir.clone()),
                ..Default::default()
            };
            let result = parse_linker_arguments(link_arguments.into_iter(), &env);
            assert_eq!(
                result.join(" "),
                format!(
                    "{} -rpath {}",
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
                ld_link_mode: LinkMode::Minimal,
                ld_run_path: vec![install_prefix_dir.join("lib")],
                prefix: Some(install_prefix_dir.clone()),
                ..Default::default()
            };
            let result = parse_linker_arguments(link_arguments.into_iter(), &env);
            assert_eq!(
                result.join(" "),
                format!(
                    "{} -rpath {}",
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
                ld_link_mode: LinkMode::Minimal,
                ld_run_path: vec![install_prefix_dir.join("lib")],
                prefix: Some(install_prefix_dir.clone()),
                ..Default::default()
            };
            let result = parse_linker_arguments(link_arguments.into_iter(), &env);
            assert_eq!(
                result.join(" "),
                format!(
                    "{} -rpath {}",
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
                ld_link_mode: LinkMode::Minimal,
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
            let libstdcxx_shared = libstdcxx_search_path.join(shared_lib_name("stdc++"));
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
            let libstdcxx_libs_shared = libstdcxx_libs_search_path.join(shared_lib_name("stdc++"));
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
                ld_link_mode: LinkMode::Minimal,
                prefix: Some(install_prefix_dir.clone()),
                ..Default::default()
            };
            let result = parse_linker_arguments(link_arguments.into_iter(), &env);
            assert_eq!(
                result.join(" "),
                format!(
                    "{} -rpath {}",
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
                ld_link_mode: LinkMode::Minimal,
                ld_run_path: vec![install_lib_dir.clone(), libstdcxx_libs_search_path.clone()],
                prefix: Some(install_prefix_dir.clone()),
                ..Default::default()
            };
            let result = parse_linker_arguments(link_arguments.into_iter(), &env);
            assert_eq!(
                result.join(" "),
                format!(
                    "{} -rpath {}",
                    raw_link_arguments,
                    libstdcxx_libs_search_path.display()
                )
            );

            // This is the case where gcc is a build dep and gcc-libs is a runtime dep but the
            // runtime lib is before the build lib on the search path, the final result should
            // prefer the runtime lib
            let raw_link_arguments = format!(
                "-L{} -L{} -lstdc++",
                libstdcxx_libs_search_path.display(),
                libstdcxx_search_path.display()
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
                ld_link_mode: LinkMode::Minimal,
                ld_run_path: vec![install_lib_dir.clone(), libstdcxx_libs_search_path.clone()],
                prefix: Some(install_prefix_dir.clone()),
                ..Default::default()
            };
            let result = parse_linker_arguments(link_arguments.into_iter(), &env);
            assert_eq!(
                result.join(" "),
                format!(
                    "{} -rpath {}",
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
            let libc_shared = libc_search_path.join(shared_lib_name("c"));
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
                ld_link_mode: LinkMode::Minimal,
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
            let libc_shared = libc_search_path.join(shared_lib_name("c"));
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
                ld_link_mode: LinkMode::Minimal,
                ld_run_path: vec![install_lib_dir.clone(), libc_search_path.clone()],
                ..Default::default()
            };
            let result = parse_linker_arguments(link_arguments.into_iter(), &env);
            assert_eq!(
                result.join(" "),
                format!(
                    "{} -rpath {}",
                    raw_link_arguments,
                    libc_search_path.display()
                )
            );
        }

        #[test]
        fn impure_library_search_path_filtering() {
            let temp_dir = TempDir::new("ld-wrapper-test").unwrap();
            let libc_search_path = temp_dir.path().join("usr").join("lib");
            let libc_shared = libc_search_path.join(shared_lib_name("c"));
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
                ld_link_mode: LinkMode::Minimal,
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
        fn impure_absolute_install_name_library_path_filtering() {
            let temp_dir = TempDir::new("ld-wrapper-test").unwrap();
            let impure_libz_search_path = temp_dir.path().join("usr").join("lib");
            let impure_libz_shared = impure_libz_search_path.join(shared_lib_name("z"));
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
            let libc_shared = libc_search_path.join(shared_lib_name("c"));
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
                ld_link_mode: LinkMode::Minimal,
                ..Default::default()
            };
            let result = parse_linker_arguments(link_arguments.into_iter(), &env);
            assert_eq!(
                result.join(" "),
                format!(
                    "-L {} -lc {} -rpath {0}",
                    libc_search_path.display(),
                    libc_shared.display()
                )
            );
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
            let libc_shared = libc_search_path.join(shared_lib_name("c"));
            touch(libc_shared);

            let raw_link_arguments =
                format!("-L {} -o libc.so -h libc.so", libc_search_path.display());
            let link_arguments = raw_link_arguments
                .split(" ")
                .map(|x| x.to_string())
                .collect::<Vec<String>>();
            let env = LDEnvironment {
                common: CommonEnvironment {
                    fs_root: temp_dir.path().to_path_buf(),
                    ..Default::default()
                },
                ld_link_mode: LinkMode::Minimal,
                ..Default::default()
            };
            let result = parse_linker_arguments(link_arguments.into_iter(), &env);
            assert_eq!(result.join(" "), raw_link_arguments);
        }
    }
}

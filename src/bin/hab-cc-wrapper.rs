use std::{io::Write, path::PathBuf};

use exec::Command;
use hab_pkg_wrappers::{env::CommonEnvironment, opts_parser, util::PrefixedArg};

struct CCEnvironment {
    pub common: CommonEnvironment,
    pub executable_name: String,
    pub ld_bin: Option<PathBuf>,
    pub c_start_files: Vec<PathBuf>,
    pub c_std_libs: Vec<PathBuf>,
    pub c_std_headers: Vec<PathBuf>,
    pub cxx_std_libs: Vec<PathBuf>,
    pub cxx_std_headers: Vec<PathBuf>,
    pub framework_dirs: Vec<PathBuf>,
}

impl Default for CCEnvironment {
    fn default() -> Self {
        Self {
            common: Default::default(),
            executable_name: std::env::var("HAB_CC_EXECUTABLE_NAME").unwrap_or_default(),
            ld_bin: std::env::var("HAB_LD_BIN").map(PathBuf::from).ok(),
            c_start_files: std::env::var("HAB_C_START_FILES")
                .map(|p| p.split(':').map(PathBuf::from).collect::<Vec<_>>())
                .unwrap_or_default(),
            c_std_libs: std::env::var("HAB_C_STD_LIBS")
                .map(|p| p.split(':').map(PathBuf::from).collect::<Vec<_>>())
                .unwrap_or_default(),
            c_std_headers: std::env::var("HAB_C_STD_HEADERS")
                .map(|p| p.split(':').map(PathBuf::from).collect::<Vec<_>>())
                .unwrap_or_default(),
            cxx_std_libs: std::env::var("HAB_CXX_STD_LIBS")
                .map(|p| p.split(':').map(PathBuf::from).collect::<Vec<_>>())
                .unwrap_or_default(),
            cxx_std_headers: std::env::var("HAB_CXX_STD_HEADERS")
                .map(|p| p.split(':').map(PathBuf::from).collect::<Vec<_>>())
                .unwrap_or_default(),
            framework_dirs: std::env::var("HAB_FRAMEWORK_DIRS")
                .map(|p| p.split(':').map(PathBuf::from).collect::<Vec<_>>())
                .unwrap_or_default(),
        }
    }
}

fn parse_cc_arguments(arguments: impl Iterator<Item = String>, env: &CCEnvironment) -> Vec<String> {
    let mut is_cxx = env.executable_name.ends_with("++");

    let mut add_start_files = true;

    let mut add_c_std_headers = true;
    let mut add_c_std_libs = true;

    let mut add_cxx_std_headers = true;
    let mut add_cxx_std_libs = true;

    let mut previous_argument: Option<&String> = None;
    let mut filtered_arguments = Vec::new();
    for argument in arguments {
        match (previous_argument.map(|x| x.as_str()), argument.as_str()) {
            (_, "-nostdinc") => {
                add_c_std_headers = false;
                add_cxx_std_headers = false;
            }
            (_, "-nostdinc++") => {
                add_cxx_std_headers = false;
            }
            (_, "-nolibc") => {
                add_c_std_libs = false;
            }
            (_, "-nostdlib") => {
                add_c_std_libs = false;
                add_cxx_std_libs = false;
            }
            (_, "-nostartfiles") => {
                add_start_files = false;
            }
            (Some("-isysroot"), _) => {
                filtered_arguments.pop();
                previous_argument = filtered_arguments.last();
                continue;
            }
            // C++ source files added
            (Some("-x"), current_argument) if current_argument.is_prefixed_with("c++") => {
                is_cxx = true
            }
            (_, _) => {}
        }
        filtered_arguments.push(argument.clone());
        previous_argument = filtered_arguments.last();
    }
    // Add the path to the linker binary
    if let Some(ld_bin) = &env.ld_bin {
        filtered_arguments.push(format!("-B{}", ld_bin.display()));
    }
    // If start files are needed, add the path to the the libc start files
    if add_start_files {
        for dir in env.c_start_files.iter() {
            filtered_arguments.push(format!("-B{}", dir.display()));
        }
    }
    // If C++ add additional libraries and headers
    if is_cxx {
        if add_cxx_std_libs {
            for lib_dir in env.cxx_std_libs.iter() {
                filtered_arguments.push(format!("-L{}", lib_dir.display()));
            }
        }
        if add_cxx_std_headers {
            for include_dir in env.cxx_std_headers.iter() {
                filtered_arguments.push("-isystem".to_string());
                filtered_arguments.push(include_dir.display().to_string());
            }
        }
    }

    // Add C std libraries and headers
    if add_c_std_libs {
        for lib_dir in env.c_std_libs.iter() {
            filtered_arguments.push(format!("-L{}", lib_dir.display()));
        }
    }
    if add_c_std_headers {
        for include_dir in env.c_std_headers.iter() {
            filtered_arguments.push("-idirafter".to_string());
            filtered_arguments.push(include_dir.display().to_string());
        }
    }

    // Add Framework directories for MacOS
    for framework_dir in env.framework_dirs.iter() {
        filtered_arguments.push("-F".to_string());
        filtered_arguments.push(framework_dir.display().to_string());
    }

    if env.common.is_debug {
        if let Some(debug_log_file) = env.common.debug_log_file.as_ref() {
            let mut file = std::fs::OpenOptions::new()
                .append(true)
                .create(true)
                .open(debug_log_file)
                .expect("Failed to open debug output log file");
            writeln!(&mut file, "is_cxx: {}", is_cxx).unwrap();
            writeln!(&mut file, "add_start_files: {}", add_start_files).unwrap();
            writeln!(&mut file, "add_c_std_libs: {}", add_c_std_libs).unwrap();
            writeln!(&mut file, "add_c_std_headers: {}", add_c_std_headers).unwrap();
            writeln!(&mut file, "add_cxx_std_libs: {}", add_cxx_std_libs).unwrap();
            writeln!(&mut file, "add_cxx_std_headers: {}", add_cxx_std_headers).unwrap();
            writeln!(
                &mut file,
                "filtered_cc_arguments: {:#?}",
                filtered_arguments
            )
            .unwrap();
        } else {
            let mut file = std::io::stderr().lock();
            writeln!(&mut file, "is_cxx: {}", is_cxx).unwrap();
            writeln!(&mut file, "add_start_files: {}", add_start_files).unwrap();
            writeln!(&mut file, "add_c_std_libs: {}", add_c_std_libs).unwrap();
            writeln!(&mut file, "add_c_std_headers: {}", add_c_std_headers).unwrap();
            writeln!(&mut file, "add_cxx_std_libs: {}", add_cxx_std_libs).unwrap();
            writeln!(&mut file, "add_cxx_std_headers: {}", add_cxx_std_headers).unwrap();
            writeln!(
                &mut file,
                "filtered_cc_arguments: {:#?}",
                filtered_arguments
            )
            .unwrap();
        };
    }
    filtered_arguments
}

fn main() {
    let env = CCEnvironment::default();
    let mut args = std::env::args();
    let executable = args.next().unwrap();
    let program = args.next().expect("Wrapped program must be specified");

    let parsed_arguments = parse_cc_arguments(
        args.flat_map(|argument| {
            if argument.is_prefixed_with("@") {
                opts_parser::expand_argument(&argument, &env.common)
            } else {
                vec![argument]
            }
        }),
        &env,
    );

    let mut command = Command::new(&program);
    command.args(&parsed_arguments);
    if env.common.is_debug {
        if let Some(debug_log_file) = env.common.debug_log_file.as_ref() {
            let mut file = std::fs::OpenOptions::new()
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

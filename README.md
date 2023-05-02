# Habitat Package Wrappers

This crate contains utility binaries that are used by wrapper scripts in various habitat packages.

## hab-ld-wrapper

An argument processor for parsing and managing arguments passed to the system linker 'ld'. It offers the following features:

- Supports file-based parameter expansion using arguments prefixed with `@`
- Handles all static and dynamic linking flags, such as `-Bstatic`, `-Bdynamic`, and their variants
- Supports `--push-state` and `--pop-state` linker flags
- Supports `--as-needed` and `--no-as-needed` flags, preventing rpath entries addition if an `--as-needed` library isn't specified as a runtime dependency
- Adds library folders from the same package to rpaths of linked binaries when necessary

**Upcoming Features**
- Support for `--copy-dt-needed` and `--no-copy-dt-needed` flags to recursively read ELF libraries and add rpath entries

### Environment Variables

| Variable | Description |
|----------|-------------|
| HAB_DEBUG | Outputs debug information to stderr for debugging purposes |
| HAB_DEBUG_LOG_FILE | Writes debug information to specified file instead of stderr. This prevents it from breaking certain autoconf checks |
| HAB_DYNAMIC_LINKER | Specifies the dynamic linker to use when creating executables with shared libraries |
| HAB_LD_RUN_PATH | A list of directories containing runtime libraries, separated by ':'; also serves as a hint for including rpath entries for `--as-needed` libraries |
| HAB_ENFORCE_PURITY | Filters out all impure paths passed to the linker when set to '1' |
| PREFIX | The installation folder for the current package being built |
| TMP, TMPDIR, TEMP, TEMPDIR | Temporary directory folders |

By configuring these environment variables, you can easily customize the behavior of the `hab-ld-wrapper` to better suit your habitat package's requirements.

## hab-cc-wrapper

An argument processor for parsing and managing arguments passed to the C compiler. It offers the following features:

- Allows the use of a specific linker binary
- Ensures correct application of various compiler options with specific C and C++ standard libraries

### Environment Variables

| Variable | Description |
|-|-|
| HAB_DEBUG | Outputs debug information to stderr for debugging purposes |
| HAB_DEBUG_LOG_FILE | Writes debug information to specified file instead of stderr. This prevents it from breaking certain autoconf checks |
| HAB_CC_EXECUTABLE_NAME | Specifies the name of the binary being wrapped; used to determine if a C or C++ compiler is in use |
| HAB_LD_BIN | Indicates the path to the desired linker binary |
| HAB_C_START_FILES	| Specifies the path to C start files |
| HAB_C_STD_LIBS | A list of C standard library directories, separated by ':' |
| HAB_C_STD_HEADERS	| A list of C standard header directories, separated by ':' |
| HAB_CXX_STD_LIBS | A list of C++ standard library directories, separated by ':' |
| HAB_CXX_STD_HEADERS | A list of C++ standard header directories, separated by ':' |

By configuring these environment variables, you can easily customize the behavior of the `hab-cc-wrapper` to suit your habitat package's requirements.

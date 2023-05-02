use std::path::PathBuf;

pub struct CommonEnvironment {
    pub debug_log_file: Option<PathBuf>,
    pub is_debug: bool,
    pub fs_root: PathBuf,
    pub cwd: PathBuf,
}
impl Default for CommonEnvironment {
    fn default() -> Self {
        Self {
            debug_log_file: std::env::var("HAB_DEBUG_LOG_FILE").map(PathBuf::from).ok(),
            is_debug: std::env::var("HAB_DEBUG")
                .map(|value| value == "1")
                .unwrap_or(false),
            fs_root: PathBuf::from("/"),
            cwd: std::env::current_dir().unwrap(),
        }
    }
}

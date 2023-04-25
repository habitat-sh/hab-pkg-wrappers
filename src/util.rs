use std::path::{Path, PathBuf};

use path_absolutize::Absolutize;

use crate::env::CommonEnvironment;

pub trait PrefixedArg {
    fn is_prefixed_with(&self, prefix: &str) -> bool;
}

impl<T> PrefixedArg for T
where
    T: AsRef<str>,
{
    fn is_prefixed_with(&self, prefix: &str) -> bool {
        self.as_ref()
            .strip_prefix(prefix)
            .map_or(false, |value| !value.is_empty())
    }
}

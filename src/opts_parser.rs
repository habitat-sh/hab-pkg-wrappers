use std::{
    fs::File,
    io::{BufRead, BufReader, Read},
    path::PathBuf,
};

use path_absolutize::Absolutize;

use crate::{env::CommonEnvironment, util::PrefixedArg};

#[derive(Debug, Clone, PartialEq, Eq)]
enum State {
    Outside,
    Unquoted,
    UnquotedEscape,
    SingleQuote,
    SingleQuoteEscape,
    DoubleQuote,
    DoubleQuoteEscape,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum CharClass {
    Space,
    Other,
    Backslash,
    Apostrophe,
    QuotationMark,
}

impl CharClass {
    fn from(char: &char) -> CharClass {
        match char {
            c if c.is_ascii_whitespace() => CharClass::Space,
            '\n' => CharClass::Space,
            '\\' => CharClass::Backslash,
            '\'' => CharClass::Apostrophe,
            '"' => CharClass::QuotationMark,
            _ => CharClass::Other,
        }
    }
}
impl State {
    fn next(&self, next_char: CharClass) -> State {
        match (&self, next_char) {
            (State::Outside, CharClass::Space) => State::Outside,
            (State::Outside, CharClass::Other) => State::Unquoted,
            (State::Outside, CharClass::Backslash) => State::UnquotedEscape,
            (State::Outside, CharClass::Apostrophe) => State::SingleQuote,
            (State::Outside, CharClass::QuotationMark) => State::DoubleQuote,
            (State::Unquoted, CharClass::Space) => State::Outside,
            (State::Unquoted, CharClass::Other) => State::Unquoted,
            (State::Unquoted, CharClass::Backslash) => State::UnquotedEscape,
            (State::Unquoted, CharClass::Apostrophe) => State::SingleQuote,
            (State::Unquoted, CharClass::QuotationMark) => State::DoubleQuote,
            (State::UnquotedEscape, CharClass::Space) => State::Unquoted,
            (State::UnquotedEscape, CharClass::Other) => State::Unquoted,
            (State::UnquotedEscape, CharClass::Backslash) => State::Unquoted,
            (State::UnquotedEscape, CharClass::Apostrophe) => State::Unquoted,
            (State::UnquotedEscape, CharClass::QuotationMark) => State::Unquoted,
            (State::SingleQuote, CharClass::Space) => State::SingleQuote,
            (State::SingleQuote, CharClass::Other) => State::SingleQuote,
            (State::SingleQuote, CharClass::Backslash) => State::SingleQuoteEscape,
            (State::SingleQuote, CharClass::Apostrophe) => State::Unquoted,
            (State::SingleQuote, CharClass::QuotationMark) => State::SingleQuote,
            (State::SingleQuoteEscape, CharClass::Space) => State::SingleQuote,
            (State::SingleQuoteEscape, CharClass::Other) => State::SingleQuote,
            (State::SingleQuoteEscape, CharClass::Backslash) => State::SingleQuote,
            (State::SingleQuoteEscape, CharClass::Apostrophe) => State::SingleQuote,
            (State::SingleQuoteEscape, CharClass::QuotationMark) => State::SingleQuote,
            (State::DoubleQuote, CharClass::Space) => State::DoubleQuote,
            (State::DoubleQuote, CharClass::Other) => State::DoubleQuote,
            (State::DoubleQuote, CharClass::Backslash) => State::DoubleQuoteEscape,
            (State::DoubleQuote, CharClass::Apostrophe) => State::DoubleQuote,
            (State::DoubleQuote, CharClass::QuotationMark) => State::Unquoted,
            (State::DoubleQuoteEscape, CharClass::Space) => State::DoubleQuote,
            (State::DoubleQuoteEscape, CharClass::Other) => State::DoubleQuote,
            (State::DoubleQuoteEscape, CharClass::Backslash) => State::DoubleQuote,
            (State::DoubleQuoteEscape, CharClass::Apostrophe) => State::DoubleQuote,
            (State::DoubleQuoteEscape, CharClass::QuotationMark) => State::DoubleQuote,
        }
    }
}

pub fn expand_argument(argument: &str, env: &CommonEnvironment) -> Vec<String> {
    if !argument.is_prefixed_with("@") {
        return vec![argument.to_string()];
    }
    let file = PathBuf::from(argument.strip_prefix("@").unwrap())
        .absolutize_from(&env.cwd)
        .unwrap()
        .to_path_buf();
    if !file.is_file() {
        return vec![argument.to_string()];
    }
    let mut contents = String::new();
    File::open(file)
        .unwrap()
        .read_to_string(&mut contents)
        .unwrap();
    let mut expanded_arguments = Vec::new();
    let mut current_argument = Vec::new();
    let mut current_state = State::Outside;
    for char in contents.chars() {
        let char_class = CharClass::from(&char);
        let next_state = current_state.next(char_class);
        if current_state == State::Unquoted && next_state == State::Outside {
            let final_argument = current_argument.clone().into_iter().collect::<String>();
            current_argument.truncate(0);
            let final_expanded_argument = expand_argument(&final_argument, env);
            expanded_arguments.extend(final_expanded_argument.into_iter());
        } else if current_state == State::UnquotedEscape
            || current_state == State::SingleQuoteEscape
            || current_state == State::DoubleQuoteEscape
            || (current_state == State::Outside && next_state == State::Unquoted)
            || (current_state != State::Outside && current_state == next_state)
        {
            current_argument.push(char);
        }
        current_state = next_state;
    }
    if current_state != State::Outside {
        let final_argument = current_argument.clone().into_iter().collect::<String>();
        let final_expanded_argument = expand_argument(&final_argument, env);
        expanded_arguments.extend(final_expanded_argument.into_iter());
    }
    expanded_arguments
}

#[cfg(test)]
mod test {
    use crate::env::CommonEnvironment;

    use super::expand_argument;

    #[test]
    fn basic_parsing() {
        let tmp_dir = tempdir::TempDir::new("opt-parser-test").unwrap();
        std::fs::write(
            tmp_dir.path().join("input"),
            "aaa 'bbb' \"ccc\" a\\ aa 'bb\\'b' \"cc\\\"c\"",
        )
        .unwrap();
        let env = CommonEnvironment {
            cwd: tmp_dir.path().to_path_buf(),
            ..Default::default()
        };
        let result = expand_argument("@input", &env);
        assert_eq!(&result, &["aaa", "bbb", "ccc", "a aa", "bb'b", "cc\"c"]);
    }

    #[test]
    fn recursive_parsing() {
        let tmp_dir = tempdir::TempDir::new("opt-parser-test").unwrap();
        std::fs::write(
            tmp_dir.path().join("input"),
            "aaa 'bbb' \"ccc\" a\\ aa @input2 'bb\\'b' \"cc\\\"c\"",
        )
        .unwrap();
        std::fs::write(
            tmp_dir.path().join("input2"),
            "xxx 'yyy' \"zzz\" x\\ xx 'yy\\'y' \"zz\\\"z\"",
        )
        .unwrap();
        let env = CommonEnvironment {
            cwd: tmp_dir.path().to_path_buf(),
            ..Default::default()
        };
        let result = expand_argument("@input", &env);
        assert_eq!(
            &result,
            &[
                "aaa", "bbb", "ccc", "a aa", "xxx", "yyy", "zzz", "x xx", "yy'y", "zz\"z", "bb'b",
                "cc\"c"
            ]
        );
    }

    #[test]
    fn recursive_parsing_of_non_existent_file() {
        let tmp_dir = tempdir::TempDir::new("opt-parser-test").unwrap();
        std::fs::write(
            tmp_dir.path().join("input"),
            "aaa 'bbb' \"ccc\" a\\ aa @input2 'bb\\'b' \"cc\\\"c\"",
        )
        .unwrap();
        let env = CommonEnvironment {
            cwd: tmp_dir.path().to_path_buf(),
            ..Default::default()
        };
        let result = expand_argument("@input", &env);
        assert_eq!(
            &result,
            &["aaa", "bbb", "ccc", "a aa", "@input2", "bb'b", "cc\"c"]
        );
    }
}

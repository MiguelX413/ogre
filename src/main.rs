pub fn is_valid_symbol(c: char) -> bool {
    matches!(c, '+' | '-' | '*' | '/' | '\\')
}

pub fn token_bound(token: &str) -> Option<usize> {
    if token.starts_with(|f: char| f.is_numeric()) {
        return Some(
            token
                .char_indices()
                .find(|(_, f)| !f.is_numeric())
                .map(|(i, _)| i)
                .unwrap_or(token.len()),
        );
    }
    if token.starts_with(|f: char| f.is_alphabetic()) {
        return Some(
            token
                .char_indices()
                .find(|(_, f)| !f.is_alphanumeric())
                .map(|(i, _)| i)
                .unwrap_or(token.len()),
        );
    }
    if token.starts_with(is_valid_symbol) {
        return Some(
            token
                .char_indices()
                .find(|(_, f)| !is_valid_symbol(*f))
                .map(|(i, _)| i)
                .unwrap_or(token.len()),
        );
    }
    None
}

pub fn split_first_token(token: &str) -> Option<(&str, &str)> {
    let trimmed = token.trim();
    Some(trimmed.split_at(token_bound(trimmed)?))
}

struct SplitTokens<'a> {
    remainder: &'a str,
}

impl<'a> SplitTokens<'a> {
    pub fn new(string: &str) -> SplitTokens {
        SplitTokens { remainder: string }
    }
}

impl<'a> Iterator for SplitTokens<'a> {
    type Item = Result<&'a str, &'a str>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.remainder.is_empty() {
            return None;
        }
        let Some((token, remainder)) = split_first_token(self.remainder) else {
            return Some(Err(self.remainder));
        };
        self.remainder = remainder;
        Some(Ok(token))
    }
}

pub fn tokenize(string: &str) -> Result<Vec<&str>, &str> {
    SplitTokens::new(string).collect()
}

fn main() {
    [
        "catfood-45",
        "catfood",
        "67z23",
        "&catfood-45",
        "catfood&-45",
        "catfood-45&",
        "&",
        " catfood -45 67z23",
    ]
    .into_iter()
    .for_each(|string| println!("{string}: {:?}", tokenize(string)));
}

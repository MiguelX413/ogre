pub fn is_valid_token(token: &str) -> bool {
    !token.is_empty()
        && ((token.starts_with(|f: char| f.is_numeric()) && token.chars().all(|f| f.is_numeric()))
            || (token.starts_with(|f: char| f.is_alphabetic())
                && token.chars().all(|f| f.is_alphanumeric()))
            || (token
                .chars()
                .all(|f| matches!(f, '+' | '-' | '*' | '/' | '\\'))))
}

pub fn tokenize<'a>(string: &'a str, mut vec: Vec<&'a str>) -> Result<Vec<&'a str>, &'a str> {
    if is_valid_token(string) {
        vec.push(string);
        return Ok(vec);
    }
    let bound = (0..=string.len())
        .rev()
        .find(|&bound| {
            if let Some(substr) = string.get(0..bound) {
                return is_valid_token(substr);
            }
            false
        })
        .ok_or(string)?;
    let (token, remainder) = string.split_at(bound);
    vec.push(token);
    tokenize(remainder, vec)
}

fn main() {
    [
        "catfood-45",
        "67z23",
        "&catfood-45",
        "catfood&-45",
        "catfood-45&",
    ]
    .into_iter()
    .for_each(|string| println!("{string}: {:?}", tokenize(string, vec![])));
}

pub fn is_valid_token(token: &str) -> bool {
    !token.is_empty()
        && ((token.starts_with(|f: char| f.is_numeric()) && token.chars().all(|f| f.is_numeric()))
            || (token.starts_with(|f: char| f.is_alphabetic())
                && token.chars().all(|f| f.is_alphanumeric()))
            || token
                .chars()
                .all(|f| matches!(f, '+' | '-' | '*' | '/' | '\\')))
}

pub fn tokenize<'a>(string: &'a str, mut vec: Vec<&'a str>) -> Result<Vec<&'a str>, &'a str> {
    if is_valid_token(string) {
        vec.push(string);
        return Ok(vec);
    }
    let bound = (1..=string.len())
        .rev()
        .find(|&bound| string.get(0..bound).map_or(false, is_valid_token))
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

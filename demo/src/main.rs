use tokenizer::split_tokens;

pub fn main() {
    [
        "catfood-45",
        "catfood",
        "67z23",
        "catfood&-45",
        "&",
        " -45 - 45 + +45",
        "if +2 + -2 else x := x - 5 ",
        "if {{10 / {45 + 3}} + {2 * 4}} - +5",
        "日本語a+123",
        "cat- 324_32432432432-ref",
        "{2133 ** 21} % 2",
        r#"let my_string := "lol\"test";
let xd: Int := 2;
let multi_line_str := "xd\
sus";"#,
        "let _ := 5;",
        "34_2 432.2_34 234.count_ones() 3424.",
        "240",
    ]
    .into_iter()
    .for_each(|string| {
        println!(
            "{string:?}: {:?}",
            split_tokens(string).collect::<Result<Vec<_>, _>>()
        )
    });
}

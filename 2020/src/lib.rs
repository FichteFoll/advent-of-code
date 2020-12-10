
pub fn read_file(day: usize) -> String {
    std::fs::read_to_string(
        std::env::args()
            .skip(1)
            .find(|s| !s.starts_with("--"))
            .unwrap_or(format!("input/day{:0>2}.txt", day)),
    )
    .unwrap()
}

// requires DAY
#[macro_export]
macro_rules! read_input {
    () => {
        read_file(DAY)
    };
}

// requires TEST_INPUT
#[macro_export]
macro_rules! test {
    ($part: ident ($($param: expr),*) == $expected:expr) => {
        paste::paste! {
            #[test]
            fn [<test_ $part>]() {
                let input = parse_input(TEST_INPUT_STR);
                assert_eq!($part(&input$(, $param)*), $expected);
            }
        }
    };
}

#[macro_export]
macro_rules! bench {
    ($part: ident ($($param: expr),*) == $expected:expr) => {
        paste::paste! {
            #[bench]
            fn [<bench_ $part>](b: &mut test::Bencher) {
                let input = parse_input(&read_input!());
                b.iter(|| assert_eq!($part(test::black_box(&input) $(, $param)* ), $expected));
            }
        }
    };
}

#[macro_export]
macro_rules! bench_parse {
    ($fn:ident, $expected_len:expr) => {
        #[bench]
        fn bench_parse(b: &mut test::Bencher) {
            let raw = read_input!();
            b.iter(|| assert_eq!(parse_input(test::black_box(&raw)).$fn(), $expected_len));
        }
    };
}

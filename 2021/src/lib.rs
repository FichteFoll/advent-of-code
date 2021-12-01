
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

// requires TEST_INPUT & parse_input
#[macro_export]
macro_rules! test {
    ($part: ident ($($param: expr),*) == $expected:expr) => {
        paste::paste! {
            #[test]
            fn [<test_ $part>]() {
                let parsed = parse_input(TEST_INPUT);
                assert_eq!($part(&parsed$(, $param)*), $expected);
            }
        }
    };
    ($input: expr, $part: ident ($($param: expr),*) == $expected:expr) => {
        paste::paste! {
            #[test]
            fn [<test_ $part>]() {
                let parsed = parse_input($parsed);
                assert_eq!($part(&parsed$(, $param)*), $expected);
            }
        }
    };
    ($suffix: ident, $input: expr, $part: ident ($($param: expr),*) == $expected:expr) => {
        paste::paste! {
            #[test]
            fn [<test_ $part _ $suffix>]() {
                let parsed = parse_input($parsed);
                assert_eq!($part(&parsed$(, $param)*), $expected);
            }
        }
    };
}

// requires parse_input
#[macro_export]
macro_rules! bench {
    ($part: ident ($($param: expr),*) == $expected:expr) => {
        paste::paste! {
            #[bench]
            fn [<bench_ $part>](b: &mut test::Bencher) {
                let input = read_input!();
                let parsed = parse_input(&input);
                b.iter(|| assert_eq!($part(test::black_box(&parsed) $(, $param)* ), $expected));
            }
        }
    };
}

// requires parse_input
#[macro_export]
macro_rules! bench_parse {
    ($fn:ident, $expected_len:expr) => {
        #[bench]
        fn bench_parse(b: &mut test::Bencher) {
            let raw = read_input!();
            b.iter(|| assert_eq!(parse_input(test::black_box(&raw)).$fn(), $expected_len));
        }
    };
    () => {
        #[bench]
        fn bench_parse(b: &mut test::Bencher) {
            let raw = read_input!();
            b.iter(|| {
                let _ = parse_input(test::black_box(&raw));
            });
        }
    };
}


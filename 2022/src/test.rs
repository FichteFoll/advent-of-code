// requires `TEST_INPUT` & `parse_input`
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
                let parsed = parse_input($input);
                assert_eq!($part(&parsed$(, $param)*), $expected);
            }
        }
    };
    ($suffix: ident, $input: expr, $part: ident ($($param: expr),*) == $expected:expr) => {
        paste::paste! {
            #[test]
            fn [<test_ $part _ $suffix>]() {
                let parsed = parse_input($input);
                assert_eq!($part(&parsed$(, $param)*), $expected);
            }
        }
    };
}

// requires `DAY` & `parse_input`
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

// requires `DAY` & `parse_input`
#[macro_export]
macro_rules! bench_parse {
    ($fn:expr, $expected:expr) => {
        #[bench]
        fn bench_parse(b: &mut test::Bencher) {
            let raw = read_input!();
            b.iter(|| {
                let parsed = parse_input(test::black_box(&raw));
                assert_eq!($fn(&parsed), $expected)
            });
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


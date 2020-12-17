extern crate impl_ops;

pub mod grid2d;

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

// requires TEST_INPUT_STR & parse_input
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
    ($input: expr, $part: ident ($($param: expr),*) == $expected:expr) => {
        paste::paste! {
            #[test]
            fn [<test_ $part>]() {
                let input = parse_input($input);
                assert_eq!($part(&input$(, $param)*), $expected);
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
                let input_str = read_input!();
                let input = parse_input(&read_input!());
                b.iter(|| assert_eq!($part(test::black_box(&input) $(, $param)* ), $expected));
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

// Given a slice of type T, return a Vec containing the powerset,
// i.e. the set of all subsets.
//
// This works by treating each int the range [0, 2**n) (where n is the
// length of the slice) as a bitmask, selecting only the members of
// the original slice whose corresponding positional bits are flipped
// on in each mask.
pub fn powerset<T: Clone>(slice: &[T]) -> Vec<Vec<T>> {
    let mut v: Vec<Vec<T>> = Vec::new();

    for mask in 0..(1 << slice.len()) {
        let mut ss: Vec<T> = vec![];
        let mut bitset = mask;
        while bitset > 0 {
            // isolate the rightmost bit to select one item
            let rightmost: u64 = bitset & !(bitset - 1);
            // turn the isolated bit into an array index
            let idx = rightmost.trailing_zeros();
            let item = (*slice.get(idx as usize).unwrap()).clone();
            ss.push(item);
            // zero the trailing bit
            bitset &= bitset - 1;
        }
        v.push(ss);
    }
    v
}

// fn rotate(list: &mut Iterator<Item=usize>, skip: usize, num: usize) {
use std::cell::Cell;

//     skip += 1;
// }

fn main() {
    const LIST_SIZE: usize = 256;
    let input: [usize; 16] = [189,1,111,246,254,2,0,120,215,93,255,50,84,15,94,62];
    // const LIST_SIZE: usize = 5;
    // let input: [usize; 4] = [3, 4, 1, 5];

    let list: Vec<Cell<usize>> = (0..LIST_SIZE).map(Cell::new).collect();

    let mut skip: usize = 0;
    let mut list_iter = list.iter().cycle();
    for n in input.iter() {
        let section: Vec<_> = list_iter.clone().take(*n).collect();
        // println!("section: {:?}", section);

        for (i, cell1) in section.iter().rev().enumerate() {
            if let Some(cell2) = list_iter.next() {
                if i < section.len() / 2 {
                    // println!("swapping {:?} with {:?}", cell1, cell2);
                    cell1.swap(&cell2);
                }
            }
            else {
                panic!("This should never happen");
            }
        }
        // list_iter = list_iter.skip(skip);  // cannot use this because it returns a Skip type
        // println!("skipping {}", skip);
        for _ in 0..skip {
            list_iter.next();
        }
        skip += 1;
    }

    println!("final list: {:?}", list);
    println!("product: {}", list[0].get() * list[1].get());
}

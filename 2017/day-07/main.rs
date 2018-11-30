#![feature(refcell_replace_swap)]

use std::fs::File;
use std::io::prelude::*;
use std::collections::HashMap;
use std::rc::{Rc, Weak};
use std::cell::{Ref, RefCell};
use std::fmt;


fn get_input() -> String {
    let input_filename = "input.txt";
    let mut f = File::open(input_filename).expect("file not found");
    let mut contents = String::new();
    f.read_to_string(&mut contents).expect("something went wrong reading the file");
    contents
}


#[derive (Debug)]
struct Tower {
    name: String,
    weight: usize,
    subtowers: RefCell<Vec<Weak<Tower>>>,
    supertower: RefCell<Option<Weak<Tower>>>,
}

impl Tower {
    fn subtower_names(&self) -> Vec<String> {
        let subtowers: Ref<Vec<Weak<Tower>>> = self.subtowers.borrow();
        subtowers.iter().map(|x| x.upgrade().unwrap().name.clone()).collect()
        // subtower_names
    }
    fn supertower_name(&self) -> Option<String> {
        let supertower: Ref<Option<Weak<Tower>>> = self.supertower.borrow();
        match *supertower {
            Some(ref tower) => Some(tower.upgrade().unwrap().name.clone()),
            None => None
        }
    }
}

impl fmt::Display for Tower {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} ({}) [{:?}] -> {:?}", self.name, self.weight, self.supertower_name(), self.subtower_names())
    }
}


fn parse_input(input: &str) -> HashMap<String, Rc<Tower>> {
    let mut tower_map: HashMap<String, Rc<Tower>> = HashMap::new();
    let weight_trims: &[_] = &['(', ')'];

    let mut associations: Vec<(&str, Vec<&str>)> = Vec::new();
    for line in input.split("\n") {
        // parse the tower struct
        let mut tokens = line.split(" ");
        let name_str = tokens.next().expect("No name found");
        let name = name_str.to_string();
        let weight_str = tokens.next().expect("No weight found").trim_matches(weight_trims);
        let weight: usize = weight_str.parse().expect("Invalid weigth");
        let tower = Tower{
            name: name.clone(),
            weight,
            subtowers: RefCell::new(Vec::new()),
            supertower: RefCell::new(None),
        };
        tower_map.insert(name, Rc::new(tower));

        // parse association part
        match tokens.next() {
            Some("->") => {associations.push((name_str, tokens.map(|x| x.trim_right_matches(',')).collect()));},
            Some(_) => panic!("Unexpected token"),
            None => (),
        };
    }
    // println!("associations: {:?}", associations);

    // set associations in structs
    for (supertower_name, subtower_names) in associations {
        let supertower = &tower_map[supertower_name];
        let weak_supertower = Rc::downgrade(supertower);

        let mut weak_subtowers: Vec<Weak<Tower>> = Vec::new();
        for subtower_name in subtower_names {
            let subtower = &tower_map[subtower_name];
            subtower.supertower.replace(Some(weak_supertower.clone()));
            weak_subtowers.push(Rc::downgrade(&subtower));
        }
        let mut super_subtowers = supertower.subtowers.borrow_mut();
        super_subtowers.extend(weak_subtowers);
    }
    tower_map
}

fn main() {
    let input_string = get_input();
    let input = input_string.trim();

    let tower_map = parse_input(input);

    // debugging
    for tower in tower_map.values() {
        println!("{}", tower);
    }
    println!("tower_map length: {}", tower_map.len());

    // Find root by traversing any tower's supertower until there is no more.
    // We could also iterate over all towers and pick the one with no supertower,
    // but this is obviously faster.
    let mut root: Rc<Tower> = Rc::clone(tower_map.values().next().unwrap());
    loop {
        let next_root: Rc<Tower>;
        {
            let supertower = root.supertower.borrow();
            next_root = match *supertower {
                Some(ref weak_tower) => weak_tower.upgrade().unwrap(),
                None => break,
            };
        }
        root = Rc::clone(&next_root);
    }

    println!("root node: {}", root.name);
}

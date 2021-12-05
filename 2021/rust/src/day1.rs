use std;
use std::fs;

fn read_file (filename: &str) -> Vec<u32> {
    let content = fs::read_to_string(&filename).unwrap();
    let lines = content.lines();
    let numbers = lines.map(|x| x.parse::<u32>().unwrap()).collect::<Vec<u32>>();
    return numbers;
}


pub fn puzzle1() {
    let numbers = read_file("../Input1.txt");
    let res = numbers.windows(2).filter(|x| x[0] < x[1]).count();
    println!("Day1::Puzzle1: {}", res);
}

pub fn puzzle2() {
    let numbers = read_file("../Input1.txt");
    let res = numbers.windows(3).map(|x| x[0] + x[1] + x[2]).collect::<Vec<u32>>()
                     .windows(2).filter(|x| x[0] < x[1]).count();
    println!("Day1::Puzzle2: {}", res);
}

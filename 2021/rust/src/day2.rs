use std;
use std::fs;
use regex::Regex;


enum Move {
    Forward(u32),
    Up(u32),
    Down(u32)
}



fn to_move (m: &str) -> Move {
    let items: Vec<&str> = Regex::new(r"(?P<move>\w) (?P<qty>\d+)").unwrap().captures_iter(m).map(|x| x.as_str()).collect();
    println!("{:?}", items);
    return match items[..] {
        ["forward", x] => Move::Forward(x.parse::<u32>().unwrap()),
        ["up", x] => Move::Up(x.parse::<u32>().unwrap()),
        ["down", x] => Move::Down(x.parse::<u32>().unwrap()),
        _ => unreachable!()
    }
}


fn read_file (filename: &str) -> Vec<Move> {
    let content = fs::read_to_string(&filename).unwrap();
    let moves = content.lines().map(|x| to_move(x)).collect::<Vec<Move>>();
    return moves;
}



pub fn puzzle1() {
    let moves = read_file("../Test2.txt");

    fn do_move (pos: u32, depth: u32, m: &Move) -> (u32, u32) {
        return match m {
                Move::Forward(x) => (pos+x, depth),
                Move::Up(x) => (pos, depth-x),
                Move::Down(x) => (pos, depth+x)
            }
    }

    let (pos, depth) = moves.iter().fold((0,0), |(p,d),m| do_move(p, d, m));
    let res = pos * depth;
    println!("Day2::Puzzle1: {}", res);
}
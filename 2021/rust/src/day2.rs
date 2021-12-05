use std;
use std::fs;


enum Move {
    Forward(u32),
    Up(u32),
    Down(u32)
}


fn to_move (m: &str) -> Move {
    let items = m.split(' ').collect::<Vec<&str>>();
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
    let moves = read_file("../Input2.txt");

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

pub fn puzzle2() {
    let moves = read_file("../Input2.txt");

    fn do_move (aim: u32, pos: u32, depth: u32, m: &Move) -> (u32, u32, u32) {
        return match m {
                Move::Forward(x) => (aim, pos+x, depth + aim*x),
                Move::Up(x) => (aim-x, pos, depth),
                Move::Down(x) => (aim+x, pos, depth)
            }
    }

    let (_, pos, depth) = moves.iter().fold((0,0,0), |(a,p,d),m| do_move(a,p, d, m));
    let res = pos * depth;
    println!("Day2::Puzzle2: {}", res);
}

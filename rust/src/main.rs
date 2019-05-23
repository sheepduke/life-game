extern crate rand;

use std::env;
use std::process;

use rand::Rng;
use std::fmt::Display;
use std::fmt::Error;
use std::fmt::Formatter;

struct Board {
    data: Vec<Vec<bool>>,
    size: usize,
}

impl Board {
    pub fn new(size: usize) -> Self {
        let mut board = Self {
            data: Vec::new(),
            size,
        };
        for _ in 0..size {
            let mut row = Vec::new();
            row.resize(size, false);
            board.data.push(row)
        }
        board
    }

    pub fn place_seeds(&mut self, count: usize) {
        let limit = self.size.pow(2);
        let mut rng = rand::thread_rng();
        let mut count = count;

        while count > 0 {
            let random_number = rng.gen_range(0, limit);
            let row = random_number / self.size;
            let col = random_number % self.size;
            if !self.get(row, col) {
                self.set(row, col, true);
                count -= 1;
            }
        }
    }

    fn get(&self, row: usize, col: usize) -> bool {
        self.data[row][col]
    }

    fn set(&mut self, row: usize, col: usize, value: bool) {
        self.data[row][col] = value
    }

    pub fn next_cycle(&self) -> Self {
        let mut new_board = Board::new(self.size);
        for row in 0..self.size {
            for col in 0..self.size {
                new_board.set(row, col, self.will_survive(row, col))
            }
        }
        new_board
    }

    fn will_survive(&self, row: usize, col: usize) -> bool {
        let neighbour_count = self.alive_neighbour_count(row, col);
        neighbour_count == 3 || (neighbour_count == 2 && self.get(row, col))
    }

    fn alive_neighbour_count(&self, row: usize, col: usize) -> usize {
        let mut count = 0usize;

        let row_min = if row > 0 { row - 1 } else { 0 };
        let row_max = if row < self.size - 1 { row + 1 } else { row };
        let col_min = if col > 0 { col - 1 } else { 0 };
        let col_max = if col < self.size - 1 { col + 1 } else { col };

        for i in row_min..=row_max {
            for j in col_min..=col_max {
                if self.get(i, j) {
                    count += 1
                }
            }
        }
        count
    }
}

impl Display for Board {
    fn fmt(&self, _formatter: &mut Formatter) -> Result<(), Error> {
        for row in &self.data {
            for cell in row {
                print!("{} ", if *cell { "#" } else { "-" })
            }
            println!();
        }
        Ok(())
    }
}

fn main() {
    let args: Vec<_> = env::args().collect();
    if args.len() != 3 {
        println!("Usage: {} BOARD_SIZE INIT_SEED", args[0]);
        process::exit(1);
    }

    let board_size: usize = args[1].parse().unwrap();
    let seeds_count: usize = args[2].parse().unwrap();

    let mut board = Board::new(board_size);
    board.place_seeds(seeds_count);

    let mut cycle = 100_000;
    while cycle > 0 {
        board = board.next_cycle();
        cycle -= 1;
    }
}

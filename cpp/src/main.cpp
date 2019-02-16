#include <iostream>
#include <sstream>
#include <thread>
#include <chrono>
#include "board.hpp"

using namespace std;
using namespace life_game;

int main(int argc, char *argv[]) {
  if (argc < 2) {
    cout << "Usage: " << argv[0] << " SIZE INIT_COUNT" << endl;
    exit(1);
  }

  stringstream ss;
  ss << argv[1] << " " << argv[2];

  size_t size;
  size_t random_count;

  ss >> size >> random_count;
  
  Board *board = new Board(size);
  board->place_random_seeds(random_count);
  // board->print();

  for (auto i = 0 ; i < 100000; i++) {
    Board *new_board = new Board(size);
    board->evolve_to(*new_board);
    delete board;
    board = new_board;
    // cout << "Iteration " << i << " " << endl;
    // board->print();
    // this_thread::sleep_for(chrono::milliseconds(500));
  }

  delete board;

  return 0;
}

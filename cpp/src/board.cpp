#include "board.hpp"

#include <random>

#include <iostream>
using namespace std;

namespace life_game {

  Board::Board(size_t size) {
    this->size = size;
    board = new bool*[size];
    for (size_t i = 0; i < size; i++) {
      board[i] = new bool[size];
      for (size_t j = 0; j < size; j++) {
        board[i][j] = false;
      }
    }
  }

  void Board::place_random_seeds(size_t seed_count) {
    random_device rd;
    mt19937 gen(rd());
    uniform_int_distribution<> dis(0, size * size - 1);

    while (seed_count > 0) {
      auto random = dis(gen);
      auto row = random / size;
      auto col = random % size;
      if (!board[row][col]) {
        board[row][col] = true;
        seed_count--;
      }
    }
  }

  void Board::evolve_to(Board &board) {
    for (size_t row = 0; row < size; row++) {
      for (size_t col = 0; col < size; col++) {
        board.board[row][col] = is_alive(row, col);
      }
    }
  }

  void Board::print() const {
    for (size_t row = 0; row < size; row++) {
      for (size_t col = 0; col < size; col++) {
        cout << (board[row][col] ? "# " : "- ");
      }
      cout << endl;
    }
  }

  bool Board::is_alive(size_t row, size_t col) const {
    auto count = living_neighbour_count(row, col);
    return count == 3 || (count == 2 && board[row][col]);
  }

  size_t Board::living_neighbour_count(size_t row, size_t col) const {
    size_t count = 0;
    
    size_t row_start = row > 0 ? row - 1 : row;
    size_t row_end = row < size - 1 ? row + 1 : row;
    size_t col_start = col > 0 ? col - 1 : col;
    size_t col_end = col < size - 1 ? col + 1 : col;
    

    for (auto i = row_start; i <= row_end; i++) {
      for (auto j = col_start; j <= col_end; j++) {
        if (board[i][j]) {
          count++;
        }
      }
    }

    if (board[row][col]) {
      count--;
    }
    return count;
  }
}

#ifndef BOARD_HPP
#define BOARD_HPP

#include <vector>

namespace life_game {
  class Board {
  public:
    Board(size_t size);
    void place_random_seeds(size_t seed_count);
    void evolve_to(Board &board);
    void print() const;
    
  private:
    bool is_alive(size_t row, size_t col) const;
    size_t living_neighbour_count(size_t row, size_t col) const;

    size_t size;

    bool **board;
  };
}

#endif

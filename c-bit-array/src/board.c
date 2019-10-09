#include "board.h"
#include "bit_array.h"

#include <stdlib.h>
#include <stdio.h>
#include <time.h>

struct board {
  bit_array *rows;
  unsigned int size;
};

board board_new(size_t size) {
  board board = malloc(sizeof(struct board));
  board->size = size;
  board->rows = malloc(sizeof(bit_array) * size);
  for (unsigned int i = 0; i < size; i++) {
    board->rows[i] = bit_array_new(size);
  }

  return board;
}

void board_destroy(board this) {
  for (int i = 0; i < this->size; i++) {
    bit_array_destroy(this->rows[i]);
  }
  free(this->rows);
  free(this);
}

void board_print(board this) {
  for (size_t row = 0; row < this->size; row++) {
    for (size_t col = 0; col < this->size; col++) {
      printf(bit_array_get(this->rows[row], col) ? "# " : "- ");
    }
    printf("\n");
  }
}

static inline uint8_t board_get(board this, size_t row, size_t col) {
  return bit_array_get(this->rows[row], col);
}

static inline void board_set(board this, size_t row, size_t col,
                                uint8_t value) {
  bit_array_set(this->rows[row], col, value);
}

void board_seed_randomly(board this, size_t count) {
  srand(time(NULL));
  size_t max_random = this->size * this->size;
  while (count > 0) {
    size_t random = rand() % max_random;
    size_t row = random / this->size;
    size_t col = random % this->size;
    if (!board_get(this, row, col)) {
      board_set(this, row, col, 1);
      count--;
    }
  }
}

static inline size_t living_neighbour_count(board this, size_t row, size_t col) {
  size_t count = 0;
    
  size_t row_start = row > 0 ? row - 1 : row;
  size_t row_end = row < this->size - 1 ? row + 1 : row;
  size_t col_start = col > 0 ? col - 1 : col;
  size_t col_end = col < this->size - 1 ? col + 1 : col;
    

  for (size_t row = row_start; row <= row_end; row++) {
    for (size_t col = col_start; col <= col_end; col++) {
      if (board_get(this, row, col)) {
        count++;
      }
    }
  }

  if (board_get(this, row, col)) {
    count--;
  }
  return count;
}

static inline uint8_t is_alive(board this, size_t row, size_t col) {
  size_t count = living_neighbour_count(this, row, col);
  return count == 3 || (count == 2 && board_get(this, row, col));
}

void board_evolve(board new_board, board old_board) {
  for (size_t row = 0; row < old_board->size; row++) {
    for (size_t col = 0; col < old_board->size; col++) {
      board_set(new_board, row, col, is_alive(old_board, row, col));
    }
  }
}

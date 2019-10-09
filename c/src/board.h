#ifndef BOARD_H
#define BOARD_H

#include <stdlib.h>

typedef struct board *board;

board board_new(size_t size);

void board_destroy(board this);

void board_seed_randomly(board this, size_t count);

void board_print(board this);

void board_evolve(board new_board, board old_board);

#endif

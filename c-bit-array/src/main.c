#include "board.h"

#include <stdio.h>
#include <unistd.h>

int main(int argc, char *argv[]) {
  if (argc < 2) {
    printf("Usage: %s SIZE INIT_COUNT", argv[0]);
    exit(1);
  }

  size_t size = atoi(argv[1]);
  size_t random_count = atoi(argv[2]);

  board current_board = board_new(size);
  board_seed_randomly(current_board, random_count);

  for (int i = 0 ; i < 100000; i++) {
    board new_board = board_new(size);
    board_evolve(new_board, current_board);
    board_destroy(current_board);
    current_board = new_board;
    
    // printf("Iteration %d\n", i);
    // board_print(current_board);
    // sleep(0.5);
  }

  board_destroy(current_board);

  return 0;
}

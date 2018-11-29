#!/usr/bin/env julia
#
module lifegame

function makeboard(size, seedcount)
  board = falses(size, size)
  foreach(i -> board[i] = true, rand(1:size*size, seedcount))
  board
end

"Calculate the number of neighbors of a cell."
function neighborcount(board, row, col)
  (boardsize, _) = size(board)

  getmin(x) = x > 1 ? x - 1 : 1
  getmax(x) = x < boardsize ? x + 1 : boardsize

  c = count(board[getmin(row):getmax(row), getmin(col):getmax(col)])
  if board[row, col]
    c - 1
  else
    c
  end
end

"Return a boolean value indicating if given cell will be alive."
function isalive(board, row, col)
  nc = neighborcount(board, row, col)

  nc == 3 || (board[row, col] && nc == 2)
end

function evolve(board)
  (boardsize, _) = size(board)
  [isalive(board, row, col) for row in 1:boardsize, col in 1:boardsize]
end

"Print the board to `io`, which defaults to `stdout`."
function printboard(board, io=stdout)
  (boardsize, _) = size(board)

  for row in 1:boardsize, col in 1:boardsize
    print(io, board[row, col] ? "* " : "- ")
    if col == boardsize
      println(io, "")
    end
  end
end

function main()
  length(ARGS) < 2 && error("No enough arguments!")
  boardsize = parse(Int, ARGS[1])
  initcellcount = parse(Int, ARGS[2])

  board = makeboard(boardsize, initcellcount)
  round = 0
  while round < 100000
    # println("Iterate $(round):")
    # printboard(board)
    # println()

    board = evolve(board)
    round += 1
    # sleep(0.5)
  end
end

end

@time lifegame.main()

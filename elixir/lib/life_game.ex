defprotocol LifeGame do
  defdelegate place_random_seeds(board, count), to: LifeGame.Util

  defdelegate evolve(board), to: LifeGame.Util
end

defmodule LifeGame.Util do
  def place_random_seeds(board, count) do
    size = Board.size(board)

    if count < 0 || count > size * size do
      Process.exit(self(), {:kill, "Invalid count"})
    end

    Board.indices(board)
    |> Enum.take_random(count)
    |> Enum.reduce(board, fn {row, col}, acc ->
      Board.set(acc, row, col, true)
    end)
  end

  def evolve(board) do
    Board.indices(board)
    |> Enum.map(fn {row, col} -> {{row, col}, will_be_alive(board, row, col)} end)
    |> (fn data -> Board.from_list(board, data) end).()
  end

  defp will_be_alive(board, row, col) do
    count = neighbour_count(board, row, col)
    is_alive = Board.get(board, row, col)

    count == 3 || (is_alive && count == 2)
  end

  defp neighbour_count(board, row, col) do
    [
      {row - 1, col - 1},
      {row - 1, col},
      {row - 1, col + 1},
      {row, col - 1},
      {row, col + 1},
      {row + 1, col - 1},
      {row + 1, col},
      {row + 1, col + 1}
    ]
    |> Enum.count(fn {row, col} -> Board.get(board, row, col) end)
  end
end

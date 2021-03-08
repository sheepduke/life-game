defprotocol Board do
  def size(board)

  def indices(board)

  @doc """
  Get the existence of element at (row, col) coordinate.
  """
  def get(board, row, col)

  @doc """
  Set the existence of element at (row, col) coordinate.
  """
  def set(board, row, col, value)

  @doc """
  Build board from given data.
  The data is of format `{{row, col}, value}`.
  """
  def from_list(board, data)
end

defmodule Board.Util do
  def indices_of(size) do
    range = 0..(size - 1)

    for row <- range, col <- range, do: {row, col}
  end

  def to_string(board) do
    size = Board.size(board)

    1..size
    |> Enum.map(fn row ->
      1..size
      |> Enum.map(fn col -> if Board.get(board, row, col), do: "X ", else: "- " end)
    end)
    |> Enum.join("\n")
  end
end

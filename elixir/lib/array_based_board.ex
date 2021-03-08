defmodule ArrayBasedBoard do
  defstruct size: 0, indices: [], cells: :array.new()

  def new(size) do
    array =
      0..(size - 1)
      |> Enum.reduce(:array.new(size), fn index, acc ->
        :array.set(index, :array.new(size, default: false), acc)
      end)

    %ArrayBasedBoard{size: size, indices: Board.Util.indices_of(size), cells: array}
  end
end

defimpl Board, for: ArrayBasedBoard do
  def size(%ArrayBasedBoard{size: size}), do: size

  def indices(%ArrayBasedBoard{indices: indices}), do: indices

  def get(%ArrayBasedBoard{size: size, cells: cells}, row, col) do
    if row >= 0 && row < size && col >= 0 && col < size do
      :array.get(col, :array.get(row, cells))
    else
      nil
    end
  end

  def set(board = %ArrayBasedBoard{cells: cells}, row, col, value) do
    row_array = :array.get(row, cells)
    new_cells = :array.set(row, :array.set(col, value, row_array), cells)
    %ArrayBasedBoard{board | cells: new_cells}
  end

  def from_list(board, data) do
    data
    |> Enum.reduce(board, fn {{row, col}, value}, acc ->
      set(acc, row, col, value)
    end)
  end
end

defimpl String.Chars, for: ArrayBasedBoard do
  defdelegate to_string(board), to: Board.Util
end

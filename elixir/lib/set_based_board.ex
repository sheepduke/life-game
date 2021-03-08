defmodule SetBasedBoard do
  defstruct size: 0, indices: [], cells: MapSet.new()

  def new(size) do
    %SetBasedBoard{
      size: size,
      indices: Board.Util.indices_of(size),
      cells: MapSet.new()
    }
  end
end

defimpl Board, for: SetBasedBoard do
  def size(%SetBasedBoard{size: size}), do: size

  def indices(%SetBasedBoard{indices: indices}), do: indices

  def get(%SetBasedBoard{cells: cells}, row, col) do
    MapSet.member?(cells, {row, col})
  end

  def set(board = %SetBasedBoard{cells: cells}, row, col, value) do
    new_cells =
      if value do
        MapSet.put(cells, {row, col})
      else
        MapSet.delete(cells, {row, col})
      end

    %SetBasedBoard{board | cells: new_cells}
  end

  def from_list(board, data) do
    cells =
      data
      |> Enum.reduce(MapSet.new(), fn {{row, col}, value}, acc ->
        if value, do: MapSet.put(acc, {row, col}), else: acc
      end)

    %SetBasedBoard{board | cells: cells}
  end
end

defimpl String.Chars, for: SetBasedBoard do
  defdelegate to_string(board), to: Board.Util
end

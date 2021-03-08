defmodule MapBasedBoard do
  defstruct size: 0, indices: [], cells: Map.new()

  def new(size) do
    indices = Board.Util.indices_of(size)

    %MapBasedBoard{
      size: size,
      indices: indices,
      cells: indices |> Enum.map(fn key -> {key, false} end) |> Map.new()
    }
  end
end

defimpl Board, for: MapBasedBoard do
  def size(%MapBasedBoard{size: size}), do: size

  def indices(%MapBasedBoard{indices: indices}), do: indices

  def get(%MapBasedBoard{size: size, cells: cells}, row, col) do
    if valid_index?(row, col, size) do
      Map.get(cells, {row, col})
    else
      nil
    end
  end

  def set(board = %MapBasedBoard{cells: cells}, row, col, value) do
    %MapBasedBoard{board | cells: Map.put(cells, {row, col}, value)}
  end

  def from_list(board, data) do
    %MapBasedBoard{board | cells: Map.new(data)}
  end

  defp valid_index?(row, col, size) do
    row >= 1 && row <= size && col >= 1 && col <= size
  end
end

defimpl String.Chars, for: MapBasedBoard do
  defdelegate to_string(board), to: Board.Util
end

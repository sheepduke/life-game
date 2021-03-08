defmodule EtsBasedBoard do
  defstruct size: 0, indices: [], cells: :null

  def new(size) do
    %EtsBasedBoard{
      size: size,
      indices: Board.Util.indices_of(size),
      cells: :ets.new(:cells, [:set, :protected])
    }
  end
end

defimpl Board, for: EtsBasedBoard do
  def size(%EtsBasedBoard{size: size}), do: size

  def indices(%EtsBasedBoard{indices: indices}), do: indices

  def get(%EtsBasedBoard{cells: cells}, row, col) do
    case :ets.lookup(cells, {row, col}) do
      [] -> false
      [_] -> true
    end
  end

  def set(board = %EtsBasedBoard{cells: cells}, row, col, value) do
    if value, do: :ets.insert(cells, {{row, col}, true})
    board
  end

  def from_list(board = %EtsBasedBoard{}, data) do
    cells = :ets.new(:cells, [:set, :protected])

    data
    |> Enum.each(fn {{row, col}, value} ->
      if value, do: :ets.insert(cells, {{row, col}, true})
    end)

    %EtsBasedBoard{board | cells: cells}
  end
end

defimpl String.Chars, for: EtsBasedBoard do
  defdelegate to_string(board), to: Board.Util
end

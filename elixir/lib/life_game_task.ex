defmodule Mix.Tasks.LifeGame.Run do
  def run(args) do
    if length(args) != 2 do
      IO.puts("Usage: mix life_game.run BOARD_SIZE SEEDS_COUNT\n")
    else
      # Parse the arguments and run with it.
      [board_size, seeds_count] = args |> Enum.map(&String.to_integer/1)

      AppConfig.module().new(board_size)
      |> LifeGame.place_random_seeds(seeds_count)
      |> run_cycles(1)
    end
  end

  defp run_cycles(board, cycle) do
    IO.puts("Cycle #{cycle}:\n\n#{board}\n\n")
    Process.sleep(300)
    run_cycles(LifeGame.evolve(board), cycle + 1)
  end
end

defmodule Mix.Tasks.LifeGame.Measure do
  import ExProf.Macro

  def run(args) do
    if length(args) != 2 do
      IO.puts("Usage: mix life_game.run BOARD_SIZE SEEDS_COUNT\n")
    else
      # Parse the arguments and run with it.
      [board_size, seeds_count] = args |> Enum.map(&String.to_integer/1)
      board = AppConfig.module().new(board_size)

      if AppConfig.profile?() do
        profile do
          board
          |> LifeGame.place_random_seeds(seeds_count)
          |> run_cycles(1)
        end
      else
        {time, _} =
          :timer.tc(fn ->
            board
            |> LifeGame.place_random_seeds(seeds_count)
            |> run_cycles(1)
          end)

        IO.inspect(time / 1000_000)
      end
    end
  end

  defp run_cycles(board, cycle) do
    if cycle <= AppConfig.cycle() do
      run_cycles(LifeGame.evolve(board), cycle + 1)
    end
  end
end

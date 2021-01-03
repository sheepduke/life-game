open Base
open Stdio

module Board = struct
  type t = bool Array.t

  let create size =
    Array.init size ~f:(fun _ -> Array.create ~len:size false)

  let place_random_seeds this seed_count =
    Random.self_init ();
    let size = Array.length this in
    let random_bound = size * size in
    let rec aux board seed_count =
      if seed_count > 0 then
        let random = Random.int random_bound in
        let row = random / size in
        let col = random % size in
        if board.(row).(col) then
          aux board seed_count
        else
          (board.(row).(col) <- true;
           aux board (seed_count - 1))
    in aux this seed_count

  let print this =
    Array.iter this ~f:(fun row ->
        Array.iter row ~f:(fun cell ->
            printf "%c " (if cell then 'X' else '-'));
        print_endline "")

  let can_live this row col =
    let size = Array.length this in
    let alive_neighbour_count = ref 0 in
    let get_min x = if x > 0 then x else 0 in
    for i = (get_min (row - 1)) to (min (size - 1) (row + 1)) do
      for j = (get_min (col - 1)) to (min (size - 1) (col + 1)) do
        if (i <> row || j <> col) && this.(i).(j) then
          Int.incr alive_neighbour_count;
      done
    done;
    let is_alive = this.(row).(col) in
    !alive_neighbour_count = 3 || (!alive_neighbour_count = 2 && is_alive)

  let evolve this =
    Array.mapi this ~f:(fun row _ ->
        Array.mapi this ~f:(fun col _ ->
            can_live this row col))
end

let () =
  let args = Sys.get_argv () in
  let size = Int.of_string args.(1) in
  let seeds_count = Int.of_string args.(2) in

  let cycle_count = 100000 in
  let board = ref (Board.create size) in
  Board.place_random_seeds !board seeds_count;
  let cycle = ref 0 in
  while !cycle <= cycle_count do
    (* printf "\n========== Cycle %d ==========\n" !cycle;
     * Board.print !board;
     * Unix.sleepf 0.2; *)
    board := Board.evolve !board;
    Int.incr cycle
  done;

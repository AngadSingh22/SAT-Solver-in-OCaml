(* main.ml *)

open Printf

let () =
  if Array.length Sys.argv <> 3 then begin
    eprintf "Usage: %s <input_file> <output_file>\n%!" Sys.argv.(0);
    exit 1
  end;

  let input_file = Sys.argv.(1) in
  let output_file = Sys.argv.(2) in

  try
    let num_vars, clauses = Parser.parse_dimacs input_file in

    printf "Solving SAT instance with %d variables and %d clauses...\n%!"
      num_vars (List.length clauses);

    let start_time = Unix.gettimeofday () in

    let assignment = Sat_solver.solve_sat num_vars clauses in

    let end_time = Unix.gettimeofday () in
    printf "Solved in %.4f seconds\n%!" (end_time -. start_time);

    Parser.write_solution output_file assignment;

    (match assignment with
     | None ->
         print_endline "UNSAT - No solution exists"
     | Some _ ->
         print_endline "SAT - Solution found!")

  with
  | e ->
      eprintf "Error: %s\n%!" (Printexc.to_string e);
      exit 1

(* parser.ml *)

open Printf

let parse_dimacs (filename : string) : int * int list list =
  (* Returns (num_vars, clauses) where clauses is a list of int lists *)
  let ic = open_in filename in
  let rec loop num_vars clauses =
    try
      let line = input_line ic |> String.trim in
      if line = "" || line.[0] = 'c' then
        loop num_vars clauses
      else if line.[0] = 'p' then
        (* problem line: p cnf <num_vars> <num_clauses> *)
        let parts =
          line
          |> String.split_on_char ' '
          |> List.filter (fun s -> s <> "")
        in
        let num_vars' =
          match parts with
          | _ :: "cnf" :: nv :: _ -> int_of_string nv
          | _ -> num_vars
        in
        loop num_vars' clauses
      else
        (* clause line: ints ending with 0 *)
        let parts =
          line
          |> String.split_on_char ' '
          |> List.filter (fun s -> s <> "")
        in
        let rec parse_lits ps acc =
          match ps with
          | [] -> List.rev acc
          | p :: rest ->
              let v = int_of_string p in
              if v = 0 then
                List.rev acc
              else
                parse_lits rest (v :: acc)
        in
        let clause = parse_lits parts [] in
        let clauses' = if clause = [] then clauses else clause :: clauses in
        loop num_vars clauses'
    with End_of_file ->
      close_in ic;
      (num_vars, List.rev clauses)
  in
  loop 0 []


let write_solution (filename : string) (assignment : (int * bool) list option) : unit =
  (* Writes SAT/UNSAT in the same format as the Python version *)
  let oc = open_out filename in
  (match assignment with
   | None ->
       output_string oc "UNSAT\n"
   | Some assign ->
       output_string oc "SAT\n";
       let sorted =
         List.sort (fun (v1, _) (v2, _) -> compare v1 v2) assign
       in
       List.iter
         (fun (var, value) ->
           let lit = if value then var else -var in
           fprintf oc "%d " lit)
         sorted;
       output_string oc "0\n");
  close_out oc

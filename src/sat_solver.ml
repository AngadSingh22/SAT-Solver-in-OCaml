(* sat_solver.ml *)

type lit = int * bool          (* (var, neg) *)
type clause = lit list

type value = Undef | True | False

type assign_record = {
  mutable value : value;
  mutable antecedent : clause option;
  mutable level : int;
}

let eval_lit ((v, neg) : lit) (assignments : assign_record array) : value =
  let a = assignments.(v) in
  match a.value with
  | Undef -> Undef
  | True ->
      if neg then False else True
  | False ->
      if neg then True else False

type clause_status =
  | Satisfied
  | Unsatisfied
  | Unit
  | Unresolved

let clause_status (cl : clause) (assignments : assign_record array) : clause_status =
  let len = List.length cl in
  let has_true = ref false in
  let false_count = ref 0 in
  List.iter
    (fun lit ->
       match eval_lit lit assignments with
       | True -> has_true := true
       | False -> incr false_count
       | Undef -> ())
    cl;
  if !has_true then Satisfied
  else if !false_count = len then Unsatisfied
  else if !false_count = len - 1 then Unit
  else Unresolved

type up_result =
  | UpConflict of clause
  | UpUnresolved

let unit_propagation
    (formula_clauses : clause list ref)
    (assignments : assign_record array)
    (current_dl : int) : up_result =
  (* Direct port of Python unit_propagation *)
  let rec outer () =
    let finish = ref true in
    let conflict : clause option ref = ref None in

    let rec loop_clauses cls =
      match cls with
      | [] -> ()
      | cl :: rest ->
          if !conflict <> None then ()
          else begin
            match clause_status cl assignments with
            | Satisfied | Unresolved ->
                loop_clauses rest
            | Unit ->
                (* Find the unassigned literal *)
                let rec find_unassigned lits =
                  match lits with
                  | [] -> ()
                  | (v, neg) :: tl ->
                      if assignments.(v).value = Undef then begin
                        let val_bool = not neg in
                        assignments.(v).value <- (if val_bool then True else False);
                        assignments.(v).antecedent <- Some cl;
                        assignments.(v).level <- current_dl;
                        finish := false
                      end else
                        find_unassigned tl
                in
                find_unassigned cl;
                loop_clauses rest
            | Unsatisfied ->
                conflict := Some cl
          end
    in

    loop_clauses !formula_clauses;
    match !conflict with
    | Some cl -> UpConflict cl
    | None ->
        if not !finish then outer ()
        else UpUnresolved
  in
  outer ()


(* resolve(a, b, x): resolution of clauses a and b on variable x *)
let resolve (a : clause) (b : clause) (x : int) : clause =
  let module S =
    Set.Make(struct
      type t = lit
      let compare = compare
    end)
  in
  let add_clause s (cl : clause) =
    List.fold_left
      (fun acc ((v, _) as lit) ->
         if v = x then acc else S.add lit acc)
      s cl
  in
  let s = add_clause S.empty a in
  let s = add_clause s b in
  S.elements s


let literals_at_level (cl : clause) (assignments : assign_record array) (dl : int)
  : clause =
  List.fold_left
    (fun acc ((v, _) as lit) ->
       match assignments.(v).value with
       | Undef -> acc
       | True | False ->
           if assignments.(v).level = dl then lit :: acc else acc)
    [] cl


let conflict_analysis
    (clause0 : clause)
    (assignments : assign_record array)
    (current_dl : int)
  : int * clause option =
  (* Direct port of Python conflict_analysis *)
  if current_dl = 0 then (-1, None)
  else
    let rec loop clause =
      let literals = literals_at_level clause assignments current_dl in
      if List.length literals <> 1 then begin
        (* Find implied literals (with antecedents) *)
        let implied =
          List.filter
            (fun (v, _) ->
               match assignments.(v).antecedent with
               | Some _ -> true
               | None -> false)
            literals
        in
        match implied with
        | [] -> clause
        | (v, _) :: _ ->
            let antecedent =
              match assignments.(v).antecedent with
              | Some c -> c
              | None -> []
            in
            let clause' = resolve clause antecedent v in
            loop clause'
      end else
        clause
    in
    let learnt_clause = loop clause0 in

    (* Compute backtrack level *)
    let levels =
      List.fold_left
        (fun acc (v, _) ->
           match assignments.(v).value with
           | Undef -> acc
           | True | False ->
               let dl = assignments.(v).level in
               if List.mem dl acc then acc else dl :: acc)
        [] learnt_clause
    in
    let levels_sorted = List.sort compare levels in
    let backtrack_level =
      match levels_sorted with
      | [] | [_] -> 0
      | _ ->
          let rec second_last = function
            | [] | [_] -> 0
            | [a; _] -> a
            | _ :: tl -> second_last tl
          in
          second_last levels_sorted
    in
    (backtrack_level, Some learnt_clause)


let backtrack (assignments : assign_record array) (b : int) : unit =
  (* Remove assignments with dl > b *)
  Array.iteri
    (fun v a ->
       if v > 0 && a.level > b then begin
         a.value <- Undef;
         a.antecedent <- None;
         a.level <- 0
       end)
    assignments


let pick_branching_variable (variables : int list) (assignments : assign_record array)
  : int * bool =
  (* Fixed heuristic: first unassigned variable, always True *)
  let rec aux = function
    | [] -> failwith "No unassigned variables"
    | v :: vs ->
        if assignments.(v).value = Undef then (v, true)
        else aux vs
  in
  aux variables


let count_assigned (variables : int list) (assignments : assign_record array) : int =
  List.fold_left
    (fun acc v ->
       match assignments.(v).value with
       | Undef -> acc
       | True | False -> acc + 1)
    0 variables


exception Unsat


let solve_sat (num_vars : int) (clauses : int list list)
  : (int * bool) list option =
  (* Convert clauses to internal (var,neg) format and collect variables *)
  let present = Array.make (num_vars + 1) false in
  let formula_init =
    List.map
      (fun clause ->
         List.map
           (fun lit ->
              let var = abs lit in
              let neg = lit < 0 in
              if var <= num_vars then present.(var) <- true;
              (var, neg))
           clause)
      clauses
  in
  let variables =
    let rec collect v acc =
      if v = 0 then acc
      else
        if present.(v) then collect (v - 1) (v :: acc)
        else collect (v - 1) acc
    in
    collect num_vars []
  in
  let assignments =
    Array.init (num_vars + 1)
      (fun _ -> { value = Undef; antecedent = None; level = 0 })
  in
  let formula_clauses = ref formula_init in
  let current_dl = ref 0 in

  let total_vars = List.length variables in
  let count_assigned () = count_assigned variables assignments in

  try
    (* Initial unit propagation at decision level 0 *)
    (match unit_propagation formula_clauses assignments !current_dl with
     | UpConflict _ -> raise Unsat
     | UpUnresolved -> ());

    (* Main CDCL loop *)
    while count_assigned () < total_vars do
      let (var, value) = pick_branching_variable variables assignments in
      incr current_dl;
      assignments.(var).value <- if value then True else False;
      assignments.(var).antecedent <- None;
      assignments.(var).level <- !current_dl;

      let rec inner () =
        match unit_propagation formula_clauses assignments !current_dl with
        | UpUnresolved -> ()
        | UpConflict conflict_clause ->
            let (b, learnt_opt) =
              conflict_analysis conflict_clause assignments !current_dl
            in
            if b < 0 then
              raise Unsat
            else begin
              (match learnt_opt with
               | Some learnt ->
                   formula_clauses := learnt :: !formula_clauses
               | None -> ());
              backtrack assignments b;
              current_dl := b;
              inner ()
            end
      in
      inner ()
    done;

    (* SAT: convert assignments back to (var, bool) list *)
    let result =
      List.fold_left
        (fun acc v ->
           match assignments.(v).value with
           | True -> (v, true) :: acc
           | False -> (v, false) :: acc
           | Undef ->
               (* Should not happen; default to True if it does *)
               (v, true) :: acc)
        [] variables
    in
    Some result

  with Unsat ->
    None

# CDCL SAT Solver in OCaml

An OCaml implementation of a Conflict-Driven Clause Learning (CDCL) SAT solver for DIMACS CNF inputs. The project ports the behavior of a reference Python solver into a typed functional implementation with explicit parsing, solving, and output-writing stages.

## What this project demonstrates

- DIMACS CNF parsing and SAT/UNSAT output formatting
- Internal literal and clause representation in OCaml
- Unit propagation over partially assigned clauses
- Conflict analysis and learned-clause generation
- Non-chronological backtracking
- Deterministic branching for reproducible behavior
- A small command-line wrapper for solving files end to end

## Solver behavior

Input: a DIMACS CNF file.

Output:

- `UNSAT` when the formula is unsatisfiable
- `SAT` followed by a model line ending in `0` when a satisfying assignment is found

The current implementation uses a clear CDCL-style structure with naive unit propagation. It intentionally does not use watched literals, VSIDS, restarts, or other production-SAT optimizations.

## Repository structure

```text
SAT-Solver-in-OCaml/
|-- README.md
|-- LICENSE
|-- src/
|   |-- parser.ml       # DIMACS parsing and solution writing
|   |-- sat_solver.ml   # CDCL core: propagation, analysis, learning, backtracking
|   `-- main.ml         # CLI entrypoint
`-- test_run/
    `-- main_run        # Enclosed test/run helper
```

## Core algorithm

The solver keeps a mutable assignment record for each variable:

- current truth value
- antecedent clause, if implied
- decision level

The main loop alternates between branching and propagation. When propagation finds a conflicting clause, the solver resolves through antecedents to form a learned clause, backtracks to the computed level, adds the learned clause to the formula, and continues. If a conflict appears at decision level 0, the instance is UNSAT.

## Usage

The CLI expects an input DIMACS file and an output path:

```bash
ocamlc -o sat_solver unix.cma src/parser.ml src/sat_solver.ml src/main.ml
./sat_solver input.cnf output.txt
```

Example output file for a satisfiable formula:

```text
SAT
1 -2 3 0
```

Example output file for an unsatisfiable formula:

```text
UNSAT
```

## Notes and limitations

This repository is designed as a compact algorithmic implementation, not a competitive industrial SAT solver. Its main value is clarity: the CDCL mechanics are visible in a small OCaml codebase, making it useful for studying parsing, symbolic search, learned clauses, and backtracking logic.
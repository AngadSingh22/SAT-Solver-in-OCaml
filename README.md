# CDCL SAT Solver (OCaml Port)

This repository contains an OCaml port of the reference Python CDCL SAT solver
from the Ashoka ICS "SAT" assignment.

The implementation matches the Python behaviour:

- Input: DIMACS CNF
- Output:
  - `UNSAT` if the instance is unsatisfiable
  - `SAT` followed by a single line of literals (`v` if `x_v = true`, `-v` if `x_v = false`), ending with `0`

Algorithmic features:

- CDCL-style search
- Naive unit propagation (no watched literals)
- Conflict analysis + clause learning
- Non-chronological backtracking
- Deterministic branching:
  - first unassigned variable
  - value `True` first

## Project structure

```text
cdcl-sat-ocaml/
├── README.md
├── src/
│   ├── parser.ml       # DIMACS parsing & solution writing
│   ├── sat_solver.ml   # CDCL core logic (OCaml port of sat_solver.py)
│   ├── main.ml         # CLI wrapper (OCaml port of main.py)
│   └── Makefile        # Simple build script using ocamlc
└── tests/
    └── input/
        └── test_3.cnf  # Example UNSAT test

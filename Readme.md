# Persistent Data Structures in OCaml

An exploration of persistent functional data structures in OCaml, with a focus on implementation, structural sharing, and formal verification.

## Implemented

* Binary Search Tree (BST)
* Persistent AVL Tree
* Leftist Heap
* AVL rotations and rebalancing
* Persistent updates through structural sharing

## Formal Verification

The project also contains Isabelle/HOL formalizations of the data structures.

Currently verified:

* BST insertion preserves the BST property

More formal proofs can be added as the project develops.

```

## Goal

The goal of this project is to understand how persistent data structures are implemented in a functional language, how structural sharing affects their behavior, and how their correctness can be formally expressed and verified using Isabelle/HOL.

## Technologies

* OCaml
* Isabelle/HOL
* Git

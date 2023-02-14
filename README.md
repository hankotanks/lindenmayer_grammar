# Lindenmayer System Grammar

A minimal implementation of the Lindenmayer grammar using Enum alphabets.

## The Alphabet

The basic unit of an L-system is its alphabet, which is a user-defined, data-less Enum that defines a few basic traits. The *Alphabet* trait is implied for all types that implement `Clone`, `Copy`, `PartialEq` and `Debug`.

```
#[derive(Clone, Copy, PartialEq, Debug)]
enum FractalTreeAlphabet { Leaf, Node, Left, Right }
```

## Axioms

An *Axiom* is a sentence of symbols from a single alphabet.

```
// Creating an axiom from a collection of symbols
use FractalTreeAlphabet::*
let axiom = Axiom::from(vec![Node, Left, Leaf, Right, Leaf]);

// ...or from a single symbol
let mut axiom = Axiom::from(Leaf);
```

## Productions & Rulesets

Axioms can be rewritten using a *Ruleset*, which itself is a number of *Production* rules.

```
// Defining productions using a macro
let p1 = production!(Node => Node : Node);
let p2 = production!(Leaf => Node : Left : Leaf : Right : Leaf);

// Building the Ruleset...
let rules = Ruleset::from(vec![p1, p2]);

// ...or use the provided macro
let rules = rules!(
    Node => Node : Node, 
    Leaf => Node : Left : Leaf : Right : Leaf
);

// Rewrite the Axiom using the Ruleset
axiom.rewrite(&rules); // dbg: [Node, Left, Leaf, Right, Leaf]
```

---

### Accessing Axiom Data

Although the internal structure of axioms are not available, axioms themselves are iterators. Iteration is reset upon exhaustion, allowing it to be easily reused.

```
for symbol in axiom {
    print!("{:?}, ", symbol); // dbg: Node, Left, Leaf, Right, Leaf, 
}

// We can still collect the Axiom
axiom.collect::<Vec<_>>(); // dbg: [Node, Left, Leaf, Right, Leaf]
```

All given examples use the [`turtle`](https://turtle.rs/) crate to easily visualize the results of L-system expansion.

I determined that drawing functionality was outside the scope of this crate. Check out the [`dcc-lsystem`](https://crates.io/crates/dcc-lsystem) crate by Robert Usher for a more in-depth implementation that allows axioms to be mapped directly to turtle commands.

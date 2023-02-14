# Lindenmayer System Grammar

A minimal implementation of the Lindenmayer grammar using Enum alphabets.

## Basic Usage

The basic unit of an L-system is its alphabet, which is a user-defined, data-less Enum that defines a few basic traits.

```
#[derive(Clone, Copy, PartialEq, Debug)]
enum FractalTreeAlphabet { Leaf, Node, Left, Right }
```

An [Axiom] is a sentence of symbols from a single alphabet.

```
// Creating an axiom from a collection of symbols
use FractalTreeAlphabet::*
let axiom = Axiom::from(vec![Node, Left, Leaf, Right, Leaf]);

// ...or from a single symbol
let mut axiom = Axiom::from(FractalTreeAlphabet::Leaf);
```

Axioms can be rewritten using a [Ruleset], which itself is a number of [Production] rules.

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

Axioms themselves are iterators. Iteration is reset upon exhaustion, allowing it to be easily reused.

```
// dbg: Node, Left, Leaf, Right, Leaf, 
for symbol in axiom {
    print!("{:?}, ", symbol);
}

// We can still collect the Axiom
axiom.collect::<Vec<_>>(); // dbg: [Node, Left, Leaf, Right, Leaf]
```

All given examples use the [Turtle](https://turtle.rs/) crate to easily visualize the results of L-system expansion.

I decided that drawing functionality was outside the scope of this crate; check out the [dcc-lsystem](https://crates.io/crates/dcc-lsystem) crate by Robert Usher for a more in-depth implementation that allows axioms to be mapped directly to turtle commands.

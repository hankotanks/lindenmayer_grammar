# Lindenmayer Grammar

A minimal implementation of the Lindenmayer formal grammar.

## Concept

Many other L-system crates I've found have treated the L-System itself as a type.
To me, this approach obscured what was actually happening and felt like overkill.

This crate showcases my approach.
It supports stochastic and context sensitive rules.

Axioms are the strings that substitution is performed on, and are composed of symbols from a single `Alphabet` (any type that implements `Clone` and `Ord`).
Axioms can be rewritten using a `Ruleset`, which is an ordered collection of individual `Production` rules.
The crate allows symbols to be mapped to the movement of a [turtle](https://en.wikipedia.org/wiki/Turtle_graphics).
The resulting graphic can be displayed or saved as an image.

| ![](/images/koch.png) | ![](/images/fern.png) |
|:---------------------:|:---------------------:|

| ![](/images/dragon_curve.png) | ![](/images/sierpinski.png) |
|:---------------------:|:---------------------:|

| ![](/images/levy.png) |
|:---------------------:|

## Example

```
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
enum FractalTree { Leaf, Node, Left, Right }

use FractalTree::*;

// Create an axiom from a single symbol
let mut axiom = Axiom::new(Leaf);

// Define the ruleset for creating a fractal tree
let rules = rules!(
    Node => Node : Node, 
    Leaf => Node : Left : Leaf : Right : Leaf
);

// Rewrite the axiom 6 times
for _ in 0..6 { 
    axiom.rewrite(&rules); 
}

// Build a turtle using the TurtleBuilder type
use TurtleAction::*;
let turtle = TurtleBuilder::new()
    .assign_action_set(Leaf, [Forward, PenUp, Backward, PenDown])
    .assign_action(Node, Forward)
    .assign_action_set(Left, [PushState, Turn(-PI * 0.25)])
    .assign_action_set(Right, [PopState, Turn(PI * 0.25)])
    .build();

// Last, draw the L-System's state and save to "tree.png"
axiom.visualize(turtle).save(
    [650, 600], 
    StrokeStyle { width: 2.0, ..Default::default() }, 
    SolidSource::from_unpremultiplied_argb(0xFF, 0xFF, 0xFF, 0xFF)
    "tree.png"
).unwrap();
```

use std::f32::consts::PI;

use lindenmayer_grammar::{
    Axiom, 
    rules, 
    TurtleBuilder, 
    TurtleAction
};

const DEPTH: i32 = 7;
const ANGLE: f32 = PI * 0.25;
const FILE_NAME: &'static str = "fractal_tree.png";
const RESOLUTION: f32 = 2.0;

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
enum FractalTreeAlphabet { Leaf, Node, Left, Right }

fn main() -> anyhow::Result<()> {
    use FractalTreeAlphabet::*;

    let mut axiom = Axiom::new(Leaf);
    
    let rules = rules!(
        Node => Node : Node, 
        Leaf => Node : Left : Leaf : Right : Leaf
    );

    for _ in 0..DEPTH { 
        axiom.rewrite(&rules); 
    }

    use TurtleAction::*;
    let turtle = TurtleBuilder::new()
        .assign_action_set(Leaf, [Forward, PenUp, Backward, PenDown])
        .assign_action(Node, Forward)
        .assign_action_set(Left, [PushState, Turn(-ANGLE)])
        .assign_action_set(Right, [PopState, Turn(ANGLE)])
        .build();

    axiom.visualize(turtle).save(RESOLUTION, FILE_NAME)
}
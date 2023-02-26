use std::f32::consts::PI;

use lindenmayer_grammar::{
    Axiom, 
    rules, 
    TurtleBuilder, 
    TurtleAction
};

use raqote::{StrokeStyle, SolidSource};

const DEPTH: i32 = 8;

const ANGLE: f32 = PI * 0.25;

const DIMENSIONS: [u32; 2] = [650, 600];

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
enum FractalTree { Leaf, Node, Left, Right }

#[show_image::main]
fn main() -> anyhow::Result<()> {
    use FractalTree::*;

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

    axiom.visualize(turtle).show(
        DIMENSIONS, 
        StrokeStyle::default(), 
        SolidSource::from_unpremultiplied_argb(0xFF, 0xFF, 0xFF, 0xFF)
    )
}
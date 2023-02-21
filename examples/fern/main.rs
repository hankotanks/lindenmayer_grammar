use std::f32::consts::PI;

use lindenmayer_grammar::{
    Axiom, 
    rules, 
    TurtleBuilder, 
    TurtleAction
};

const DEPTH: i32 = 5;
const ANGLE_DEGREES: f32 = 20.0;
const ANGLE_RADS: f32 = ANGLE_DEGREES * PI / 180.0;
const FILE_NAME: &'static str = "fern.png";
const RESOLUTION: f32 = 50.0;

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
enum BarnsleyFernAlphabet { A, B, Left, Right, Open, Close } 

fn main() -> anyhow::Result<()> {
    use BarnsleyFernAlphabet::*;
    
    let mut axiom = Axiom::new(A);

    let rules = rules!(
        B => B : B,
        A => B : Left : Open : Open : A : Close : Right : A : Close 
               : Right : B : Open : Right : B : A : Close : Left : A
    );

    for _ in 0..DEPTH { 
        axiom.rewrite(&rules); 
    }

    use TurtleAction::*;
    let turtle = TurtleBuilder::new()
        .assign_action(B, Forward)
        .assign_action(Left, Turn(-ANGLE_RADS))
        .assign_action(Right, Turn(ANGLE_RADS))
        .assign_action(Open, PushState)
        .assign_action(Close, PopState)
        .build();

    axiom.visualize(turtle).save(RESOLUTION, FILE_NAME)
}
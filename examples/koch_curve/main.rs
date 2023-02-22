use std::f32::consts::PI;

use lindenmayer_grammar::{
    Axiom, 
    rules, 
    TurtleBuilder, 
    TurtleAction
};

use raqote::StrokeStyle;

const DEPTH: u32 = 5;

const ANGLE: f32 = PI * 0.5;

#[show_image::main]
fn main() -> anyhow::Result<()> {
    let mut axiom = Axiom::new(0);

    let rules = rules!(0 => 0 : 1 : 0 : 2 : 0 : 2 : 0 : 1 : 0);

    for _ in 0..DEPTH { 
        axiom.rewrite(&rules); 
    }

    use TurtleAction::*;
    let turtle = TurtleBuilder::new()
        .assign_action(0, Forward)
        .assign_action(1, Turn(-ANGLE))
        .assign_action(2, Turn(ANGLE))
        .build();

    axiom.visualize(turtle).show([1200, 620], &StrokeStyle::default())
}
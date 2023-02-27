use std::f32::consts::TAU;

use lindenmayer_grammar::{
    Axiom, 
    rules, 
    TurtleBuilder, 
    TurtleAction
};

use raqote::{
    StrokeStyle, 
    SolidSource
};

// The number of times to rewrite the axiom
const DEPTH: i32 = 6;

// The angle to turn by
const ANGLE: f32 = TAU / 3.0;

const DIMENSIONS: [u32; 2] = [900, 800];

#[show_image::main]
fn main() -> anyhow::Result<()> {
    let mut axiom = Axiom::with_elements([0, 3, 1, 3, 1]);

    let rules = rules!(
        0 => 0 : 3 : 1 : 2 : 0 : 2 : 1 : 3 : 0,
        1 => 1 : 1
    );

    for _ in 0..DEPTH { 
        axiom.rewrite_in_place(&rules); 
    }

    use TurtleAction::*;
    let turtle = TurtleBuilder::new()
        .assign_action(0, Forward)
        .assign_action(1, Forward)
        .assign_action(2, Turn(-ANGLE))
        .assign_action(3, Turn(ANGLE))
        .build();

    axiom.visualize(turtle).show(
        DIMENSIONS, 
        StrokeStyle::default(), 
        SolidSource::from_unpremultiplied_argb(0xFF, 0xFF, 0xFF, 0xFF)
    )
}
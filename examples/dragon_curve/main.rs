use std::f32::consts::PI;

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

const DEPTH: i32 = 10;

const ANGLE: f32 = PI * 0.5;

const DIMENSIONS: [u32; 2] = [600, 650];

#[show_image::main]
fn main() -> anyhow::Result<()> {
    let mut axiom = Axiom::new(0);

    let rules = rules!(
        0 => 0 : 2 : 1,
        1 => 0 : 3 : 1
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
        StrokeStyle { width: 3.0, ..Default::default() }, 
        SolidSource::from_unpremultiplied_argb(0xFF, 0xFF, 0xFF, 0xFF)
    )
}
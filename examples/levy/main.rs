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

const DEPTH: i32 = 12;

const ANGLE: f32 = PI * 0.25;

const DIMENSIONS: [u32; 2] = [1200, 750];

#[show_image::main]
fn main() -> anyhow::Result<()> {
    let mut axiom = Axiom::new(0);

    let rules = rules!(0 => 1 : 0 : 2 : 2 : 0 : 1);

    for _ in 0..DEPTH {
        axiom.rewrite(&rules);
    }

    use TurtleAction::*;
    let turtle = TurtleBuilder::new()
        .assign_action(0, Forward)
        .assign_action_set(1, [Turn(ANGLE), SetSolidSource(move |_|
            SolidSource::from_unpremultiplied_argb(0xFF, 0, 0xFF, 0xF0)
        )])
        .assign_action_set(2, [Turn(-ANGLE), SetSolidSource(move |_|
            SolidSource::from_unpremultiplied_argb(0xFF, 0, 0xF0, 0xFF)
        )])
        .build();

    axiom.visualize(turtle).show(
        DIMENSIONS, 
        StrokeStyle::default(), 
        SolidSource::from_unpremultiplied_argb(0xFF, 0xFF, 0xFF, 0xFF)
    )
}
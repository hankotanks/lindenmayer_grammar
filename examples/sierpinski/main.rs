use std::f32::consts::TAU;

use lindenmayer_grammar::{
    Axiom, 
    rules, 
    TurtleBuilder, 
    TurtleAction
};
use raqote::{StrokeStyle, LineCap};

const DEPTH: i32 = 6;
const FILE_NAME: &'static str = "sierpinski.png";
const ANGLE: f32 = TAU / 3.0;
const RESOLUTION: f32 = 20.0;

fn main() -> anyhow::Result<()> {
    let mut axiom = Axiom::with_elements([0, 3, 1, 3, 1]);

    let rules = rules!(
        0 => 0 : 3 : 1 : 2 : 0 : 2 : 1 : 3 : 0,
        1 => 1 : 1
    );

    for _ in 0..DEPTH { 
        axiom.rewrite(&rules); 
    }

    use TurtleAction::*;
    let turtle = TurtleBuilder::new()
        .assign_action(0, Forward)
        .assign_action(1, Forward)
        .assign_action(2, Turn(-ANGLE))
        .assign_action(3, Turn(ANGLE))
        .build();

    let style = StrokeStyle {
        width: RESOLUTION * 0.2,
        cap: LineCap::Butt,
        ..Default::default()
    };

    axiom.visualize(turtle).save(RESOLUTION, &style, FILE_NAME)
}
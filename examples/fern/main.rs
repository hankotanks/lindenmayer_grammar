use std::f32::consts::PI;

use lindenmayer_grammar::{
    Axiom, 
    rules, 
    TurtleBuilder, 
    TurtleAction
};

use rand::{thread_rng, Rng};

use raqote::{
    StrokeStyle, 
    LineCap, 
    SolidSource
};

const DEPTH: i32 = 6;

const ANGLE_DEGREES: f32 = 20.0;
const ANGLE_RADS: f32 = ANGLE_DEGREES * PI / 180.0;

const DIMENSIONS: [u32; 2] = [900, 600];

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
enum BarnsleyFernAlphabet { A, B, Left, Right, Open, Close } 

#[show_image::main]
fn main() -> anyhow::Result<()> {
    use BarnsleyFernAlphabet::*;
    
    let mut axiom = Axiom::new(A);

    let rules = rules!(
        B [0.6] => B : B,
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
        .assign_action_set(Close, [PopState, SetSolidSource(
            move || SolidSource::from_unpremultiplied_argb(
                0xFF,
                thread_rng().gen_range(0..100),
                thread_rng().gen_range(150..200),
                thread_rng().gen_range(0..150),
            )
        )])
        .build();

    axiom.visualize(turtle).show(
        DIMENSIONS,
        StrokeStyle { width: 2.0, cap: LineCap::Round, ..Default::default() }, 
        SolidSource::from_unpremultiplied_argb(0xFF, 0x33, 0x99, 0x33)
    )
}
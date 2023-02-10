use std::{
    fmt::Display, 
    collections::VecDeque
};

use lindenmayer_system_framework::{
    Axiom, 
    rules,
    rewrite_in_place
};

use turtle::{
    Turtle, 
    Speed
};

#[derive(Clone, Copy, PartialEq)]
enum FractalTreeAlphabet { Leaf, Node, Left, Right }

impl Display for FractalTreeAlphabet {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use FractalTreeAlphabet::*;
        write!(f, "{}",
            match self {
                Leaf => '0',
                Node => '1',
                Left => '[',
                Right => ']',
            }
        )
    }
}

const BASE_SEG_LENGTH: f64 = 300.;

fn draw_fractal_tree(axiom: Axiom<FractalTreeAlphabet>, depth: i32) {
    use FractalTreeAlphabet::*;

    let mut turtle = Turtle::new();
    let mut turtle_state = VecDeque::new();

    // configure the turtle
    turtle.hide();
    turtle.set_speed(Speed::from(25));

    // calculate the length of each segment based on the depth of the tree
    // necessary so the whole tree fits regardless of depth
    let seg_length = BASE_SEG_LENGTH / 2f64.powi(depth);

    // draw the tree
    for token in axiom {
        match token {
            Leaf => { 
                turtle.forward(seg_length);
                turtle.backward(seg_length);
            },
            Node => { turtle.forward(seg_length); },
            Left => { 
                turtle_state.push_front((turtle.position(), turtle.heading())); 
                turtle.left(45.); 
            },
            Right => {
                turtle.pen_up();
                turtle_state.pop_front().map(|(pos, angle)| (turtle.go_to(pos), turtle.set_heading(angle - 45.)));
                turtle.pen_down();
            },
        }
    }
}

const DEPTH: i32 = 6;

fn main() {
    use FractalTreeAlphabet::*;
    let mut axiom = Axiom::from(Leaf);

    let rules = rules!(
        Node => Node : Node, 
        Leaf => Node : Left : Leaf : Right : Leaf
    );

    for _n in 0..DEPTH { rewrite_in_place(&rules, &mut axiom); }

    draw_fractal_tree(axiom, DEPTH);
}
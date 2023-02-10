use std::{fmt::Display, collections::VecDeque};

use lindenmayer_system_framework::{rewrite, production};
use turtle::Turtle;

#[derive(Clone, Copy, PartialEq)]
enum FractalTreeAlphabet {
    Leaf,
    Node,
    Left,
    Right
}

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

fn main() {
    use FractalTreeAlphabet::*;
    let mut axiom = vec![Leaf];

    let rules = vec![
        production!(Node => Node : Node),
        production!(Leaf => Node : Left : Leaf : Right : Leaf)
    ];

    for _ in 0..4 {
        axiom = rewrite(&rules, axiom.clone());
    }

    let mut turtle = Turtle::new();

    let mut turtle_state = VecDeque::new();

    for token in axiom.iter() {
        match token {
            Leaf => { 
                turtle.forward(2.); 
                turtle.pen_up(); 
                turtle.backward(2.); 
                turtle.pen_down(); 
            },
            Node => { turtle.forward(2.); },
            Left => { 
                turtle_state.push_front((turtle.position(), turtle.heading())); 
                turtle.left(45.); 
            },
            Right => {
                let (position, angle) = turtle_state.pop_front().unwrap();
                turtle.go_to(position);
                turtle.set_heading(angle);
                turtle.right(45.)
            },
        }
    }
}
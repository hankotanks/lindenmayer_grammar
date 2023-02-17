use std::collections::VecDeque;

use lindenmayer_grammar::{Axiom, rules};
use turtle::Drawing;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
enum BarnsleyFernAlphabet { A, B, Left, Right, Open, Close } 

const DEPTH: i32 = 5;

fn draw_barnsley_fern(axiom: &Axiom<BarnsleyFernAlphabet>) {
    let mut drawing = Drawing::new();

    let size = drawing.size();

    drawing.set_center((size.width as f64 / 4., size.height as f64 / 2.));
    
    let mut turtle = drawing.add_turtle();
    turtle.hide();
    turtle.set_heading(60.);
    turtle.set_speed("instant");

    let mut positions = VecDeque::new();
    
    use BarnsleyFernAlphabet::*;
    for token in axiom.iter() {
        match token {
            A => {  },
            B => { turtle.forward(2.); },
            Left => { turtle.right(25.); },
            Right => { turtle.left(25.); },
            Open => { positions.push_front((turtle.position(), turtle.heading())); },
            Close => {
                let (position, heading) = positions.pop_front().unwrap();

                turtle.pen_up();
                turtle.go_to(position);
                turtle.pen_down();
                turtle.set_heading(heading);
            }
        }
    }
}

fn main() {
    use BarnsleyFernAlphabet::*;
    let mut axiom = Axiom::from(A);

    let rules = rules!(
        A => B : Left : Open : Open : A : Close : Right : A : Close : Right : B : Open : Right : B : A : Close : Left : A,
        B => B : B
    );

    for _n in 0..DEPTH { axiom.rewrite(&rules); }

    draw_barnsley_fern(&axiom);
}

//(A → BLeft Open Open AClose Right AClose Right BOpen Right BAClose Left A), (B → BB)
use lindenmayer_system_framework::{
    rules, 
    Axiom, 
    rewrite
};

use turtle::{
    Speed, 
    Drawing
};

#[derive(Clone, Copy, PartialEq, Debug)]
enum CantorSetAlphabet { A, B }

fn draw_cantor_set(axioms: &Vec<Axiom<CantorSetAlphabet>>) {
    use CantorSetAlphabet::*;

    let mut drawing = Drawing::new();

    let size = drawing.size();

    let mut turtle = drawing.add_turtle();

    turtle.hide();
    turtle.set_speed(Speed::from(25));
    turtle.set_pen_size((size.height as usize / axioms.len()) as f64);
    turtle.pen_up();
    turtle.go_to((0., 0.));
    turtle.pen_down();

    for (idx, step) in axioms.iter().enumerate() {
        turtle.set_heading(180.);
        let distance = (size.width as usize / step.len()) as f64;
        for token in step {
            match token {
                A => {
                    turtle.forward(distance);
                },
                B => {
                    turtle.pen_up();
                    turtle.forward(distance);
                    turtle.pen_down();
                },
            }
        }

        turtle.pen_up();
        turtle.go_to((0., (size.height as usize * idx / axioms.len()) as f64));
        turtle.pen_down();
    }

}

const DEPTH: i32 = 6;

fn main() {
    use CantorSetAlphabet::*;
    let mut axioms = vec![Axiom::from(A)];

    let rules = rules!(
        A => A : B : A,
        B => B : B : B
    );

    for _n in 0..DEPTH {
        axioms.push(rewrite(&rules, axioms.last().unwrap().clone()));
    }

    draw_cantor_set(&axioms);
}
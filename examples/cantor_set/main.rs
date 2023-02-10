use lindenmayer_system_framework::{
    rules, 
    Axiom, 
    rewrite
};

use turtle::Drawing;

#[derive(Clone, Copy, PartialEq, Debug)]
enum CantorSetAlphabet { A, B }

fn draw_cantor_set(axioms: &Vec<Axiom<CantorSetAlphabet>>) {
    use CantorSetAlphabet::*;

    let mut drawing = Drawing::new();
    let size = drawing.size();
    drawing.set_center((size.width as f64 * -0.5, size.height as f64 * 0.5));

    let mut turtle = drawing.add_turtle();
    turtle.hide();
    turtle.set_speed("instant");
    turtle.pen_up();
    turtle.go_to((0., 0.));
    turtle.pen_down();
    turtle.set_heading(180.);

    for (idx, step) in axioms.iter().enumerate() {
        let distance = size.width as f64 / step.size() as f64;

        let start_y = ((size.height as usize * idx) as f64 / axioms.len() as f64) as i32;
        let end_y = ((size.height as usize * (idx + 1)) as f64 / axioms.len() as f64) as i32;

        for i in start_y..end_y {
            turtle.pen_up();
            turtle.go_to((0., i as f64));
            turtle.pen_down();

            for token in step {
                match token {
                    A => { turtle.forward(distance); },
                    B => {
                        turtle.pen_up();
                        turtle.forward(distance);
                        turtle.pen_down();
                    },
                }
            }
        }
    }

}

const DEPTH: i32 = 9;

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
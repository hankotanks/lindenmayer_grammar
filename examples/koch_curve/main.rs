use lindenmayer_grammar::{Axiom, rules};

use turtle::Drawing;

const DEPTH: u32 = 5;

fn draw_koch_curve(axiom: &Axiom<i32>) {
    let mut drawing = Drawing::new();

    let size = drawing.size();

    drawing.set_center(((size.width as f64 / 2.), (size.height as f64 / 3.)));
    drawing.set_title("Koch Curve");
    
    let dist = size.width as f64 / 3u32.pow(DEPTH) as f64;

    let mut turtle = drawing.add_turtle();

    turtle.set_speed("instant");
    turtle.hide();
    turtle.set_heading(0.);

    for token in axiom.iter() {
        match token {
            0 => turtle.forward(dist),
            1 => turtle.left(90.),
            2 => turtle.right(90.),
            _ => unreachable!()
        }
    }
}

fn main() {
    let mut axiom = Axiom::from(0);

    let rules = rules!(0 => 0:1:0:2:0:2:0:1:0);

    for _n in 0..DEPTH { axiom.rewrite(&rules); }

    draw_koch_curve(&axiom);
}
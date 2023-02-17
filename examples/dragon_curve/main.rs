use lindenmayer_grammar::{Axiom, rules};

use turtle::Drawing;

const DEPTH: i32 = 8;

fn draw_dragon_curve(axiom: &Axiom<i32>) {
    let mut drawing = Drawing::new();

    let size = drawing.size();

    let dist = size.width as f64 / 100.;

    let mut turtle = drawing.add_turtle();

    turtle.hide();
    turtle.set_speed("instant");
    turtle.set_heading(180.);

    for token in axiom.iter() {
        match token {
            0 | 1 => turtle.forward(dist),
            2 => turtle.left(90.),
            3 => turtle.right(90.),
            _ => unreachable!()
        }
    }
}

fn main() {
    let mut axiom = Axiom::from(0);

    let rules = rules!(
        0 => 0 : 2 : 1,
        1 => 0 : 3 : 1
    );

    for _n in 0..DEPTH { axiom.rewrite(&rules); }

    draw_dragon_curve(&axiom);
}

//(F → F+G), (G → F-G)
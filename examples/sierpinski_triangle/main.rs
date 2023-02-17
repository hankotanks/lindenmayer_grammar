use turtle::Drawing;
use lindenmayer_grammar::{axiom, Axiom, rules};

const DEPTH: i32 = 7;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
enum SierpinskiAlphabet { A, B, P, M }

fn draw_sierpinski_triangle(axiom: &Axiom<SierpinskiAlphabet>) {

    let mut drawing = Drawing::new();

    let size = drawing.size();

    drawing.set_center(((size.width as f64 / 2.), (size.height as f64 / 2.)));

    let dist = size.width.min(size.height) as f64;
    let dist = dist / 2f64.powi(DEPTH);

    let mut turtle = drawing.add_turtle();

    turtle.set_speed("instant");
    turtle.hide();
    turtle.set_heading(60.);

    use SierpinskiAlphabet::*;
    for token in axiom.iter() {
        match token {
            A | B => turtle.forward(dist),
            P => turtle.left(120.),
            M => turtle.right(120.)
        }
    }
}

fn main() {
    use SierpinskiAlphabet::*;
    let mut axiom = axiom!(A, M, B, M, B);

    let rules = rules!(
        A => A : M : B : P : A : P : B : M : A,
        B => B : B
    );

    for _n in 0..DEPTH { axiom.rewrite(&rules); }

    draw_sierpinski_triangle(&axiom);
}
// (F → F−G+F+G−F), (G → GG)
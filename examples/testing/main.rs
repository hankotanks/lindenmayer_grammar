use lindenmayer_grammar::{Axiom, rules};

fn main() {
    let axiom = Axiom::with_elements([0, 1, 0]);

    let rules = rules!(|0| 1 |0| => 2);

    dbg!(axiom.rewrite_with_context(&rules));
}
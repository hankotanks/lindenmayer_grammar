use std::fmt::Display;

pub struct Production<A: Clone + Copy + PartialEq + Display> {
    matcher: Vec<A>,
    transcriber: Vec<A>
}

impl<A> Production<A> where A: Clone + Copy + PartialEq + Display {
    pub fn new(matcher: Vec<A>, transcriber: Vec<A>) -> Self {
        Self { matcher, transcriber }
    }
}

pub fn rewrite<A: Clone + Copy + PartialEq + Display>(rules: &[Production<A>], mut axiom: Vec<A>) -> Vec<A> {
    let mut output: Vec<A> = Vec::new();

    'token: while !axiom.is_empty() {
        for rule in rules.iter() {
            if let Some(token_length) = matcher(rule, &axiom) {
                axiom.drain(0..token_length);
                output.append(&mut rule.transcriber.clone());
                
                continue 'token;
            }
        }

        output.push(axiom.remove(0));
    }

    output
}

fn matcher<A: Clone + Copy + PartialEq + Display>(rule: &Production<A>, axiom: &[A]) -> Option<usize> {
    for i in 1..=axiom.len() {
        if rule.matcher == &axiom[0..i] {
            return Some(i);
        }
    }

    None
}

#[macro_export]
macro_rules! production {
    ($a:path $(: $b:path)*) => {
        {
            use lindenmayer_system_framework::Production;
        
            let mut matcher = vec![$a];
            $(matcher.push($b);)*
    
            Production::new(matcher.clone(), matcher)
        }
    };
    ($a:path $(: $b:path)* => $c:path $(: $d:path)*) => {
        {
            use lindenmayer_system_framework::Production;

            let mut matcher = vec![$a];
            $(matcher.push($b);)*

            let mut transcriber = vec![$c];
            $(transcriber.push($d);)*

            Production::new(matcher, transcriber)
        }
    };
}
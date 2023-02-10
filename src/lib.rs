use std::{fmt::Display, cell::Cell};

pub struct Axiom<A>(Vec<A>, Cell<usize>) where A: Clone + Copy + PartialEq + Display;

impl<A> From<A> for Axiom<A> where A: Clone + Copy + PartialEq + Display {
    fn from(token: A) -> Self {
        Self(vec![token], Cell::new(0))
    }
}

impl<A> From<Vec<A>> for Axiom<A> where A: Clone + Copy + PartialEq + Display {
    fn from(tokens: Vec<A>) -> Self {
        Self(tokens, Cell::new(0))
    }
}

impl<A> Iterator for Axiom<A> where A: Clone + Copy + PartialEq + Display {
    type Item = A;

    fn next(&mut self) -> Option<Self::Item> {
        let item = self.0.get(self.1.get()).copied();

        if item.is_none() { 
            self.1.set(0); 
        } else {
            self.1.set(self.1.get() + 1);
        }

        item
    }
}

pub struct Ruleset<A>(Vec<Production<A>>) where A: Clone + Copy + PartialEq + Display;

impl<A> From<Production<A>> for Ruleset<A> where A: Clone + Copy + PartialEq + Display {
    fn from(rule: Production<A>) -> Self {
        Self(vec![rule])
    }
}

impl<A> From<Vec<Production<A>>> for Ruleset<A> where A: Clone + Copy + PartialEq + Display {
    fn from(rules: Vec<Production<A>>) -> Self {
        Self(rules)
    }
}

pub struct Production<A: Clone + Copy + PartialEq + Display> {
    matcher: Vec<A>,
    transcriber: Vec<A>
}

impl<A> Production<A> where A: Clone + Copy + PartialEq + Display {
    pub fn new(matcher: Vec<A>, transcriber: Vec<A>) -> Self {
        Self { matcher, transcriber }
    }
}

#[inline]
fn matcher<A: Clone + Copy + PartialEq + Display>(rule: &Production<A>, slice: &[A]) -> Option<usize> {
    for i in 1..=slice.len() {
        if rule.matcher == &slice[0..i] {
            return Some(i);
        }
    }

    None
}

pub fn rewrite<A: Clone + Copy + PartialEq + Display>(rules: &Ruleset<A>, mut axiom: Axiom<A>) -> Axiom<A> {
    let mut output: Vec<A> = Vec::new();

    'token: while !axiom.0.is_empty() {
        for rule in rules.0.iter() {
            if let Some(token_length) = matcher(rule, &axiom.0) {
                axiom.0.drain(0..token_length);
                output.append(&mut rule.transcriber.clone());
                
                continue 'token;
            }
        }

        output.push(axiom.0.remove(0));
    }

    output.into()
}

pub fn rewrite_in_place<A: Clone + Copy + PartialEq + Display>(rules: &Ruleset<A>, axiom: &mut Axiom<A>) {
    let mut size = axiom.0.len() as i32;

    let mut pointer = 0;
    'token: while (pointer as i32) < size {
        for rule in rules.0.iter() {
            if let Some(token_length) = matcher(rule, &axiom.0[pointer..]) {
                axiom.0.drain(pointer..(pointer + token_length));
                rule.transcriber.iter().rev().for_each(|token| axiom.0.insert(pointer, *token));

                pointer += rule.transcriber.len();
                size += rule.transcriber.len() as i32 - token_length as i32;
                
                continue 'token;
            }
        }

        pointer += 1;
    }
}

#[macro_export]
macro_rules! rules {
    ($($a:path $(: $b:path)* => $c:path $(: $d:path)*),+) => {
        {
            use lindenmayer_system_framework::{production, Ruleset};

            let mut rules = Vec::new();
            $(rules.push(production!($a $(: $b)* => $c $(: $d)*));)+
            
            Ruleset::from(rules)
        }
    }
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
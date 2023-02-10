use std::{
    cell::Cell, 
    fmt::Debug
};

pub trait Alphabet: Clone + Copy + PartialEq + Debug {  }

impl<T> Alphabet for T where T: Clone + Copy + PartialEq + Debug {  }

#[derive(Clone, PartialEq, Eq)]
pub struct Axiom<A>(Vec<A>, Cell<usize>) where A: Alphabet;

impl<A> From<A> for Axiom<A> where A: Alphabet {
    fn from(token: A) -> Self {
        Self(vec![token], Cell::new(0))
    }
}

impl<A> From<Vec<A>> for Axiom<A> where A: Alphabet {
    fn from(tokens: Vec<A>) -> Self {
        Self(tokens, Cell::new(0))
    }
}

impl<A> Iterator for Axiom<A> where A: Alphabet {
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

impl<A> Iterator for &Axiom<A> where A: Alphabet {
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

impl<A> Debug for Axiom<A> where A: Alphabet {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_list().entries(self).finish()
    }
}

#[derive(Clone, PartialEq, Eq)]
pub struct Production<A: Alphabet> {
    matcher: Axiom<A>,
    transcriber: Axiom<A>
}

impl<A> Production<A> where A: Alphabet {
    pub fn new(matcher: Axiom<A>, transcriber: Axiom<A>) -> Self {
        Self { matcher, transcriber }
    }
}

impl<A> Debug for Production<A> where A: Alphabet {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let matcher = format!("{:?}", &self.matcher).replace(',', " :");
        let transcriber = format!("{:?}", &self.transcriber).replace(',', " :");

        write!(f, "{} => {}", 
            matcher.trim_start_matches('[').trim_end_matches(']'), 
            transcriber.trim_start_matches('[').trim_end_matches(']')
        )
    }
}

#[derive(Clone)]
pub struct Ruleset<A>(Vec<Production<A>>) where A: Alphabet;

impl<A> Default for Ruleset<A> where A: Alphabet {
    fn default() -> Self {
        Self(Default::default())
    }
}

impl<A> Ruleset<A> where A: Alphabet {
    pub fn add(&mut self, rule: Production<A>) {
        if !self.0.contains(&rule) {
            self.0.push(rule);
        }
    }
}

impl<A> From<Production<A>> for Ruleset<A> where A: Alphabet {
    fn from(rule: Production<A>) -> Self {
        Self(vec![rule])
    }
}

impl<A> From<Vec<Production<A>>> for Ruleset<A> where A: Alphabet {
    fn from(rules: Vec<Production<A>>) -> Self {
        Self(rules)
    }
}

impl<A> Debug for Ruleset<A> where A: Alphabet {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut dbg_builder = f.debug_struct("Rules");
        for (idx, item) in self.0.iter().enumerate() {
            dbg_builder.field(&format!("{}", idx), item);
        }
        
        dbg_builder.finish()
    }
}

#[inline]
fn matcher<A: Alphabet>(rule: &Production<A>, slice: &[A]) -> Option<usize> {
    (1..=slice.len()).find(|&i| rule.matcher.0 == slice[0..i])
}

pub fn rewrite<A: Alphabet>(rules: &Ruleset<A>, mut axiom: Axiom<A>) -> Axiom<A> {
    let mut output: Vec<A> = Vec::new();

    'token: while !axiom.0.is_empty() {
        for rule in rules.0.iter() {
            if let Some(token_length) = matcher(rule, &axiom.0) {
                axiom.0.drain(0..token_length);
                output.append(&mut rule.transcriber.0.clone());
                
                continue 'token;
            }
        }

        output.push(axiom.0.remove(0));
    }

    output.into()
}

pub fn rewrite_in_place<A: Alphabet>(rules: &Ruleset<A>, axiom: &mut Axiom<A>) {
    let mut size = axiom.0.len() as i32;

    let mut pointer = 0;
    'token: while (pointer as i32) < size {
        for rule in rules.0.iter() {
            if let Some(token_length) = matcher(rule, &axiom.0[pointer..]) {
                axiom.0.drain(pointer..(pointer + token_length));
                rule.transcriber.0.iter().rev().for_each(|token| { 
                    axiom.0.insert(pointer, *token); 
                } );

                pointer += rule.transcriber.0.len();
                size += rule.transcriber.0.len() as i32 - token_length as i32;
                
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
            use lindenmayer_system_framework::{Axiom, Production};
        
            let mut matcher = vec![$a];
            $(matcher.push($b);)*
    
            Production::new(Axiom::from(matcher.clone()), Axiom::from(matcher))
        }
    };
    ($a:path $(: $b:path)* => $c:path $(: $d:path)*) => {
        {
            use lindenmayer_system_framework::{Axiom, Production};

            let mut matcher = vec![$a];
            $(matcher.push($b);)*

            let mut transcriber = vec![$c];
            $(transcriber.push($d);)*

            Production::new(Axiom::from(matcher), Axiom::from(transcriber))
        }
    };
}
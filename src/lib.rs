//! A minimal implementation of the Lindenmayer grammar using Enum alphabets
//! 
//! L-systems can be defined by three attributes
//! - An [Alphabet] of symbols[^a]
//! - A set of [Production] rules, called a [Ruleset]
//! - An intial [Axiom]
//! 
//! # Example
//! 
//! Below is a simple implementation of a binary fractal tree L-system.
//! 
//! ```
//! use lindenmayer_grammar::{Axiom, rules};
//! 
//! // Define the alphabet and derive necessary traits
//! #[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
//! enum BFTAlphabet { Leaf, Node, Left, Right }
//! 
//! fn main() {
//!     use BFTAlphabet::*;
//! 
//!     // The L-system initiator
//!     let mut axiom = Axiom::from(Leaf);
//! 
//!     // Declare the system's productions with the rules! macro
//!     let rules = rules!(
//!         Node => Node : Node,
//!         Leaf => Node : Left : Leaf : Right : Leaf
//!     );
//! 
//!     // The axiom is rewritten using the defined rules, resulting in
//!     // [Node, Left, Leaf, Right, Leaf]
//!     axiom.rewrite(&rules); 
//! }
//! ```
//! 
//! [^a]: In this crate, any type which implements `Clone` and `Ord` is considered an Alphabet. Any integer type is ideal for this use case, but Enums can work too!

use std::{
    cell::Cell, 
    fmt::Debug, 
    collections::BTreeSet
};

/// An internal trait that is automatically implied for all types that can be used as valid alphabets.
/// 
/// Any type which implements `Clone`, and `Ord` is considered an Alphabet. This trait does not need to be derived.
/// 
/// # Examples
/// 
/// ```
/// #[derive(Clone, Copy, PartialEq, Debug)]
/// enum Koch { Forward, Left, Right }
/// ```
pub trait Alphabet: Clone + Ord {  }

impl<T> Alphabet for T where T: Clone + Ord {  }

/// A sentence of symbols from a single [Alphabet].
/// 
/// An axiom can be initialized from a single symbol or a collection of symbols.
/// When provided a [Ruleset], an Axiom can be rewritten, either with the creation of a new Axiom (through the associated [step](#method:step) method) or mutably with [rewrite](#method:rewrite).
/// 
/// The collection of symbols that underpin an Axiom cannot be accessed, although they can be turned into iterators with the associated [iter](#method:iter) method.
/// 
/// # Examples
/// 
/// ```
/// use lindenmayer_grammar::{Axiom, rules};
/// 
/// // i32 implements the requisite traits
/// let mut axiom = Axiom::from(vec![0, 1, 0]);
/// 
/// for _ in 0..3 {
///     // Rewrite the axiom using Lindenmayer's algae model
///     axiom.rewrite(&rules!(0 => 0 : 1, 1 => 0));
/// }
/// 
/// // We can iterate through the axiom, printing each element...
/// for symbol in &axiom { print!("{:?}, ", symbol); }
/// 
/// // The iterator can still be collected
/// let elements = axiom.collect::<Vec<_>>();
/// ```
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Axiom<A>(Vec<A>) where A: Alphabet;

impl<A> Axiom<A> where A: Alphabet {
    pub fn size(&self) -> usize {
        self.0.len()
    }

    pub fn iter(&self) -> AxiomIterator<'_, A> {
        AxiomIterator::new(self)
    }
}

impl<A> From<A> for Axiom<A> where A: Alphabet {
    fn from(token: A) -> Self {
        Self(vec![token])
    }
}

impl<A> From<Vec<A>> for Axiom<A> where A: Alphabet {
    fn from(tokens: Vec<A>) -> Self {
        Self(tokens)
    }
}

impl<A> From<&[A]> for Axiom<A> where A: Alphabet {
    fn from(tokens: &[A]) -> Self {
        Self(tokens.to_vec())
    }
}

impl<A> Debug for Axiom<A> where A: Alphabet + Debug {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_list().entries(self.iter()).finish()
    }
}

#[inline]
fn matcher<A: Alphabet>(rule: &Production<A>, slice: &[A]) -> Option<usize> {
    (1..=slice.len()).find(|&i| rule.matcher.0 == slice[0..i])
}

impl<A> Axiom<A> where A: Alphabet {
    /// Produces a new Axiom from the provided [Ruleset].
    /// 
    /// Can be used when the initial axiom shouldn't be consumed.
    /// For example, if we want to accumulate all generations of an L-system.
    /// 
    /// Axioms can be rewritten in place using their associated [rewrite](#method:rewrite) method.
    /// 
    /// # Examples
    /// 
    /// ```
    /// use lindenmayer_grammar::{Axiom, rules};
    /// 
    /// let mut axioms = vec![Axiom::from(0)];
    /// 
    /// // Define the Ruleset for Lindenmayer's algae model
    /// let rules = rules!(0 => 0 : 1, 1 => 0);
    /// 
    /// for _ in 0..3 {
    ///     // Each step, push the new axiom to the list of previous axioms
    ///     axioms.push(axioms.last().unwrap().step(&rules));
    /// }
    /// ```
    pub fn step(&self, rules: &Ruleset<A>) -> Axiom<A> {
        let mut output: Vec<A> = Vec::new();
    
        let mut pointer = 0;
        'token: while pointer < self.0.len() {
            for rule in rules.0.iter() {
                if let Some(token_length) = matcher(rule, &self.0[pointer..]) {
                    pointer += token_length;
                    output.append(&mut rule.transcriber.0.clone());
                    
                    continue 'token;
                }
            }
    
            output.push(self.0[pointer].clone());
        }
    
        output.into()
    }

    /// Rewrites the [Axiom] using the given [Ruleset]
    /// 
    /// Can be used to quickly step through all generations of an L-system to a desired point.
    /// 
    /// The associated method [step](#method:step) produces a new [Axiom] from the original.
    /// 
    /// # Examples
    /// 
    /// ```
    /// use lindenmayer_grammar::{Axiom, rules};
    /// 
    /// // Define the Ruleset for Lindenmayer's algae model
    /// let rules = rules!(0 => 0 : 1, 1 => 0);
    /// 
    /// // Create the system's initiator
    /// let mut axiom = Axiom::from(0);
    /// 
    /// // Continue to rewrite it until its length exceeds 100 symbols
    /// while axiom.len() < 100 { axiom.rewrite(&rules); }
    /// ```
    pub fn rewrite(&mut self, rules: &Ruleset<A>) {
        let mut size = self.0.len() as i32;

        let mut pointer = 0;
        'token: while (pointer as i32) < size {
            for rule in rules.0.iter() {
                if let Some(token_length) = matcher(rule, &self.0[pointer..]) {
                    self.0.drain(pointer..(pointer + token_length));
                    rule.transcriber.0.iter().cloned().rev().for_each(|token| { 
                        self.0.insert(pointer, token); 
                    } );

                    pointer += rule.transcriber.0.len();
                    size += rule.transcriber.0.len() as i32 - token_length as i32;
                    
                    continue 'token;
                }
            }

            pointer += 1;
        }
    }

    /// Returns the length of the current [Axiom]
    /// 
    /// # Examples
    /// 
    /// ```
    /// use lindenmayer_grammar::Axiom;
    /// 
    /// assert_eq!(Axiom::from(vec![0, 1, 0, 1]).len(), 4);
    /// ```
    pub fn len(&self) -> usize {
        self.0.len()
    }
}

/// Iterates over an Axiom's symbols.
/// Borrows the [Axiom] for the iterators lifetime.
/// 
/// # Examples
/// 
/// ```
/// use lindenmayer_grammar::{Axiom, axiom};
/// 
/// // Create a new Axiom with a macro
/// let axiom: Axiom<i32> = axiom!(0, 1, 0, 0, 1, 0, 1, 0);
/// 
/// // Count the difference in token count
/// axiom.iter().fold(0, |diff, &t| diff + if t == 0 { -1 } else { 1 } );
/// ```
pub struct AxiomIterator<'a, A>(&'a Axiom<A>, Cell<usize>) where A: Alphabet;

impl<'a, A> Iterator for AxiomIterator<'a, A> where A: Alphabet {
    type Item = &'a A;

    fn next(&mut self) -> Option<Self::Item> {
        let item = self.0.0.get(self.1.get());

        if item.is_none() { 
            self.1.set(0); 
        } else {
            self.1.set(self.1.get() + 1);
        }

        item
    }
}

impl<'a, A> Iterator for &AxiomIterator<'a, A> where A: Alphabet {
    type Item = &'a A;

    fn next(&mut self) -> Option<Self::Item> {
        let item = self.0.0.get(self.1.get());

        if item.is_none() { 
            self.1.set(0); 
        } else {
            self.1.set(self.1.get() + 1);
        }

        item
    }
}

impl<'a, A> AxiomIterator<'a, A> where A: Alphabet {
    fn new(axiom: &'a Axiom<A>) -> Self {
        Self(axiom, Cell::new(0))
    }
}

/// A rule that dictates how symbols are rewritten in an L-System.
/// 
/// Each [Production] consists of two axioms
/// - the matcher
/// - the transcriber
/// 
/// When the matcher is found in a given [Axiom], it is replaced with the transcriber.
/// Productions are most easily defined using the [production!] macro.
/// 
/// Productions cannot be applied to axioms on their own, they must be compiled into a [Ruleset] first.
/// 
/// # Examples
/// 
/// ```
/// use lindenmayer_grammar::{Axiom, Ruleset, Production, production};
/// 
/// let mut axiom = Axiom::from(0);
/// 
/// // Create two productions. The former uses the struct definition, the second uses a macro
/// let p1 = Production::new(Axiom::from(0), Axiom::from(vec![0, 1]));
/// let p2 = production!(1 => 0);
/// 
/// // Compile the two productions into a Ruleset
/// let rules = Ruleset::from(vec![p1, p2]);
/// 
/// // Apply the Ruleset
/// axiom.rewrite(&rules);
/// ```
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Production<A: Alphabet> {
    matcher: Axiom<A>,
    transcriber: Axiom<A>
}

impl<A> Production<A> where A: Alphabet {
    /// Creates a new [Production] from two Axioms
    /// 
    /// Consider using the more concise [production!] macro syntax instead.
    /// 
    /// # Examples
    /// 
    /// ```
    /// use lindenmayer_grammar::{Axiom, Production, production};
    /// 
    /// // Create two identical productions. The former uses the struct definition, the second uses a macro
    /// let p1 = Production::new(Axiom::from(0), Axiom::from(vec![0, 1]));
    /// let p2 = production!(0 => 0 : 1);
    /// 
    /// // The two productions are equal
    /// assert_eq!(p1, p2);
    /// ```
    pub fn new(matcher: Axiom<A>, transcriber: Axiom<A>) -> Self {
        Self { matcher, transcriber }
    }
}

impl<A> Debug for Production<A> where A: Alphabet + Debug {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let matcher = format!("{:?}", &self.matcher).replace(',', " :");
        let transcriber = format!("{:?}", &self.transcriber).replace(',', " :");

        write!(f, "{} => {}", 
            matcher.trim_start_matches('[').trim_end_matches(']'), 
            transcriber.trim_start_matches('[').trim_end_matches(']')
        )
    }
}

/// An ordered collection of [Production] rules.
/// 
/// Can be created from a single production, a collection of productions, or through the [rules!] macro.
/// 
/// Identity productions are implicit, and do not need to be added to the Ruleset.
/// 
/// # Examples
/// 
/// ```
/// use lindenmayer_grammar::{Ruleset, rules, production};
/// 
/// // The following Ruleset initializations are equivalent
/// let rs = rules!(0 => 0 : 1, 1 => 0);
/// let rs = Ruleset::from(vec![production!(0 => 0 : 1), production!(1 => 0)]);
/// ```
#[derive(Clone)]
pub struct Ruleset<A>(BTreeSet<Production<A>>) where A: Alphabet;

impl<A> Default for Ruleset<A> where A: Alphabet {
    fn default() -> Self {
        Self(Default::default())
    }
}

impl<A> From<Production<A>> for Ruleset<A> where A: Alphabet {
    fn from(rule: Production<A>) -> Self {
        let mut ruleset = BTreeSet::new();

        ruleset.insert(rule);

        Self(ruleset)
    }
}

impl<A> From<Vec<Production<A>>> for Ruleset<A> where A: Alphabet {
    fn from(rules: Vec<Production<A>>) -> Self {
        let mut ruleset = BTreeSet::new();

        for rule in rules.into_iter() {
            ruleset.insert(rule);
        }

        Self(ruleset)
    }
}

impl<A> From<&[Production<A>]> for Ruleset<A> where A: Alphabet {
    fn from(rules: &[Production<A>]) -> Self {
        let mut ruleset = BTreeSet::new();

        for rule in rules.iter().cloned() {
            ruleset.insert(rule);
        }

        Self(ruleset)
    }
}

impl<A> From<BTreeSet<Production<A>>> for Ruleset<A> where A: Alphabet {
    fn from(rules: BTreeSet<Production<A>>) -> Self {
        Ruleset(rules)
    }
}

impl<A> Debug for Ruleset<A> where A: Alphabet + Debug {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut dbg_builder = f.debug_struct("Rules");

        for (idx, item) in self.0.iter().enumerate() {
            dbg_builder.field(&format!("{}", idx), item);
        }
        
        dbg_builder.finish()
    }
}

/// Builds a [Ruleset] from a comma-separated list of [Production] statements.
/// 
/// Matcher and transcriber axioms should be separated by a `=>` symbol.
/// 
/// The elements of the two components should be separated with a colon.
/// 
/// # Examples
/// 
/// ```
/// use lindenmayer_grammar::rules;
///
/// rules!(0 => 0 : 1, 1 => 0);
/// ```
#[macro_export]
macro_rules! rules {
    ($($a:literal $(: $b:literal)* => $c:literal $(: $d:literal)*),+) => {
        {
            use std::collections::BTreeSet;

            use lindenmayer_grammar::{production, Ruleset};

            let mut ruleset = BTreeSet::new();
            $(ruleset.insert(production!($a $(: $b)* => $c $(: $d)*));)+
            
            Ruleset::from(ruleset)
        }
    };
    ($($a:path $(: $b:path)* => $c:path $(: $d:path)*),+) => {
        {
            use std::collections::BTreeSet;

            use lindenmayer_grammar::{production, Ruleset};

            let mut ruleset = BTreeSet::new();
            $(ruleset.insert(production!($a $(: $b)* => $c $(: $d)*));)+
            
            Ruleset::from(ruleset)
        }
    };
}

/// A wrapper to create a single [Production].
/// 
/// Matcher and transcriber axioms should be separated by a `=>` symbol.
/// 
/// The elements of the two components should be separated with a colon.
/// 
/// # Examples
/// 
/// ```
/// use lindenmayer_grammar::{Axiom, Production, production};
/// 
/// production!(0 => 0 : 1);
/// 
/// // This is equivalent to...
/// Production::new(Axiom::from(0), Axiom::from(vec![0, 1]));
/// ```
#[macro_export]
macro_rules! production {
    ($a:literal $(: $b:literal)*) => {
        {
            use lindenmayer_grammar::{Axiom, Production};
        
            let mut matcher = vec![$a];
            $(matcher.push($b);)*
    
            Production::new(Axiom::from(matcher.clone()), Axiom::from(matcher))
        }
    };
    ($a:literal $(: $b:literal)* => $c:literal $(: $d:literal)*) => {
        {
            use lindenmayer_grammar::{Axiom, Production};

            let mut matcher = vec![$a];
            $(matcher.push($b);)*

            let mut transcriber = vec![$c];
            $(transcriber.push($d);)*

            Production::new(Axiom::from(matcher), Axiom::from(transcriber))
        }
    };
    ($a:path $(: $b:path)*) => {
        {
            use lindenmayer_grammar::{Axiom, Production};
        
            let mut matcher = vec![$a];
            $(matcher.push($b);)*
    
            Production::new(Axiom::from(matcher.clone()), Axiom::from(matcher))
        }
    };
    ($a:path $(: $b:path)* => $c:path $(: $d:path)*) => {
        {
            use lindenmayer_grammar::{Axiom, Production};

            let mut matcher = vec![$a];
            $(matcher.push($b);)*

            let mut transcriber = vec![$c];
            $(transcriber.push($d);)*

            Production::new(Axiom::from(matcher), Axiom::from(transcriber))
        }
    };
}

/// Analagous to `Axiom::from()`, but does not require the instantiation of an intermediate Vec.
/// 
/// # Examples
/// 
/// ```
/// use lindenmayer_grammar::{Axiom, axiom};
/// 
/// axiom!(0, 1, 0);
/// 
/// // This is equivalent to...
/// Axiom::from(vec![0, 1, 0]);
/// ```
#[macro_export]
macro_rules! axiom {
    ($($a:literal),+) => {
        {
            use lindenmayer_grammar::{Axiom, Production};

            let mut elements = Vec::new();
            $(elements.push($a);)+

            Axiom::from(elements)
        }
    };
    ($($a:path),+) => {
        {
            use lindenmayer_grammar::{Axiom, Production};

            let mut elements = Vec::new();
            $(elements.push($a);)+

            Axiom::from(elements)
        }
    };
}
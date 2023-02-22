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
//! use BFTAlphabet::*;
//!
//! // The L-system initiator
//! let mut axiom = Axiom::from(Leaf);
//!
//! // Declare the system's productions with the rules! macro
//! let rules = rules!(
//!     Node => Node : Node,
//!     Leaf => Node : Left : Leaf : Right : Leaf
//! );
//!
//! // The axiom is rewritten using the defined rules, resulting in
//! // [Node, Left, Leaf, Right, Leaf]
//! axiom.rewrite(&rules); 

//! ```
//! 
//! [^a]: In this crate, any type which implements `Clone` and `Ord` is considered an Alphabet. Any integer type is ideal for this use case, but Enums can work too!

use std::{
    fmt::Debug, 
    collections::{BTreeSet, BTreeMap, VecDeque}, 
    vec::IntoIter, 
    iter::once, 
    slice::Iter
};

use ordered_float::OrderedFloat;
use rand::{thread_rng, Rng};
use raqote::{DrawTarget, PathBuilder, Source, SolidSource, StrokeStyle, DrawOptions, Path};
use show_image::{Image, context, WindowProxy};

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
    pub fn new(symbol: A) -> Self {
        Self(vec![symbol])
    }

    pub fn with_elements<I: IntoIterator<Item = A>>(collection: I) -> Self {
        Self(collection.into_iter().collect::<Vec<_>>())
    }

    pub fn iter(&self) -> Iter<A> {
        self.0.iter()
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

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

impl<A> Debug for Axiom<A> where A: Alphabet + Debug {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_list().entries(self.0.iter()).finish()
    }
}

impl<A> IntoIterator for Axiom<A> where A: Alphabet {
    type Item = A;
    type IntoIter = IntoIter<A>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
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

        let mut prng = thread_rng();
    
        let mut pointer = 0;
        'token: while pointer < self.0.len() {
            for rule in rules.0.iter() {
                if let Some(token_length) = rule.matcher(&self.0[pointer..]) {
                    if prng.gen::<f32>() < rule.probability.0 {
                        output.append(&mut rule.transcriber.0.clone());
                    }

                    pointer += token_length;
                    
                    continue 'token;
                }
            }
    
            output.push(self.0[pointer].clone());
        }
    
        Self::with_elements(output)
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

        let mut prng = thread_rng();

        let mut pointer = 0;
        'token: while (pointer as i32) < size {
            for rule in rules.0.iter() {
                if let Some(token_length) = rule.matcher(&self.0[pointer..]) {
                    if prng.gen::<f32>() < rule.probability.0 {
                        self.0.drain(pointer..(pointer + token_length));
                        rule.transcriber.0.iter().cloned().rev().for_each(|token| { 
                            self.0.insert(pointer, token); 
                        } );
                    }

                    pointer += rule.transcriber.0.len();
                    size += rule.transcriber.0.len() as i32 - token_length as i32;
                    
                    continue 'token;
                }
            }

            pointer += 1;
        }
    }

    pub fn visualize(&self, turtle: Turtle<A>) -> Drawing {
        let mut state = VecDeque::new();

        let mut minima = (0., 0.);
        let mut maxima = (0., 0.);

        let mut actions = Vec::new();

        let mut position = (0f32, 0f32);
        let mut heading = 0f32;

        use TurtleAction::*;
        for symbol in self.iter() {
            for action in turtle.0.get(symbol).unwrap_or(&Vec::new()) {
                actions.push(*action);

                match action {
                    Forward => {
                        position.0 += heading.cos();
                        position.1 += heading.sin(); 
                    },
                    Backward => { 
                        position.0 -= heading.cos();
                        position.1 -= heading.sin(); 
                    },
                    Turn(rad) => { 
                        heading += rad; 
                    },
                    PushState => { 
                        state.push_front((position, heading)); 
                    },
                    PopState => { 
                        (position, heading) = state.pop_front().unwrap(); 
                    },
                    _ => {  }
                }

                minima.0 = position.0.min(minima.0);
                minima.1 = position.1.min(minima.1);
                maxima.0 = position.0.max(maxima.0);
                maxima.1 = position.1.max(maxima.1);
            }
        }

        Drawing::new((maxima.0 - minima.0) as i32, (maxima.1 - minima.1) as i32, (minima.0.abs(), minima.1.abs()), actions)
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
    transcriber: Axiom<A>,
    probability: OrderedFloat<f32>
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
    pub fn new(matcher: Axiom<A>, transcriber: Axiom<A>, probability: f32) -> Self {
        Self { matcher, transcriber, probability: OrderedFloat(probability) }
    }

    fn matcher(&self, slice: &[A]) -> Option<usize> {
        (1..=slice.len()).find(|&i| self.matcher.0 == slice[0..i])
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

impl<A> From<Production<A>> for Ruleset<A> where A: Alphabet {
    fn from(rule: Production<A>) -> Self {
        Self(BTreeSet::from_iter(once(rule)))
    }
}

impl<A, I> From<I> for Ruleset<A> where A: Alphabet, I: IntoIterator<Item = Production<A>> {
    fn from(rules: I) -> Self {
        Self(BTreeSet::from_iter(rules.into_iter()))
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
    ($($a:literal $(: $b:literal)* $([$p:literal])? => $c:literal $(: $d:literal)*),+) => {
        {
            use std::collections::BTreeSet;

            use lindenmayer_grammar::{production, Ruleset};

            let mut ruleset = BTreeSet::new();
            $(ruleset.insert(production!($a $(: $b)* $([$p])? => $c $(: $d)*));)+
            
            Ruleset::from(ruleset)
        }
    };
    ($($a:path $(: $b:path)* $([$p:literal])? => $c:path $(: $d:path)*),+) => {
        {
            use std::collections::BTreeSet;

            use lindenmayer_grammar::{production, Ruleset};

            let mut ruleset = BTreeSet::new();
            $(ruleset.insert(production!($a $(: $b)* $([$p])? => $c $(: $d)*));)+
            
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
    ($a:literal $(: $b:literal)* $([$p:literal])? => $c:literal $(: $d:literal)*) => {
        {
            use lindenmayer_grammar::{Axiom, Production};

            let mut matcher = vec![$a];
            $(matcher.push($b);)*

            let mut transcriber = vec![$c];
            $(transcriber.push($d);)*

            let mut probability = 1.0;
            $(probability = $p;)?

            Production::new(Axiom::with_elements(matcher), Axiom::with_elements(transcriber), probability)
        }
    };
    ($a:path $(: $b:path)* $([$p:literal])? => $c:path $(: $d:path)*) => {
        {
            use lindenmayer_grammar::{Axiom, Production};

            let mut matcher = vec![$a];
            $(matcher.push($b);)*

            let mut transcriber = vec![$c];
            $(transcriber.push($d);)*

            let mut probability = 1.0;
            $(probability = $p;)?

            Production::new(Axiom::with_elements(matcher), Axiom::with_elements(transcriber), probability)
        }
    };
}

#[derive(Clone)]
pub struct Turtle<A>(BTreeMap<A, Vec<TurtleAction>>) where A: Alphabet;

#[derive(Clone, Copy)]
pub enum TurtleAction {
    Forward,
    Backward,
    Turn(f32),
    PushState,
    PopState,
    PenUp,
    PenDown
}

#[derive(Clone)]
pub struct TurtleBuilder<A>(BTreeMap<A, Vec<TurtleAction>>) where A: Alphabet;

impl<A> Default for TurtleBuilder<A> where A: Alphabet {
    fn default() -> Self {
        Self(BTreeMap::new())
    }
}

impl<A> TurtleBuilder<A> where A: Alphabet {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn build(self) -> Turtle<A> {
        Turtle(self.0)
    }

    pub fn assign_action(mut self, symbol: A, action: TurtleAction) -> Self {
        self.0.insert(symbol, vec![action]);
        self
    }

    pub fn assign_action_set<I: IntoIterator<Item = TurtleAction>>(mut self, symbol: A, actions: I) -> Self {
        self.0.insert(symbol, actions.into_iter().collect::<Vec<_>>());
        self
    }
}

pub struct Drawing {
    width: i32,
    height: i32,
    origin: (f32, f32),
    actions: Vec<TurtleAction>
}

impl Drawing {
    pub fn new(width: i32, height: i32, origin: (f32, f32), actions: Vec<TurtleAction>) -> Self {
        Self {
            width,
            height,
            origin, 
            actions
        }
    }

    fn build_path(&self, resolution: f32) -> Path {
        let mut position = (self.origin.0 * resolution + resolution, self.origin.1 * resolution + resolution);
        let mut heading = 0f32;
        let mut active = true;

        let mut pb = PathBuilder::new();
        pb.move_to(position.0, position.1);

        let mut state = VecDeque::new();

        use TurtleAction::*;
        for action in self.actions.iter() {
            match action {
                Forward => {
                    position.0 += heading.cos() * resolution;
                    position.1 += heading.sin() * resolution; 

                    if active {
                        pb.line_to(position.0, position.1);
                    } else {
                        pb.move_to(position.0, position.1);
                    }
                },
                Backward => {
                    position.0 -= heading.cos() * resolution;
                    position.1 -= heading.sin() * resolution; 
                    
                    if active {
                        pb.line_to(position.0, position.1);
                    } else {
                        pb.move_to(position.0, position.1);
                    }
                },
                Turn(rad) => {
                    heading += rad;
                },
                PushState => {
                    state.push_front((position, heading));
                },
                PopState => {
                    (position, heading) = state.pop_front().unwrap();
                    pb.move_to(position.0, position.1);
                },
                PenUp => { active = false; },
                PenDown => { active = true; },
            }
        }

        pb.finish()
    }

    fn build_draw_target(&self, resolution: f32, style: &StrokeStyle) -> DrawTarget {
        let width = ((2. + self.width as f32) * resolution) as i32;
        let height = ((2. + self.height as f32) * resolution) as i32;

        let mut target = DrawTarget::new(width, height);

        target.clear(SolidSource::from_unpremultiplied_argb(0xFF, 0, 0, 0));
        target.stroke(
            &self.build_path(resolution), 
            &Source::Solid(SolidSource::from_unpremultiplied_argb(0xFF, 0xFF, 0xFF, 0xFF)),
            style,
            &DrawOptions::new(),
        );

        target
    }

    pub fn save(&self, resolution: f32, style: &StrokeStyle, file_name: &str) -> anyhow::Result<()> {
        self.build_draw_target(resolution, style).write_png(file_name)?;

        Ok(())
    }

    pub fn show(&self, resolution: f32, style: &StrokeStyle) -> anyhow::Result<()> {
        let draw_target = self.build_draw_target(resolution, style);

        let image: Image = draw_target.into();

        let window = context().run_function_wait(move |context| -> anyhow::Result<WindowProxy> {
            let mut window = context.create_window("image", Default::default())?;
            window.set_image("mondriaan", &image.as_image_view()?);
            Ok(window.proxy())
        })?;

        window.wait_until_destroyed()?;

        Ok(())
    }
}
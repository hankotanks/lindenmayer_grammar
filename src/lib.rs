//! A minimal implementation of the Lindenmayer formal grammar with visualization tools
//! 
//! L-systems can be defined by three attributes
//! - An [Alphabet] of symbols[^a]
//! - A set of [Production] rules, called a [Ruleset]
//! - An intial [Axiom]
//! 
//! # Example
//! 
//! Below is a simple implementation of a [binary fractal tree](https://en.wikipedia.org/wiki/Fractal_canopy) L-system.
//! 
//! ```
//! #[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
//! enum FractalTree { Leaf, Node, Left, Right }
//! 
//! // Import all enum variants for ease of use
//! use FractalTree::*;
//! 
//! // Create an axiom from a single symbol
//! let mut axiom = Axiom::new(Leaf);
//! 
//! // Define the ruleset for creating a fractal tree
//! let rules = rules!(
//!     Node => Node : Node, 
//!     Leaf => Node : Left : Leaf : Right : Leaf
//! );
//! 
//! // Rewrite the axiom 6 times
//! for _ in 0..6 { 
//!     axiom.rewrite(&rules); 
//! }
//! 
//! // Build a turtle using the TurtleBuilder type
//! use TurtleAction::*;
//! let turtle = TurtleBuilder::new()
//!     .assign_action_set(Leaf, [Forward, PenUp, Backward, PenDown])
//!     .assign_action(Node, Forward)
//!     .assign_action_set(Left, [PushState, Turn(-PI * 0.25)])
//!     .assign_action_set(Right, [PopState, Turn(PI * 0.25)])
//!     .build();
//! 
//! // Last, draw the L-System's state and save to "tree.png"
//! axiom.visualize(turtle).save(
//!     [650, 600], 
//!     StrokeStyle { width: 2.0, ..Default::default() }, 
//!     SolidSource::from_unpremultiplied_argb(0xFF, 0xFF, 0xFF, 0xFF)
//!     "tree.png"
//! ).unwrap();
//! ```
//! 
//! [^a]: In this crate, any type which implements `Clone` and `Ord` is a valid Alphabet. Any integer type is ideal for this use case, but enums can work too if they derive the appropriate subtraits!

use std::{
    fmt::Debug, 
    vec::IntoIter, 
    iter::once, 
    collections::{BTreeSet, BTreeMap, VecDeque}, 
    slice::Iter
};

use ordered_float::OrderedFloat;

use rand::{
    thread_rng, 
    Rng
};

use raqote::{
    DrawTarget, 
    PathBuilder, 
    Source, 
    SolidSource, 
    StrokeStyle, 
    DrawOptions, 
    Path
};

use show_image::{
    Image, 
    context, 
    WindowProxy, 
    WindowOptions
};

/// An internal trait that is automatically implied for all types that can be used as valid alphabets.
/// 
/// Any type which implements `Clone`, and `Ord` is a valid Alphabet. This trait does not need to be derived.
/// 
/// # Examples
/// 
/// ```
/// #[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
/// enum Koch { Forward, Left, Right }
/// ```
pub trait Alphabet: Clone + Ord {  }

impl<T> Alphabet for T where T: Clone + Ord {  }

/// A sentence of symbols from a single [Alphabet].
/// 
/// An axiom can be initialized from a single symbol or a collection of symbols.
/// When provided a [Ruleset], an Axiom can be rewritten, either with the creation of a new Axiom (through the associated [rewrite](#method:rewrite) method) or mutably with [rewrite_in_place](#method:rewrite_in_place).
/// 
/// The collection of symbols that underpin an Axiom cannot be mutably accessed, although they can be turned into iterators with the associated [iter](#method:iter) method or represented as an immutable slice with [as_slice](#method:as_slice).
/// 
/// # Examples
/// 
/// ```
/// use lindenmayer_grammar::{Axiom, rules};
/// 
/// // i32 implements the requisite traits
/// let mut axiom = Axiom::with_elements([0, 1, 0]);
/// 
/// for _ in 0..3 {
///     // Rewrite the axiom using Lindenmayer's algae model
///     axiom.rewrite_in_place(&rules!(0 => 0 : 1, 1 => 0));
/// }
/// 
/// // Since i32 implements Debug, any Axiom composed of i32 elements implements Debug
/// println!("{:?}", &axiom);
/// 
/// // Iterating through an Axiom
/// for symbol in axiom.iter() {
///     todo!()
/// }
/// ```
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Axiom<A>(Vec<A>) where A: Alphabet;

impl<A> Axiom<A> where A: Alphabet {
    /// Create a new Axiom from a single symbol
    pub fn new(symbol: A) -> Self {
        Self(vec![symbol])
    }

    /// Create a new Axiom from any collection of symbols that can be iterated over.
    /// 
    /// To avoid unexpected behavior, any collection provided to this function should preserve order.
    /// 
    /// # Examples
    /// ```
    /// use lindenmayer_grammar::Axiom;
    /// 
    /// assert_eq!(Axiom::with_elements([0, 1, 0]), Axiom::with_elements(vec![0, 1, 0]));
    /// assert_eq!(Axiom::new(0), Axiom::with_elements([0]))
    /// ```
    pub fn with_elements<I: IntoIterator<Item = A>>(collection: I) -> Self {
        Self(collection.into_iter().collect::<Vec<_>>())
    }

    /// Iterate over the elements of an Axiom, yielding references to the Axiom's elements
    pub fn iter(&self) -> Iter<A> {
        self.0.iter()
    }

    /// Returns the number of elements in the Axiom
    pub fn len(&self) -> usize {
        self.0.len()
    }

    /// Returns `true` if the Axiom is empty, `false` otherwise.
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    // Extracts a slice containing all of the Axiom's elements.
    pub fn as_slice(&self) -> &[A] {
        self.0.as_slice()
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
    /// Axioms can be mutably rewritten using their associated [rewrite_in_place](#method:rewrite_in_place) method.
    /// 
    /// # Examples
    /// 
    /// ```
    /// use lindenmayer_grammar::{Axiom, rules};
    /// 
    /// let mut axioms = vec![Axiom::new(0)];
    /// 
    /// // Define the Ruleset for Lindenmayer's algae model
    /// let rules = rules!(0 => 0 : 1, 1 => 0);
    /// 
    /// for _ in 0..3 {
    ///     // Each step, push the new axiom to the list of previous axioms
    ///     axioms.push(axioms.last().unwrap().rewrite(&rules));
    /// }
    /// ```
    pub fn rewrite(&self, rules: &Ruleset<A>) -> Axiom<A> {
        let mut output = Vec::new();

        let mut pointer = 0;
        'token: while pointer < self.len() {
            for rule in rules.0.iter() {
                if rule.matcher(self.0.as_slice(), pointer) {
                    output.append(&mut rule.transcriber.0.clone());
                    pointer += rule.matcher.len();

                    continue 'token;
                }
            }

            output.push(self.0[pointer].clone());

            pointer += 1;
        }

        Axiom::with_elements(output)
    }

    /// Rewrites the [Axiom] using the given [Ruleset]
    /// 
    /// Can be used to quickly step through all generations of an L-system to a desired point.
    /// 
    /// The associated method [rewrite](#method:rewrite) produces a new [Axiom] from the original.
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
    /// let mut axiom = Axiom::new(0);
    /// 
    /// // Continue to rewrite it until its length exceeds 100 symbols
    /// while axiom.len() < 32 { axiom.rewrite(&rules); }
    /// ```
    pub fn rewrite_in_place(&mut self, rules: &Ruleset<A>) {
        self.0 = self.rewrite(rules).0;
    }

    /// Generates a [Drawing] from the axiom and a [Turtle], which describes the actions each symbol represents.
    /// 
    /// # Examples
    /// 
    /// ```
    /// use std::f32::consts::PI;
    /// 
    /// use lindenmayer_grammar::{Axiom, TurtleAction, TurtleBuilder};
    /// 
    /// // Initialize an Axiom for demonstration purposes
    /// let axiom = Axiom::with_elements([0, 1, 0, 1, 0, 1, 0, 1]);
    /// 
    /// // Create the turtle
    /// use TurtleAction::*;
    /// let turtle = TurtleBuilder::new()
    ///     .assign_action(0, PushState)
    ///     .assign_action_set(1, [Turn(PI * 0.5), Forward, PopState])
    ///     .build();
    /// 
    /// // The drawing represents a '+' symbol
    /// let drawing = axiom.visualize(turtle);
    /// ```
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
                actions.push(action.clone());

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

        Drawing::new(
            (maxima.0 - minima.0) as i32, 
            (maxima.1 - minima.1) as i32, 
            (minima.0.abs(), minima.1.abs()), 
            actions
        )
    }
}

/// A rule that dictates how symbols are rewritten in an L-System.
/// 
/// Each [Production] consists of two primary axioms
/// - the matcher
/// - the transcriber
/// 
/// When the matcher is found in a given [Axiom], it is replaced with the transcriber.
/// Optionally, precursor and successor axioms can be given to a production.
/// When present, the substitution will only succeed when the matcher is preceeded by the precursor and followed by the successor.
/// 
/// Additionally, a percentage (represented as a float) in the range `0f32..=1f32` can be supplied to a Production.
/// This makes the rule stochastic: substitution is probabilistic.
/// 
/// Productions are most easily defined using the [production!] macro.
/// 
/// Productions cannot be applied to axioms on their own, they must be compiled into a [Ruleset] first.
/// 
/// # Examples
/// 
/// ```
/// use lindenmayer_grammar::{Axiom, Ruleset, production};
/// 
/// let mut axiom = Axiom::from(0);
/// 
/// // 80% of the time, 1 becomes 0 when bordered by zeros
/// let p1 = production!(|0| 1 |0| => (0.8) 0);
/// 
/// // Unconditionally, 0 expands to 0, 1, 0
/// let p2 = production!(0 => 0 : 1 : 0);
/// 
/// // Compile the two productions into a Ruleset
/// let rules = Ruleset::from([p1, p2]);
/// 
/// // Apply the Ruleset twice
/// axiom.rewrite(&rules).rewrite(&rules);
/// ```
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Production<A: Alphabet> {
    matcher: Axiom<A>,
    transcriber: Axiom<A>,
    precursor: Option<Axiom<A>>,
    successor: Option<Axiom<A>>,
    probability: OrderedFloat<f32>
}

impl<A> Production<A> where A: Alphabet {
    /// Creates a new [Production], favor using the [production!] or [rules!] macros instead.
    pub fn new(
        matcher: Axiom<A>, 
        transcriber: Axiom<A>, 
        precursor: Option<Axiom<A>>, 
        successor: Option<Axiom<A>>,
        probability: f32
    ) -> Self {
        Self { 
            matcher, 
            transcriber, 
            precursor,
            successor,
            probability: OrderedFloat(probability),
        }
    }

    // Returns true if a match is found at `index`
    // Takes precursor and successor Axioms into account
    fn matcher(&self, tokens: &[A], index: usize) -> bool {
        if thread_rng().gen::<f32>() > self.probability.0 { return false; }

        if !tokens[index..].starts_with(self.matcher.0.as_slice()) { return false; }

        if let Some(precursor) = &self.precursor {
            let start = index.overflowing_sub(precursor.len());
            if start.1 { return false; }

            if &tokens[start.0..(start.0 + precursor.len())] != (precursor.0.as_slice()) { return false; }
        }

        if let Some(successor) = &self.successor {
            if successor.len() + self.matcher.len() + index > tokens.len() { return false; }

            let start = index + self.matcher.len();
            let end = start + successor.len();
            if &tokens[start..end] != successor.0.as_slice() { return false; }
        }

        true
    }
}

impl<A> Debug for Production<A> where A: Alphabet + Debug {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {

        #[inline]
        fn fmt_axiom<A>(axiom: &Axiom<A>) -> String where A: Alphabet + Debug {
            format!("{:?}", axiom)
                .replace(',', " :")
                .trim_start_matches('[')
                .trim_end_matches(']')
                .to_string()
        }

        let mut matcher = fmt_axiom(&self.matcher);

        if let Some(precursor) = &self.precursor {
            matcher.insert_str(0, " | ");
            matcher.insert_str(0, &fmt_axiom(precursor));
        }

        if let Some(successor) = &self.successor {
            matcher.push_str(" | ");
            matcher.push_str(&fmt_axiom(successor));
        }

        let mut transcriber = fmt_axiom(&self.transcriber);

        if self.probability.0 != 1.0 {
            transcriber.push_str(&format!(" ({})", self.probability.0));
        }

        write!(f, "\"{} => {}\"", matcher, transcriber)
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
/// Matcher and transcriber axioms are separated by a `=>` symbol.
/// Elements of the matcher and transcribers should be separated with a colon.
/// 
/// This macro supports stochastic productions, but context sensitive rules must be constructed using the [production!] macro.
/// 
/// # Examples
/// 
/// ```
/// use lindenmayer_grammar::rules;
///
/// // The second rule only occurs 50% of the time
/// // The first is unconditional
/// rules!(
///     0 => 0 : 1, 
///     1 => (0.5) 0
/// );
/// ```
#[macro_export]
macro_rules! rules {
    ($($a:literal $(: $b:literal)* => $(($p:literal))? $c:literal $(: $d:literal)*),+) => {
        {
            use std::collections::BTreeSet;

            use lindenmayer_grammar::{production, Ruleset};

            let mut ruleset = BTreeSet::new();
            $(ruleset.insert(production!($a $(: $b)* => $(($p))? $c $(: $d)*));)+
            
            Ruleset::from(ruleset)
        }
    };
    ($($a:path $(: $b:path)* => $(($p:literal))? $c:path $(: $d:path)*),+) => {
        {
            use std::collections::BTreeSet;

            use lindenmayer_grammar::{production, Ruleset};

            let mut ruleset = BTreeSet::new();
            $(ruleset.insert(production!($a $(: $b)* => $(($p))? $c $(: $d)*));)+
            
            Ruleset::from(ruleset)
        }
    };
}

/// A wrapper to create a single [Production].
/// 
/// Matcher and transcriber axioms should be separated by a `=>` symbol.
/// Elements of the matcher and transcriber should be separated with a colon.
/// 
/// This macro can represent context-sensitive rules.
/// An optional precursor axiom comes before matcher, while a successor can come afterwards.
/// 
/// `precursor | matcher > successor => (probability) transcriber`
/// # Examples
/// 
/// ```
/// use lindenmayer_grammar::{Axiom, Production, production};
/// 
/// // 0 becomes 0, 1 unconditionally
/// production!(0 => 0 : 1);
/// 
/// // 50% of the time, 1 becomes 0, but only when bounded by zeros
/// production!(0 | 1 > 0 => (0.5) 0);
/// 
/// // This rule could also be expressed as...
/// Production::new(Axiom::new(1), Axiom::new(0), Some(Axiom::new(0)), Some(Axiom::new(0)), 0.5);
/// ```
#[macro_export]
macro_rules! production {
    ($a:literal $(: $b:literal)* $(> $x:literal $(: $z:literal)*)? => $(($p:literal))? $c:literal $(: $d:literal)*) => {
        {
            use lindenmayer_grammar::{Axiom, Production};

            let mut matcher = vec![$a];
            let mut transcriber = vec![$c];
            let mut successor = Vec::new();
            let mut probability = 1.0;

            $(matcher.push($b);)*
            $(transcriber.push($d);)*
            $(probability = $p;)?
            
            $(
                successor.push($x);
                $(successor.push($z);)*
            )?

            Production::new(
                Axiom::with_elements(matcher), 
                Axiom::with_elements(transcriber), 
                None,
                if successor.is_empty() { None } else { Some(Axiom::with_elements(successor)) },
                probability
            )
        }
    };
    ($f:literal $(: $g:literal)* | $a:literal $(: $b:literal)* $(> $x:literal $(: $z:literal)*)? => $(($p:literal))? $c:literal $(: $d:literal)*) => {
        {
            use lindenmayer_grammar::{Axiom, Production};

            let mut matcher = vec![$a];
            let mut transcriber = vec![$c];
            let mut precursor = Vec::new();
            let mut successor = Vec::new();
            let mut probability = 1.0;

            $(matcher.push($b);)*
            $(transcriber.push($d);)*
            $(probability = $p;)?
            
            $(
                precursor.push($f);
                $(precursor.push($g);)*
            )?

            $(
                successor.push($x);
                $(successor.push($z);)*
            )?

            Production::new(
                Axiom::with_elements(matcher), 
                Axiom::with_elements(transcriber), 
                Some(Axiom::with_elements(precursor)),
                if successor.is_empty() { None } else { Some(Axiom::with_elements(successor)) },
                probability
            )
        }
    };
    ($a:path $(: $b:path)* $(> $x:path $(: $z:path)*)? => $(($p:literal))? $c:path $(: $d:path)*) => {
        {
            use lindenmayer_grammar::{Axiom, Production};

            let mut matcher = vec![$a];
            let mut transcriber = vec![$c];
            let mut successor = Vec::new();
            let mut probability = 1.0;

            $(matcher.push($b);)*
            $(transcriber.push($d);)*
            $(probability = $p;)?
            
            $(
                successor.push($x);
                $(successor.push($z);)*
            )?

            Production::new(
                Axiom::with_elements(matcher), 
                Axiom::with_elements(transcriber), 
                None,
                if successor.is_empty() { None } else { Some(Axiom::with_elements(successor)) },
                probability
            )
        }
    };
    ($f:path $(: $g:path)* | $a:path $(: $b:path)* $(> $x:path $(: $z:path)*)? => $(($p:literal))? $c:path $(: $d:path)*) => {
        {
            use lindenmayer_grammar::{Axiom, Production};

            let mut matcher = vec![$a];
            let mut transcriber = vec![$c];
            let mut precursor = vec![$f];
            let mut successor = Vec::new();
            let mut probability = 1.0;

            $(matcher.push($b);)*
            $(transcriber.push($d);)*
            $(probability = $p;)?
            $(precursor.push($g);)*

            $(
                successor.push($x);
                $(successor.push($z);)*
            )?

            Production::new(
                Axiom::with_elements(matcher), 
                Axiom::with_elements(transcriber), 
                Some(Axiom::with_elements(precursor)),
                if successor.is_empty() { None } else { Some(Axiom::with_elements(successor)) },
                probability
            )
        }
    };
}

/// Constructs a [Production] rule from a series of string literals.
/// 
/// Obeys the same grammar rules as the [production!] macro. 
/// However, instead of colon-delineated symbols, the corresponding strings are broken down into `char` arrays.
/// 
/// Unlike the [production!] macro; however, this macro always returns a value of type `Production<char>`.
/// 
/// # Examples
/// 
/// ```
/// use lindenmayer_grammar::{Ruleset, production_str};
/// 
/// // The production_str! macro makes the respresentation of complex L-Systems easy
/// let rules: Ruleset<char> = Ruleset::from([
///     production_str!("F" => "F[-EF[&&&A]]E[+F[```A]]"),
///     production_str!("F" | "E" => "F[&F[+++A]][`F[---A]]"),
///     production_str!("A" => "{[++++G.][++GG.][GGGGG.][-GGG.][--G.][----G.]}")
/// ]);
/// ```
#[macro_export]
macro_rules! production_str {
    ($m:literal $(> $s:literal)? => $(($c:literal))? $t:literal) => {
        {
            use lindenmayer_grammar::{Axiom, Production};

            let mut successor = None;
            $(successor = Some(Axiom::with_elements($s.chars));)?

            let mut probability = 1.0;
            $(probability = $c;)?

            Production::new(
                Axiom::with_elements($m.chars()),
                Axiom::with_elements($t.chars()),
                None,
                successor,
                probability
            )
        }
    };
    ($p:literal | $m:literal $(> $s:literal)? => $(($c:literal))? $t:literal) => {
        {
            use lindenmayer_grammar::{Axiom, Production};

            let mut successor = None;
            $(successor = Some(Axiom::with_elements($s.chars));)?

            let mut probability = 1.0;
            $(probability = $c;)?

            Production::new(
                Axiom::with_elements($m.chars()),
                Axiom::with_elements($t.chars()),
                Some(Axiom::with_elements($p.chars())),
                successor,
                probability
            )
        }
    };
}

/// Converts elements of an L-System [Alphabet] into sets of [TurtleAction] commands.
/// 
/// Must be supplied to [visualize](lindenmayer_grammar::Axiom::visualize) to create an Axiom's corresponding [Drawing].
/// 
/// Cannot be constructed directly; see [TurtleBuilder].
#[derive(Clone)]
pub struct Turtle<A>(BTreeMap<A, Vec<TurtleAction>>) where A: Alphabet;

/// Describes an action that can be taken by a [Turtle].
/// 
/// These actions are mapped to the symbols of an [Alphabet] using [TurtleBuilder].
/// 
/// # Examples
/// 
/// ```
/// use std::f32::consts::PI;
/// 
/// use lindenmayer_grammar::{TurtleBuilder, TurtleAction};
/// 
/// use TurtleAction::*;
/// let turtle = TurtleBuilder::new()
///     .assign_action(0, Forward)
///     .assign_action(1, Turn(PI * 0.5))
///     .assign_action(2, Turn(-PI * 0.5))
///     .build();
/// ```
#[derive(Clone)]
pub enum TurtleAction {
    /// Advances the [Turtle] by one unit.
    Forward,

    /// The [Turtle] retreats one unit. Does not affect it's heading.
    Backward,

    /// Turn by the specified number of radians. 
    /// Values less than 0 turn left. 
    /// Values greater than 0 turn right.
    Turn(f32),

    /// Save the heading and position of the turtle to a stack.
    /// These values will be restored when the `PopState` command is encountered.
    PushState,

    /// Pop the most recent heading and position values from the stack.
    /// Moves the [Turtle] to the position *without* drawing a line.
    PopState,

    /// The [Turtle] will no longer draw until `PenDown` is executed.
    PenUp,

    /// Allows the [Turtle] to continue drawing.
    PenDown,

    /// Updates the `StrokeStyle` of the [Turtle].
    /// Expects a function that takes a reference to the old `StrokeStyle` as a parameter.
    SetStrokeStyle(fn(&StrokeStyle) -> StrokeStyle),

    /// Updates the `SolidSource` of the [Turtle].
    /// Expects a function that takes a reference to the old `SolidSource` as a parameter.
    SetSolidSource(fn(&SolidSource) -> SolidSource)
}

/// Allows the construction of a [Turtle] by assigning sets of [TurtleAction] commands to [Alphabet] symbols.
/// 
/// # Examples
/// ```
/// use std::f32::consts::PI;
/// 
/// use lindenmayer_grammar::{TurtleBuilder, TurtleAction};
/// 
/// use TurtleAction::*;
/// let turtle = TurtleBuilder::new()
///     .assign_action(0, Forward)
///     .assign_action(1, Turn(PI * 0.5))
///     .assign_action(2, Turn(-PI * 0.5))
///     .build();
/// ```
#[derive(Clone)]
pub struct TurtleBuilder<A>(BTreeMap<A, Vec<TurtleAction>>) where A: Alphabet;

impl<A> Default for TurtleBuilder<A> where A: Alphabet {
    fn default() -> Self {
        Self(BTreeMap::new())
    }
}

impl<A> TurtleBuilder<A> where A: Alphabet {
    /// Initializes a new [TurtleBuilder].
    pub fn new() -> Self {
        Self::default()
    }

    /// Consumes, the [TurtleBuilder], yielding a completed [Turtle].
    pub fn build(self) -> Turtle<A> {
        Turtle(self.0)
    }

    /// Assign a single [TurtleAction] to a symbol.
    pub fn assign_action(mut self, symbol: A, action: TurtleAction) -> Self {
        self.0.insert(symbol, vec![action]);
        self
    }

    // Assign an ordered collection of [TurtleAction] commands to a symbol.
    pub fn assign_action_set<I: IntoIterator<Item = TurtleAction>>(mut self, symbol: A, actions: I) -> Self {
        self.0.insert(symbol, actions.into_iter().collect::<Vec<_>>());
        self
    }
}

/// The visualization of an L-System.
/// 
/// Use the associated [save](#method:save) method to render the [Drawing] to a `.png` file.
/// 
/// Or, display to the screen using [show](#method:show).
/// 
/// # Example
/// 
/// ```
/// use lindenmayer_grammar::{Axiom, TurtleAction, TurtleBuilder};
/// 
/// // Initialize an axiom for demonstration purposes
/// let axiom = Axiom::with_elements([0, 1, 0, 1, 0, 1, 0, 1]);
/// 
/// // Create the turtle
/// use TurtleAction::*;
/// let turtle = TurtleBuilder::new()
///     .assign_action(0, PushState)
///     .assign_action_set(1, [Turn(PI * 0.5), Forward, PopState])
///     .build();
/// 
/// // The drawing represents a '+' symbol
/// let drawing = axiom.visualize(turtle);
/// ```
pub struct Drawing {
    width: i32,
    height: i32,
    origin: (f32, f32),
    actions: Vec<TurtleAction>
}

impl Drawing {
    fn new(
        width: i32, 
        height: i32, 
        origin: (f32, f32), 
        actions: Vec<TurtleAction>
    ) -> Self {
        Self {
            width,
            height,
            origin, 
            actions
        }
    }

    fn build_path(
        &self, 
        resolution: f32, 
        offset_x: f32,
        offset_y: f32, 
        initial_stroke_style: StrokeStyle, 
        initial_solid_source: SolidSource
    ) -> Vec<(Path, StrokeStyle, SolidSource)> {
        let offset_x = offset_x + resolution * 0.5;
        let offset_y = offset_y + resolution * 0.5;

        let mut pos_x = self.origin.0 * resolution + offset_x;
        let mut pos_y = self.origin.1 * resolution + offset_y;

        let mut heading = 0f32;
        let mut active = true;

        let mut paths = vec![(
            PathBuilder::new(), 
            initial_stroke_style, 
            initial_solid_source
        )];

        paths.last_mut().unwrap().0.move_to(pos_x, pos_y);

        let mut state = VecDeque::new();

        use TurtleAction::*;
        for action in self.actions.iter() {
            match action {
                PenUp => { 
                    active = false; 
                },
                PenDown => { 
                    active = true; 
                },
                Forward => {
                    pos_x += heading.cos() * resolution;
                    pos_y += heading.sin() * resolution; 

                    if active {
                        paths.last_mut().unwrap().0.line_to(pos_x, pos_y);
                    } else {
                        paths.last_mut().unwrap().0.move_to(pos_x, pos_y);
                    }
                },
                Backward => {
                    pos_x -= heading.cos() * resolution;
                    pos_y -= heading.sin() * resolution; 
                    
                    if active {
                        paths.last_mut().unwrap().0.line_to(pos_x, pos_y);
                    } else {
                        paths.last_mut().unwrap().0.move_to(pos_x, pos_y);
                    }
                },
                Turn(rad) => {
                    heading += rad;
                },
                PushState => {
                    state.push_front((pos_x, pos_y, heading));
                },
                PopState => {
                    (pos_x, pos_y, heading) = state.pop_front().unwrap();
                    paths.last_mut().unwrap().0.move_to(pos_x, pos_y);
                }
                SetStrokeStyle(style) => {
                    let mut temp = PathBuilder::new();
                    temp.move_to(pos_x, pos_y);

                    paths.push((temp, (style)(&paths.last().unwrap().1), paths.last().unwrap().2));
                },
                SetSolidSource(solid_source) => {
                    let mut temp = PathBuilder::new();
                    temp.move_to(pos_x, pos_y);

                    paths.push((
                        temp, 
                        paths.last().unwrap().1.clone(), 
                        (solid_source)(&paths.last().unwrap().2)
                    ));
                }
            }
        }

        paths.into_iter()
            .map(|(pb, style, source)| (pb.finish(), style, source))
            .collect::<Vec<_>>()
    }

    fn build_draw_target(
        &self, 
        size: [u32; 2], 
        initial_stroke_style: StrokeStyle, 
        initial_solid_source: SolidSource
    ) -> DrawTarget {
        let pen = initial_stroke_style.width.ceil();

        let target_width = size[0] as i32 + pen as i32;
        let target_height = size[1] as i32 + pen as i32;
        
        let mut target = DrawTarget::new(target_width, target_height);

        let resolution = if self.width > self.height {
            size[0] as f32 / (self.width + 2) as f32
        } else {
            size[1] as f32 / (self.height + 2) as f32
        };

        let offset_x = (size[0] as f32 - resolution * (self.width + 2) as f32) / 2.0 + pen;
        let offset_y = (size[1] as f32 - resolution * (self.height + 2) as f32) / 2.0 + pen;

        target.clear(SolidSource::from_unpremultiplied_argb(0xFF, 0, 0, 0));

        let paths = self.build_path(
            resolution, 
            offset_x, 
            offset_y, 
            initial_stroke_style, 
            initial_solid_source
        );

        for (path, stroke_style, solid_source) in paths {
            target.stroke(
                &path,
                &Source::Solid(solid_source),
                &stroke_style,
                &DrawOptions::new(),
            );
        }

        target
    }

    /// Show the [Drawing] in a window.
    /// 
    /// The `raqote` crate is used to draw the L-System, and its `StrokeStyle` and `SolidSource` types are required to complete the visualization.
    /// 
    /// # Examples
    /// 
    /// ```
    /// use raqote::{StrokeStyle, SolidSource};
    /// 
    /// use lindenmayer_grammar::{Axiom, TurtleAction, TurtleBuilder};
    /// 
    /// // Initialize an axiom for demonstration purposes
    /// let axiom = Axiom::with_elements([0, 1, 0, 1, 0, 1, 0, 1]);
    /// 
    /// // Create the turtle
    /// use TurtleAction::*;
    /// let turtle = TurtleBuilder::new()
    ///     .assign_action(0, PushState)
    ///     .assign_action_set(1, [Turn(PI * 0.5), Forward, PopState])
    ///     .build();
    /// 
    /// // The drawing represents a '+' symbol
    /// let drawing = axiom.visualize(turtle);
    /// 
    /// // Display the drawing using white lines on a black background
    /// drawing.show(
    ///     [500, 500],
    ///     StrokeStyle::default(), 
    ///     SolidSource::from_unpremultiplied_argb(0xFF, 0xFF, 0xFF, 0xFF)
    /// );
    /// ```
    pub fn show(
        &self, 
        size: [u32; 2], 
        initial_stroke_style: StrokeStyle, 
        initial_solid_source: SolidSource
    ) -> anyhow::Result<()> {
        let draw_target = self.build_draw_target(size, initial_stroke_style, initial_solid_source);

        let image: Image = draw_target.into();

        let window_options = WindowOptions {
            size: Some(size),
            ..Default::default()
        };

        let window = context().run_function_wait(
            move |context| -> anyhow::Result<WindowProxy> {
                let mut window = context.create_window("L-System Visualization", window_options)?;
                window.set_image("image", &image.as_image_view()?);

                Ok(window.proxy())
            }
        )?;

        window.wait_until_destroyed()?;

        Ok(())
    }

    /// Save the [Drawing] as a `.png` image.
    /// 
    /// The `raqote` crate is used to draw the L-System, and its `StrokeStyle` and `SolidSource` types are required to complete the visualization.
    /// 
    /// # Examples
    /// 
    /// ```
    /// use raqote::{StrokeStyle, SolidSource};
    /// 
    /// use lindenmayer_grammar::{Axiom, TurtleAction, TurtleBuilder};
    /// 
    /// // Initialize an axiom for demonstration purposes
    /// let axiom = Axiom::with_elements([0, 1, 0, 1, 0, 1, 0, 1]);
    /// 
    /// // Create the turtle
    /// use TurtleAction::*;
    /// let turtle = TurtleBuilder::new()
    ///     .assign_action(0, PushState)
    ///     .assign_action_set(1, [Turn(PI * 0.5), Forward, PopState])
    ///     .build();
    /// 
    /// // The drawing represents a '+' symbol
    /// let drawing = axiom.visualize(turtle);
    /// 
    /// // Save the drawing to "plus.png"
    /// drawing.save(
    ///     [500, 500],
    ///     StrokeStyle::default(), 
    ///     SolidSource::from_unpremultiplied_argb(0xFF, 0xFF, 0xFF, 0xFF),
    ///     "plus.png"
    /// );
    /// ```
    pub fn save(
        &self, 
        size: [u32; 2], 
        initial_stroke_style: StrokeStyle, 
        initial_solid_source: SolidSource, 
        file_name: &str
    ) -> anyhow::Result<()> {
        self.build_draw_target(size, initial_stroke_style, initial_solid_source)
            .write_png(file_name)?;

        Ok(())
    }
}
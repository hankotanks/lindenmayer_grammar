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

pub trait Alphabet: Clone + Ord {  }

impl<T> Alphabet for T where T: Clone + Ord {  }

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

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

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

    pub fn rewrite_in_place(&mut self, rules: &Ruleset<A>) {
        self.0 = self.rewrite(rules).0;
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

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Production<A: Alphabet> {
    matcher: Axiom<A>,
    transcriber: Axiom<A>,
    precursor: Option<Axiom<A>>,
    successor: Option<Axiom<A>>,
    probability: OrderedFloat<f32>
}

impl<A> Production<A> where A: Alphabet {
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

    fn matcher(&self, tokens: &[A], index: usize) -> bool {
        if thread_rng().gen::<f32>() > self.probability.0 { return false; }

        if !tokens[index..].starts_with(self.matcher.0.as_slice()) { return false; }

        if let Some(precursor) = &self.precursor {
            let start = index.overflowing_sub(precursor.len());
            if start.1 { return false; }

            if &tokens[start.0..precursor.len()] != (precursor.0.as_slice()) { return false; }
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

#[macro_export]
macro_rules! rules {
    ($($(| $f:literal $(: $g:literal)* |)? $a:literal $(: $b:literal)* $(| $x:literal $(: $z:literal)* |)? $([$p:literal])? => $c:literal $(: $d:literal)*),+) => {
        {
            use std::collections::BTreeSet;

            use lindenmayer_grammar::{production, Ruleset};

            let mut ruleset = BTreeSet::new();
            $(ruleset.insert(production!($(| $f $(: $g)* |)? $a $(: $b)* $(| $x $(: $z)* |)? $([$p])? => $c $(: $d)*));)+
            
            Ruleset::from(ruleset)
        }
    };
    ($($(| $f:path $(: $g:path)* |)? $a:path $(: $b:path)* $(| $x:path $(: $z:path)* |)? $([$p:literal])? => $c:path $(: $d:path)*),+) => {
        {
            use std::collections::BTreeSet;

            use lindenmayer_grammar::{production, Ruleset};

            let mut ruleset = BTreeSet::new();
            $(ruleset.insert(production!($(| $f $(: $g)* |)? $a $(: $b)* $(| $x $(: $z)* |)? $([$p])? => $c $(: $d)*));)+
            
            Ruleset::from(ruleset)
        }
    };
}

#[macro_export]
macro_rules! production {
    ($(| $f:literal $(: $g:literal)* |)? $a:literal $(: $b:literal)* $(| $x:literal $(: $z:literal)* |)? $([$p:literal])? => $c:literal $(: $d:literal)*) => {
        {
            use lindenmayer_grammar::{Axiom, Production};

            let mut matcher = vec![$a];

            $(
                matcher.push($b);
            )*

            let mut transcriber = vec![$c];

            $(
                transcriber.push($d);
            )*

            let mut probability = 1.0;

            $(
                probability = $p;
            )?

            let mut precursor = Vec::new();
            $(
                precursor.push($f);

                $(
                    precursor.push($g);
                )*
            )?

            let mut successor = Vec::new();
            $(
                successor.push($x);

                $(
                    successor.push($z);
                )*
            )?

            Production::new(
                Axiom::with_elements(matcher), 
                Axiom::with_elements(transcriber), 
                if precursor.is_empty() { None } else { Some(Axiom::with_elements(precursor)) },
                if successor.is_empty() { None } else { Some(Axiom::with_elements(successor)) },
                probability
            )
        }
    };
    ($(| $f:path $(: $g:path)* |)? $a:path $(: $b:path)* $(| $x:path $(: $z:path)* |)? $([$p:literal])? => $c:path $(: $d:path)*) => {
        {
            use lindenmayer_grammar::{Axiom, Production};

            let mut matcher = vec![$a];

            $(
                matcher.push($b);
            )*

            let mut transcriber = vec![$c];

            $(
                transcriber.push($d);
            )*

            let mut probability = 1.0;

            $(
                probability = $p;
            )?

            let mut precursor = Vec::new();
            $(
                precursor.push($f);

                $(
                    precursor.push($g);
                )*
            )?

            let mut successor = Vec::new();
            $(
                successor.push($x);

                $(
                    successor.push($z);
                )*
            )?

            Production::new(
                Axiom::with_elements(matcher), 
                Axiom::with_elements(transcriber), 
                if precursor.is_empty() { None } else { Some(Axiom::with_elements(precursor)) },
                if successor.is_empty() { None } else { Some(Axiom::with_elements(successor)) },
                probability
            )
        }
    };
}

#[derive(Clone)]
pub struct Turtle<A>(BTreeMap<A, Vec<TurtleAction>>) where A: Alphabet;

#[derive(Clone)]
pub enum TurtleAction {
    Forward,
    Backward,
    Turn(f32),
    PushState,
    PopState,
    PenUp,
    PenDown,
    SetStrokeStyle(fn(&StrokeStyle) -> StrokeStyle),
    SetSolidSource(fn(&SolidSource) -> SolidSource)
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
    pub fn new(
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

                    paths.push((temp, (style)(&paths.last().unwrap().1), paths.last().unwrap().2.clone()));
                },
                SetSolidSource(solid_source) => {
                    let mut temp = PathBuilder::new();
                    temp.move_to(pos_x, pos_y);

                    paths.push((temp, paths.last().unwrap().1.clone(), (solid_source)(&paths.last().unwrap().2)));
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
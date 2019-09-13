//! A tokenizer and parser for sic (image) operations.
//!
//! A very recent history =)
//! ========================
//!
//!
//! Previously two methods where employed which allowed someone to apply operations on images with
//! sic. The first method is known as Apply Operations Script (AOS) [^1]. The idea here is that
//! you call sic with a separate "script" of commands, like so:
//! `sic -i a.jpg -o b.jpg --apply-operations "<script>"`
//! The method above was chosen instead of providing commands as cli arguments (e.g. `--blur 0.5`),
//! a method we call Image Operations as Cli Arguments, or IOCA [^2] (not the best name I ever picked :p).
//! CLI arguments were parsed by Clap, and while very accessible as a normal argument parser, it also
//! brought several difficulties if it were to be used with the IOCA method.
//! Minor difficulties included repetitions and negative values but the major road block was the order
//! of image operations. `A.B=AB & B.A = BA & AB ~= BA`. The way Clap gave back values was sub-optimal
//! for this method. At that time, parsing the arguments by hand seemed much more work, however at the
//! moment of writing I believe my judgement was mistaken. But we'll get there.
//! AOS uses the Pest PEG parser to parse image operation commands. It was the first time I wrote a
//! parser using a PEG language and I don't think it is the cleanest, or most efficient parser I ever
//! wrote for that reason. But it worked pretty well.
//!
//! Recently I started researching the image-rs/imageproc library, and prototyped towards
//! ways to incorporate it into sic.
//! Since sic is basically a cli front-end for the image-rs/image library, we tend to not implement
//! image operations ourselves. We provide an (hopefully accessible) interface to use all this
//! amazing work. This means that our main task is to attempt and aim to make sic an  accessible
//! (i.e. user friendly) tool for users which like to use the image conversion and image processing
//! functionality from the cli.
//! [ Of course an exemplary and way more extensive cli (and library) already exists, namely ImageMagick.
//!   After all, I started sic not to dethrone ImageMagick, but rather to gradually improve on my
//!   knowledge and understanding of working with the Rust language.
//!   ImageMagick does however not have the most beginner friendly cli interface. ]
//!  
//! Anyhow, while prototyping, I wasn't sure whether I liked to user a separate interface
//! for the image operations. Writing image commands with the sic cli felt like two separate systems;
//! i.e. a call to sic and separately the image operations. When generating image operation script
//! the AOS method was convenient. But hand writing commands felt less accessible. Another
//! disadvantage of not using IOCA is that this custom syntax makes generating auto completion harder.
//!
//! I decided to bite the bullet and regardless implement the IOCA method as well. The implementation [^3]
//! uses the results provided by Clap. Clap parses the image operations and verifies the structure
//! of the cli image arguments. Then we use Claps ArgMatches to build a tree of image operations by
//! using the two things Claps provides us: the indices of arguments and the values of the same
//! arguments. Since we know for operations how much arguments they take we can determine where
//! values occur, unify them and create a parse tree. This process happens per operation, because
//! Clap provides us with the indices and argument values per defined clap::Arg (and each operation
//! if defined as such an Arg).
//! Back then I was quite happy how I made it work. However there are some complexities which I feel
//! are unnecessary for such a 'simple' parser. Additionally, I would like to at some point create a
//! single implementation which serves both AOS as well as IOCA (so I also can defer whether I would r
//! remove one or the other :)).  
//!
//! For the parser side, that is where we are right now. We have two parsers. One pulls in some serious
//! amount of dependencies while the other passes through Clap which results in unnecessary
//! complexities.
//!
//! The prototype below provides a start on simplifying the parsing side of the equation for sic.
//! For now it will be hand written as to not pull in too many dependencies. Of course we could rewrite
//! our PEG parser to accept cli arguments from std::env::args() as well, but as written above, I would
//! also like to reduce the amount of dependencies (to improve both compile time and binary size).
//!
//!
//!
//! [^1]: AOS available from sic 0.5.0 (initial), 0.7.0 (most operations), 0.9.0 (modifiers)
//!
//! [^2]: IOCA available from sic 0.10.0
//!
//! [^3]: [commit](https://github.com/foresterre/sic/commit/8066ca67b1cfe30ecfb42180c5beced7af857d4c) which added the current as of writing IOCA implementation which uses Clap to parse the cli image operations and its arguments and then separately creates an image operations parse tree from the Clap provided values and indices (per operation).
//!

use peekmore::{PeekMore, PeekMoreIterator};
use std::env;
use std::fmt::Debug;

pub type IType = i32;
pub type UType = u32;
pub type FType = f32;

// todo:
// * write bench

#[derive(Debug)]
pub enum Op {
    Blur,
    Brighten,
    Contrast,
    Crop,
    Filter3x3,
    FlipH,
    FlipV,
    GrayScale,
    HueRotate,
    Invert,
    Resize,
    Rotate90,
    Rotate180,
    Rotate270,
    Unsharpen,
}

impl Op {
    pub fn from_str(input: &str) -> Option<Self> {
        match input {
            "blur" => Some(Self::Blur),
            "brighten" => Some(Self::Brighten),
            "contrast" => Some(Self::Contrast),
            "crop" => Some(Self::Crop),
            "filter3x3" => Some(Self::Filter3x3),
            "fliph" => Some(Self::FlipH),
            "flipv" => Some(Self::FlipV),
            "grayscale" => Some(Self::GrayScale),
            "huerotate" => Some(Self::HueRotate),
            "invert" => Some(Self::Invert),
            "resize" => Some(Self::Resize),
            "rotate90" => Some(Self::Rotate90),
            "rotate180" => Some(Self::Rotate180),
            "rotate270" => Some(Self::Rotate270),
            "unsharpen" => Some(Self::Unsharpen),
            _ => None,
        }
    }
}

#[derive(Debug)]
pub enum Modifier {
    PreserveAspectRatio,
    SamplingFilter,
}

impl Modifier {
    pub fn from_str(input: &str) -> Option<Self> {
        match input {
            "preserve-aspect-ratio" => Some(Self::PreserveAspectRatio),
            "sampling-filter" => Some(Self::SamplingFilter),
            _ => None,
        }
    }
}

#[derive(Debug)]
pub enum Token {
    Op(Op),
    Int(IType),
    UInt(UType),
    FP(FType),
    Skip,
    // set [operation] [modifier]
    Set(Op, Modifier),
}

pub trait StopMark {
    fn mark() -> Self;
}

impl StopMark for String {
    fn mark() -> Self {
        // End of text
        format!("{}", 0x03)
    }
}

#[derive(Debug)]
pub struct Tokenizer<V: StopMark + AsRef<str> + Debug, T: Iterator<Item = V>> {
    source: PeekMoreIterator<T>,
    stop_mark: V,
}

impl<V: StopMark + AsRef<str> + Debug, T: Iterator<Item = V>> Tokenizer<V, T> {
    pub fn new(input: T) -> Self {
        Tokenizer {
            source: input.peekmore(),
            stop_mark: V::mark(),
        }
    }

    pub fn prefixed(&mut self) -> bool {
        fn starts_with_dashes<V: AsRef<str>>(input: V) -> bool {
            let dashes = input.as_ref();
            dbg!(dashes);
            dashes.starts_with("--")
            //input.as_ref().starts_with("--")
        }

        let peek_cursor = self.source.needle_position();
        dbg!(peek_cursor);

        let ok = self.source.peek().map(starts_with_dashes);
        ok.unwrap_or(false)
    }
}

impl<V: StopMark + AsRef<str> + Debug, T: Iterator<Item = V>> Iterator for Tokenizer<V, T> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        let prefixed = self.prefixed();
        dbg!(prefixed);

        let consume_next = self.source.next();
        let tokens_next = consume_next.as_ref().unwrap_or(&self.stop_mark);
        let tokens_as_str = tokens_next.as_ref();

        eprintln!("current: {}", tokens_as_str);

        match tokens_as_str {
            v if v == self.stop_mark.as_ref() => None,
            v if v == "1" => Some(Token::Int(1)),
            _ => Some(Token::Skip),
        }
    }
}

pub fn prototype() {
    for arg in env::args() {
        eprintln!("{:?}", arg);
    }

    eprintln!("               ");
    eprintln!("===============");
    eprintln!("               ");

    // whitespace separated args
    let args = env::args();

    // fixme: args.into_iter() -> args whitespace separated; not characters; but this might as well be enough for us here
    let tokenizer = Tokenizer::new(args.into_iter());

    for (i, token) in tokenizer.enumerate() {
        eprintln!("#{}, token: {:?}\n\n", i, token);
    }
}

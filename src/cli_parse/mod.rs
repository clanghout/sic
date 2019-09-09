use crate::cli_parse::look_ahead_iterator::{
    CreateLookAheadIterator, LookAheadIterator, LookAheadView,
};
use std::env;
use std::fmt::Debug;

pub mod look_ahead_iterator;

pub type IType = i32;
pub type UType = u32;
pub type FType = f32;

// todo:
// * write bench

#[derive(Debug)]
pub enum OpType {
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
    ResizeSetPreserveAspectRatio,
    ResizeSetSamplingFilter,
    Rotate90,
    Rotate180,
    Rotate270,
    Unsharpen,
}

impl OpType {
    // \tdo{ transform into macro for DRY }

    pub fn from_str(input: &str) -> Self {
        match input {
            "blur" => Self::Blur,
            _ => Self::Unsharpen,
        }
    }
}

#[derive(Debug)]
pub enum Token {
    Operation(OpType),
    Integer(IType),
    UnsignedInteger(UType),
    FloatingPoint(FType),
    Skip,
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
    source: LookAheadIterator<T>,
    stop_mark: V,
}

impl<V: StopMark + AsRef<str> + Debug, T: Iterator<Item = V>> Tokenizer<V, T> {
    pub fn new(input: T) -> Self {
        Tokenizer {
            source: input.look_ahead(),
            stop_mark: V::mark(),
        }
    }

    pub fn peek_is_op_arg(&mut self) -> bool {
        fn starts_with_dashes<V: AsRef<str>>(input: V) -> bool {
            let dashes = input.as_ref();
            dbg!(dashes);
            dashes.starts_with("--")
            //input.as_ref().starts_with("--")
        }

        let a = self.source.advance_view();
        let b = a.preview();
        dbg!(b);

        eprintln!("__DEBUG Queue: {:?}", self.source.__debug_view_queue());
        eprintln!(
            "__DEBUG (needle position, queue size): {:?}",
            self.source.__debug_sizes()
        );

        let ok = self.source.preview_next().map(starts_with_dashes);

        // or unwrap_or(false)
        match ok {
            Some(v) => v,
            _ => false,
        }
    }
}

impl<V: StopMark + AsRef<str> + Debug, T: Iterator<Item = V>> Iterator for Tokenizer<V, T> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        let _is_arg_prefix = self.peek_is_op_arg();
        dbg!(_is_arg_prefix);

        let next = self.source.next();
        let next = next.as_ref().unwrap_or(&self.stop_mark);
        let next = next.as_ref();

        eprintln!("current: {}", next);

        match next {
            v if v == self.stop_mark.as_ref() => None,
            v if v == "1" => Some(Token::Integer(1)),
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

    let tokenizer = Tokenizer::new(args.into_iter());

    for (i, token) in tokenizer.enumerate() {
        eprintln!("#{}, token: {:?}\n\n", i, token);
    }
}

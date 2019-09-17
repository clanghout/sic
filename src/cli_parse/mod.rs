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
//! **-- September, 2019*
//!
//! [^1]: AOS available from sic 0.5.0 (initial), 0.7.0 (most operations), 0.9.0 (modifiers)
//!
//! [^2]: IOCA available from sic 0.10.0
//!
//! [^3]: [commit](https://github.com/foresterre/sic/commit/8066ca67b1cfe30ecfb42180c5beced7af857d4c) which added the current as of writing IOCA implementation which uses Clap to parse the cli image operations and its arguments and then separately creates an image operations parse tree from the Clap provided values and indices (per operation).
//!

use std::fmt::Debug;

use crate::cli_parse::numbers::{F32, I32, U32};
use crate::cli_parse::parse_to::{ParseFromIter, ParsePerTypeError};
use peekmore::{PeekMore, PeekMoreIterator};

pub mod numbers;
pub mod parse_to;

// todo:
// * write bench

#[derive(Debug)]
#[cfg_attr(test, derive(Eq, PartialEq))]
pub enum Core {
    Operation(Op),
    // set [operation] [modifier]
    SetModifier(Op, Modifier),
    Skip,
}

// fixme: specialize the errors
#[derive(Debug)]
#[cfg_attr(test, derive(Eq, PartialEq))]
pub enum ParserError {
    Unexpected,

    // fixme: temporary type for convenience
    PPTE(ParsePerTypeError),
}

#[derive(Debug)]
#[cfg_attr(test, derive(Eq, PartialEq))]
pub enum Op {
    Blur(F32),
    Brighten(I32),
    Contrast(F32),
    Crop((U32, U32, U32, U32)),
    Filter3x3([F32; 9]),
    FlipH,
    FlipV,
    GrayScale,
    HueRotate(I32),
    Invert,
    Resize((U32, U32)),
    Rotate90,
    Rotate180,
    Rotate270,
    Unsharpen((F32, I32)),
}

impl Op {
    const SELECTION: &'static [&'static str] = &[
        "blur",
        "brighten",
        "contrast",
        "crop",
        "filter3x3",
        "flip-horizontal",
        "flip-vertical",
        "grayscale",
        "hue-rotate",
        "invert",
        "resize",
        "rotate90",
        "rotate180",
        "rotate180",
        "rotate270",
        "unsharpen",
    ];

    pub fn is_some(input: &str) -> bool {
        Op::SELECTION.contains(&input)
    }

    // fixme: re-use sic_parser::value_parser?
    pub fn from_str<I: Iterator>(
        iter: &mut PeekMoreIterator<I>,
        input: &str,
    ) -> Option<Result<Self, ParserError>>
    where
        I::Item: AsRef<str> + std::fmt::Debug,
    {
        match input {
            "blur" => {
                let result: Result<Self, ParserError> = ParseFromIter::parse(iter)
                    .map_err(|err| ParserError::PPTE(err))
                    .map(|arg: F32| Op::Blur(arg));

                Some(result)
            }
            "brighten" => {
                let result: Result<Self, ParserError> = ParseFromIter::parse(iter)
                    .map_err(|err| ParserError::PPTE(err))
                    .map(|arg: i32| Op::Brighten(arg));

                Some(result)
            }
            "contrast" => {
                let result: Result<Self, ParserError> = ParseFromIter::parse(iter)
                    .map_err(|err| ParserError::PPTE(err))
                    .map(|arg: F32| Op::Contrast(arg));

                Some(result)
            }
            "crop" => {
                let result: Result<Self, ParserError> = ParseFromIter::parse(iter)
                    .map_err(|err| ParserError::PPTE(err))
                    .map(|arg: (u32, u32, u32, u32)| Op::Crop(arg));

                Some(result)
            }
            "filter3x3" => {
                let result: Result<Self, ParserError> = ParseFromIter::parse(iter)
                    .map_err(|err| ParserError::PPTE(err))
                    .map(|arg: [F32; 9]| Op::Filter3x3(arg));

                Some(result)
            }
            "flip-horizontal" => Some(Ok(Op::FlipH)),
            "flip-vertical" => Some(Ok(Op::FlipV)),
            "grayscale" => Some(Ok(Op::GrayScale)),
            "hue-rotate" => {
                let result: Result<Self, ParserError> = ParseFromIter::parse(iter)
                    .map_err(|err| ParserError::PPTE(err))
                    .map(|arg: i32| Op::HueRotate(arg));

                Some(result)
            }
            "invert" => Some(Ok(Op::Invert)),
            "resize" => {
                let result: Result<Self, ParserError> = ParseFromIter::parse(iter)
                    .map_err(|err| ParserError::PPTE(err))
                    .map(|arg: (u32, u32)| Op::Resize(arg));

                Some(result)
            }
            "rotate90" => Some(Ok(Op::Rotate90)),
            "rotate180" => Some(Ok(Op::Rotate180)),
            "rotate270" => Some(Ok(Op::Rotate270)),
            "unsharpen" => {
                let result: Result<Self, ParserError> = ParseFromIter::parse(iter)
                    .map_err(|err| ParserError::PPTE(err))
                    .map(|arg: (F32, i32)| Op::Unsharpen(arg));

                Some(result)
            }
            _ => None,
        }
    }
}

#[derive(Debug)]
#[cfg_attr(test, derive(Eq, PartialEq))]
pub enum Modifier {
    PreserveAspectRatio,
    SamplingFilter,
}

impl Modifier {
    pub fn from_str<I: Iterator>(
        _iter: &mut PeekMoreIterator<I>,
        input: &str,
    ) -> Option<Result<Self, ParserError>>
    where
        I::Item: AsRef<str>,
    {
        match input {
            "preserve-aspect-ratio" => Some(Ok(Self::PreserveAspectRatio)),
            "sampling-filter" => Some(Ok(Self::SamplingFilter)),
            _ => None,
        }
    }
}

pub trait StopMark {
    fn mark() -> Self;
}

impl StopMark for String {
    fn mark() -> Self {
        // End of text
        (0x03 as char).to_string()
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum ParsingMode {
    Arg,
    Script,
    Ambiguous,
}

#[derive(Debug)]
pub struct Parser<V: StopMark + AsRef<str> + Debug, T: Iterator<Item = V>> {
    source: PeekMoreIterator<T>,
    stop_mark: V,
    mode: ParsingMode,
}

impl<V: StopMark + AsRef<str> + Debug, T: Iterator<Item = V>> Parser<V, T> {
    pub fn new(input: T) -> Self {
        Parser {
            source: input.peekmore(),
            stop_mark: V::mark(),
            mode: ParsingMode::Ambiguous,
        }
    }

    pub fn peek_check_for_cli_arg_prefixes(&mut self) -> bool {
        fn starts_with_dashes<V: AsRef<str>>(input: V) -> bool {
            let arg = input.as_ref();
            arg.starts_with("--") && arg.len() > 2
        }

        let ok = self.source.peek().map(starts_with_dashes);
        ok.unwrap_or(false)
    }

    // if the source we are parsing includes potentially cli args
    // we ignore the ones we don't know
    pub fn is_non_image_op_cli_arguments(&self, arg: &str) -> bool {
        self.mode != ParsingMode::Script
            && ((arg.starts_with("--") && arg.len() > 2)
                || (arg.starts_with("-") && arg.len() == 2))
    }

    pub fn skip_non_image_op_cli_arguments(&self) -> Option<Result<Core, ParserError>> {
        Some(Ok(Core::Skip))
    }
}

impl<V: StopMark + AsRef<str> + Debug, T: Iterator<Item = V>> Iterator for Parser<V, T> {
    type Item = Result<Core, ParserError>;

    fn next(&mut self) -> Option<Self::Item> {
        let prefixed = self.peek_check_for_cli_arg_prefixes();
        dbg!(prefixed);

        let next_consumed = self.source.next();
        let next_token = next_consumed.as_ref().unwrap_or(&self.stop_mark);
        // unless parsing mode is Script, if the current token is prefixed,
        // we limit the slice as to not include the `--`.
        let next_token = if self.mode != ParsingMode::Script && prefixed {
            let (_, hi) = next_token.as_ref().split_at(2);
            hi
        } else {
            next_token.as_ref()
        };

        eprintln!("token(in): {}", next_token);

        match next_token {
            // The stop mark tells us that the iterator is finished.
            v if v == self.stop_mark.as_ref() => None,

            // One of the image operations.
            op if Op::is_some(op) => {
                let which = Op::from_str(&mut self.source, op);
                which.map(|v| v.map(|ok| Core::Operation(ok)))
            }

            // An operation from the cli which is not an image operation.
            // FIXME: we'll also have to skip the arguments of said potential non image operation
            //      argument.
            //      - peek until next peek starts with '--' again; <= skip
            arg if self.is_non_image_op_cli_arguments(arg) => {
                self.skip_non_image_op_cli_arguments()
            }
            _ => Some(Err(ParserError::Unexpected)),
        }
    }
}

// fixme: extend test suite
// fixme: args.into_iter() -> args whitespace separated; not characters; but this might as well be enough for us here
pub fn prototype<T: Iterator<Item = String>>(args: T) -> Result<Vec<Core>, ParserError> {
    let parser = Parser::new(args);

    parser
        .inspect(|out| println!("token(out): {:?}", out))
        .collect::<Result<Vec<_>, ParserError>>()
}

#[cfg(test)]
mod tests;

#[cfg(test)]
mod tests_parser {
    use super::*;

    macro_rules! combi_test {
        ($mod_name:ident, $expected:expr, $($input:expr),*) => {

            mod $mod_name {
                use super::*;

                make_test!{ without, {
                    without_dashes_body!($expected, $($input),*)
                }}

                make_test!{ with, {
                    with_dashes_body!($expected, $($input),*)
                }}
           }
        };
    }

    macro_rules! without_dashes_body {
        ($expected: expr, $($input:expr),*) => {{
            let input = vec![$($input.to_string()),*];

            assert_eq!(
                prototype(input.into_iter()),
                $expected
            );
        }};
    }

    macro_rules! with_dashes_body {
        ($expected: expr, $($input:expr),*) => {{
            let mut input = vec![$($input.to_string()),*];

            let mut new = input[0].chars().rev().collect::<String>();
            input[0] = {
                new.push_str("-");
                new.push_str("-");
                new.chars().rev().collect::<String>()
            };

            assert_eq!(
                prototype(input.into_iter()),
                $expected
            );
        }};
    }

    macro_rules! make_test {
        ($name:ident, $body:block) => {
            #[test]
            fn $name() {
                $body
            }
        };
    }

    mod blur {
        use super::*;

        combi_test!(
            blur_pos15,
            Ok(vec![Core::Operation(Op::Blur(F32(1.5)))]),
            "blur",
            "1.5"
        );
        combi_test!(
            blur2,
            Err(ParserError::PPTE(ParsePerTypeError::ParseFloatError)),
            "blur",
            "xyz"
        );

        combi_test!(
            blur_pos1,
            Ok(vec![Core::Operation(Op::Blur(F32(1.0)))]),
            "blur",
            "1"
        );

        combi_test!(
            blur_neg1,
            Ok(vec![Core::Operation(Op::Blur(F32(-1.0)))]),
            "blur",
            "-1"
        );
    }

    mod brighten {
        use super::*;

        combi_test!(
            brighten,
            Ok(vec![Core::Operation(Op::Brighten(1))]),
            "brighten",
            "1"
        );

        combi_test!(
            brighten_neg,
            Ok(vec![Core::Operation(Op::Brighten(-1))]),
            "brighten",
            "-1"
        );

        combi_test!(
            brighten_not_fp,
            Err(ParserError::PPTE(ParsePerTypeError::ParseIntError)),
            "brighten",
            "-1.5"
        );
    }

    mod contrast {
        use super::*;

        combi_test!(
            constrast,
            Ok(vec![Core::Operation(Op::Contrast(F32(1.0)))]),
            "contrast",
            "1"
        );

        combi_test!(
            contrast_neg,
            Ok(vec![Core::Operation(Op::Contrast(F32(-1.0)))]),
            "contrast",
            "-1"
        );

        combi_test!(
            contrast_fp,
            Ok(vec![Core::Operation(Op::Contrast(F32(-1.0)))]),
            "contrast",
            "-1.0"
        );

        combi_test!(
            brighten_not_fp,
            Err(ParserError::PPTE(ParsePerTypeError::ParseFloatError)),
            "contrast",
            "x"
        );
    }

    mod crop {
        use super::*;

        combi_test!(
            crop_basic,
            Ok(vec![Core::Operation(Op::Crop((1, 2, 3, 4)))]),
            "crop",
            "1",
            "2",
            "3",
            "4"
        );

        combi_test!(
            crop_zeros,
            Ok(vec![Core::Operation(Op::Crop((0, 0, 0, 0)))]),
            "crop",
            "0",
            "0",
            "0",
            "0"
        );

        combi_test!(
            crop_accept_any_fitting_uint,
            Ok(vec![Core::Operation(Op::Crop((1, 1, 1, 1)))]),
            "crop",
            "1",
            "1",
            "1",
            "1"
        );

        combi_test!(
            crop_cant_be_neg,
            Err(ParserError::PPTE(ParsePerTypeError::ParseUIntError)),
            "crop",
            "1",
            "1",
            "1",
            "-1"
        );

        combi_test!(
            crop_cant_be_not_a_number,
            Err(ParserError::PPTE(ParsePerTypeError::ParseUIntError)),
            "crop",
            "1",
            "1",
            "1",
            "~"
        );
    }

    mod multi {
        use super::*;

        combi_test!(
            multi_1,
            Ok(vec![
                Core::Operation(Op::Blur(F32(1.0))),
                Core::Operation(Op::Brighten(-2)),
                Core::Operation(Op::Contrast(F32(3.0))),
            ]),
            "blur",
            "1",
            "brighten",
            "-2",
            "contrast",
            "3"
        );

    }

}
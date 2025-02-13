use std::collections::BTreeMap;
use std::str::FromStr;

use clap::{App, AppSettings, Arg, ArgGroup, ArgMatches};
use sic_image_engine::engine::Statement;

use crate::app::config::{validate_jpeg_quality, Config, ConfigBuilder, SelectedLicenses};
use crate::app::operations::{
    extend_index_tree_with_unification, IndexTree, IndexedOps, Op, OperationId,
};
use crate::get_tool_name;
use crate::{op_valueless, op_with_values};
use arg_names::*;
use sic_io::load::FrameIndex;

const ABOUT: &str = include_str!("../../resources/help-pages/about.txt");
const HELP_OPERATIONS_AVAILABLE: &str =
    include_str!("../../resources/help-pages/image_operations.txt");

// table of argument names
pub(crate) mod arg_names {
    // cli - possible arguments

    // organisational:
    pub(crate) const ARG_LICENSE: &str = "license";
    pub(crate) const ARG_DEP_LICENSES: &str = "dep_licenses";

    // io(input):
    pub(crate) const ARG_INPUT: &str = "input";
    pub(crate) const ARG_OUTPUT: &str = "output";

    // io(output):
    pub(crate) const ARG_INPUT_FILE: &str = "input_file";
    pub(crate) const ARG_OUTPUT_FILE: &str = "output_file";

    // config(in):
    pub(crate) const ARG_SELECT_FRAME: &str = "select_frame";

    // config(out):
    pub(crate) const ARG_DISABLE_AUTOMATIC_COLOR_TYPE_ADJUSTMENT: &str =
        "disable_automatic_color_type_adjustment";
    pub(crate) const ARG_FORCED_OUTPUT_FORMAT: &str = "forced_output_format";
    pub(crate) const ARG_JPEG_ENCODING_QUALITY: &str = "jpeg_encoding_quality";

    pub(crate) const ARG_PNM_ENCODING_ASCII: &str = "pnm_encoding_ascii";

    // image-operations(script):
    pub(crate) const ARG_APPLY_OPERATIONS: &str = "script";

    // image-operations(cli-arguments):
    pub(crate) const GROUP_IMAGE_OPERATIONS: &str = "group";
    pub(crate) const OP_BLUR: &str = "op_blur";
    pub(crate) const OP_BRIGHTEN: &str = "op_brighten";
    pub(crate) const OP_CONTRAST: &str = "op_contrast";
    pub(crate) const OP_CROP: &str = "op_crop";
    pub(crate) const OP_FILTER3X3: &str = "op_filter3x3";
    pub(crate) const OP_FLIP_HORIZONTAL: &str = "op_fliph";
    pub(crate) const OP_FLIP_VERTICAL: &str = "op_flipv";
    pub(crate) const OP_GRAYSCALE: &str = "op_grayscale";
    pub(crate) const OP_HUE_ROTATE: &str = "op_huerotate";
    pub(crate) const OP_INVERT: &str = "op_invert";
    pub(crate) const OP_RESIZE: &str = "op_resize";
    pub(crate) const OP_ROTATE90: &str = "op_rot90";
    pub(crate) const OP_ROTATE180: &str = "op_rot180";
    pub(crate) const OP_ROTATE270: &str = "op_rot270";
    pub(crate) const OP_UNSHARPEN: &str = "op_unsharpen";

    // image-operations(cli-arguments/modifiers):
    pub(crate) const OPMOD_RESIZE_PRESERVE_ASPECT_RATIO: &str = "opmod_resize_par";
    pub(crate) const OPMOD_RESIZE_SAMPLING_FILTER: &str = "opmod_resize_sampling_filter";
}

pub fn cli() -> App<'static, 'static> {
    App::new(get_tool_name())
        .version(env!("CARGO_PKG_VERSION"))
        .about(ABOUT)
        .after_help("For more information, visit: https://github.com/foresterre/sic")
        .author("Martijn Gribnau <garm@ilumeo.com>")

        // settings
        .global_setting(AppSettings::NextLineHelp)
        .global_setting(AppSettings::ColoredHelp)
        .global_setting(AppSettings::ColorAuto)
        .global_setting(AppSettings::DontCollapseArgsInUsage)
        .global_setting(AppSettings::UnifiedHelpMessage)
        .max_term_width(120)

        // cli arguments

        // organisational:
        .arg(Arg::with_name(ARG_LICENSE)
            .long("license")
            .help("Displays the license of this piece of software (`sic`).")
            .takes_value(false)
            .conflicts_with_all(&[ARG_DEP_LICENSES, ARG_INPUT_FILE, ARG_OUTPUT_FILE, ARG_INPUT, ARG_OUTPUT]))
        .arg(Arg::with_name(ARG_DEP_LICENSES)
            .long("dep-licenses")
            .help("Displays the licenses of the dependencies on which this software relies.")
            .takes_value(false)
            .conflicts_with_all(&[ARG_LICENSE, ARG_INPUT_FILE, ARG_OUTPUT_FILE, ARG_INPUT, ARG_OUTPUT]))

        // io(input):
        .arg(Arg::with_name(ARG_INPUT)
            .long("input")
            .short("i")
            .value_name("INPUT_PATH")
            .takes_value(true)
            .help("Input image path. When using this option, input piped from stdin will be ignored.")
            .conflicts_with_all(&[ARG_INPUT_FILE, ARG_OUTPUT_FILE, ARG_LICENSE, ARG_DEP_LICENSES]))
        .arg(Arg::with_name(ARG_INPUT_FILE)
            .help("DEPRECATED. Use '--input' instead. (Sets the input file. Can only be used in combination with OUTPUT_FILE.)")
            .value_name("INPUT_FILE")
            .requires(ARG_OUTPUT_FILE)
            .conflicts_with_all(&[ARG_INPUT, ARG_OUTPUT, ARG_LICENSE, ARG_DEP_LICENSES])
            .index(1))

        // io(output):
        .arg(Arg::with_name(ARG_OUTPUT)
            .long("output")
            .short("o")
            .value_name("OUTPUT_PATH")
            .takes_value(true)
            .help("Output image path. When using this option, output won't be piped to stdout.")
            .conflicts_with_all(&[ARG_INPUT_FILE, ARG_OUTPUT_FILE, ARG_LICENSE, ARG_DEP_LICENSES]))
        .arg(Arg::with_name(ARG_OUTPUT_FILE)
            .help("DEPRECATED. Use '--output' instead. (Sets the desired output file. Can only be used in combination with INPUT_FILE.)")
            .value_name("OUTPUT_FILE")
            .requires(ARG_INPUT_FILE)
            .conflicts_with_all(&[ARG_INPUT, ARG_OUTPUT, ARG_LICENSE, ARG_DEP_LICENSES])
            .index(2))

        // config(in):
        .arg(Arg::with_name(ARG_SELECT_FRAME)
            .long("select-frame")
            .value_name("#FRAME")
            .help("Frame to be loaded as still image if the input image is an animated image.\
            To pick the first and last frame respectively, you can provide 'first' and 'last' as arguments. \
            Otherwise provide a single one-indexed positive number which corresponds with the frame index. \
            For example, to select the first frame, the argument would be '1', for the second '2', etc.")
            .takes_value(true))

        // config(out):
        .arg(Arg::with_name(ARG_DISABLE_AUTOMATIC_COLOR_TYPE_ADJUSTMENT)
            .long("disable-automatic-color-type-adjustment")
            .help("Some image output formats do not support the color type of the image buffer prior to encoding. \
            By default the program tries to adjust the color type. If this flag is provided, \
            the program will not try to adjust the color type."))
        .arg(Arg::with_name(ARG_FORCED_OUTPUT_FORMAT)
            .short("f")
            .long("output-format")
            .value_name("FORMAT")
            .help("Force the output image format to use FORMAT, regardless of the (if any) extension of the given output file path. \
                Output formats (FORMAT values) supported: BMP, GIF, ICO, JPEG, PNG, PBM, PGM, PPM and PAM.")
            .takes_value(true))
        .arg(Arg::with_name(ARG_JPEG_ENCODING_QUALITY)
            .long("jpeg-encoding-quality")
            .help("Set the jpeg quality to QUALITY. Valid values are positive numbers from 1 up to and including 100. Will only be used when the output format is determined to be jpeg.")
            .value_name("QUALITY")
            .takes_value(true))
        .arg(Arg::with_name(ARG_PNM_ENCODING_ASCII)
            .long("pnm-encoding-ascii")
            .help("Use ascii based encoding when using a PNM image output format (pbm, pgm or ppm). Doesn't apply to 'pam' (PNM Arbitrary Map)."))

        // image-operations(script):
        .arg(Arg::with_name(ARG_APPLY_OPERATIONS)
            .long("apply-operations")
            .short("x")
            .alias("A")
            .help(HELP_OPERATIONS_AVAILABLE)
            .value_name("OPERATIONS")
            .takes_value(true))

        // image-operations(cli-arguments):
        .group(ArgGroup::with_name(GROUP_IMAGE_OPERATIONS)
            .args(&[
                OP_BLUR,
                OP_BRIGHTEN,
                OP_CONTRAST,
                OP_CROP,
                OP_FILTER3X3,
                OP_FLIP_HORIZONTAL,
                OP_FLIP_VERTICAL,
                OP_GRAYSCALE,
                OP_HUE_ROTATE,
                OP_INVERT,
                OP_RESIZE,
                OP_ROTATE90,
                OP_ROTATE180,
                OP_ROTATE270,
                OP_UNSHARPEN,

                OPMOD_RESIZE_PRESERVE_ASPECT_RATIO,
                OPMOD_RESIZE_SAMPLING_FILTER,
            ])
            .conflicts_with(ARG_APPLY_OPERATIONS)
            .multiple(true))
        .arg(Arg::with_name(OP_BLUR)
            .help("Operation: blur.")
            .long("--blur")
            .takes_value(true)
            .value_name("fp")
            .number_of_values(1)
            .multiple(true)
            .allow_hyphen_values(true))
        .arg(Arg::with_name(OP_BRIGHTEN)
            .help("Operation: brighten.")
            .long("--brighten")
            .takes_value(true)
            .value_name("int")
            .number_of_values(1)
            .multiple(true)
            .allow_hyphen_values(true))
        .arg(Arg::with_name(OP_CONTRAST)
            .help("Operation: contrast.")
            .long("--contrast")
            .takes_value(true)
            .value_name("fp")
            .number_of_values(1)
            .multiple(true)
            .allow_hyphen_values(true))
        .arg(Arg::with_name(OP_CROP)
            .help("Operation: crop.")
            .long("--crop")
            .takes_value(true)
            .value_name("uint uint uint uint")
            .number_of_values(4)
            .multiple(true))
        .arg(Arg::with_name(OP_FILTER3X3)
            .help("Operation: filter3x3.")
            .long("--filter3x3")
            .takes_value(true)
            .value_name("fp fp fp fp fp fp fp fp fp")
            .number_of_values(9)
            .multiple(true)
            .allow_hyphen_values(true))
        .arg(Arg::with_name(OP_FLIP_HORIZONTAL)
            .help("Operation: flip horizontal.")
            .long("--flip-horizontal")
            .multiple(true))
        .arg(Arg::with_name(OP_FLIP_VERTICAL)
            .help("Operation: flip vertical.")
            .long("--flip-vertical")
            .multiple(true))
        .arg(Arg::with_name(OP_GRAYSCALE)
            .help("Operation: grayscale.")
            .long("--grayscale")
            .multiple(true))
        .arg(Arg::with_name(OP_INVERT)
            .help("Operation: invert.")
            .long("--invert")
            .multiple(true))
        .arg(Arg::with_name(OP_HUE_ROTATE)
            .help("Operation: hue rotate.")
            .long("--hue-rotate")
            .takes_value(true)
            .value_name("int")
            .number_of_values(1)
            .multiple(true)
            .allow_hyphen_values(true))
        .arg(Arg::with_name(OP_RESIZE)
            .help("Operation: resize.")
            .long("--resize")
            .takes_value(true)
            .value_name("uint uint")
            .number_of_values(2)
            .multiple(true))
        .arg(Arg::with_name(OP_ROTATE90)
            .help("Operation: rotate 90 degree.")
            .long("--rotate90")
            .multiple(true))
        .arg(Arg::with_name(OP_ROTATE180)
            .help("Operation: rotate 180 degree.")
            .long("--rotate180")
            .multiple(true))
        .arg(Arg::with_name(OP_ROTATE270)
            .help("Operation: rotate 270 degree.")
            .long("--rotate270")
            .multiple(true))
        .arg(Arg::with_name(OP_UNSHARPEN)
            .help("Operation: unsharpen.")
            .long("--unsharpen")
            .takes_value(true)
            .value_name("fp int")
            .number_of_values(2)
            .multiple(true)
            .allow_hyphen_values(true))

        // image-operations(cli-arguments/modifiers):
        .arg(Arg::with_name(OPMOD_RESIZE_PRESERVE_ASPECT_RATIO)
            .help("Operation modifier for: resize")
            .long("--set-resize-preserve-aspect-ratio")
            .takes_value(true)
            .value_name("bool")
            .number_of_values(1)
            .multiple(true)
            .possible_values(&["true", "false"])
        )
        .arg(Arg::with_name(OPMOD_RESIZE_SAMPLING_FILTER)
            .help("Operation modifier for: resize")
            .long("--set-resize-sampling-filter")
            .takes_value(true)
            .value_name("str")
            .number_of_values(1)
            .multiple(true)
            .possible_values(&["catmullrom", "gaussian", "lanczos3", "nearest", "triangle"])
        )
}

// Here any argument should not panic when invalid.
// Previously, it was allowed to panic within Config, but this is no longer the case.
pub fn build_app_config<'a>(matches: &'a ArgMatches) -> Result<Config<'a>, String> {
    let mut builder = ConfigBuilder::new();

    // organisational/licenses:
    let texts_requested = (
        matches.is_present(ARG_LICENSE),
        matches.is_present(ARG_DEP_LICENSES),
    );

    match texts_requested {
        (true, false) => {
            builder = builder.show_license_text_of(SelectedLicenses::ThisSoftware);
        }
        (false, true) => {
            builder = builder.show_license_text_of(SelectedLicenses::Dependencies);
        }
        (true, true) => {
            builder = builder.show_license_text_of(SelectedLicenses::ThisSoftwarePlusDependencies);
        }
        (false, false) => (),
    };

    // io(output):
    if let Some(path) = matches
        .value_of(ARG_OUTPUT)
        .or_else(|| matches.value_of(ARG_OUTPUT_FILE))
    {
        builder = builder.output_path(path);
    }

    // config(in)/gif-select-frame:
    if let Some(frame_in) = matches.value_of(ARG_SELECT_FRAME) {
        let frame_out = match frame_in {
            "first" => FrameIndex::First,
            "last" => FrameIndex::Last,
            n => {
                let pick = n.parse::<usize>().map_err(|_| {
                    "Provided argument for --select-frame is not a valid option. \
                     Valid options are 'first', 'last' or a (one-indexed) positive number."
                        .to_string()
                })?;

                if pick == 0 {
                    return Err(
                        "Provided argument for --select-frame is not a valid option. \
                         If a number is provided, the number should be positive and larger than 0. \
                         To select the first frame, provide the argument '1'."
                            .to_string(),
                    );
                }

                FrameIndex::Nth(pick - 1)
            }
        };

        builder = builder.select_frame(frame_out);
    }

    // config(out)/disable-automatic-color-type-adjustment:
    if matches.is_present(ARG_DISABLE_AUTOMATIC_COLOR_TYPE_ADJUSTMENT) {
        builder = builder.disable_automatic_color_type_adjustment(true);
    }

    // config(out)/output-format:
    if let Some(format) = matches.value_of(ARG_FORCED_OUTPUT_FORMAT) {
        builder = builder.forced_output_format(format);
    }

    // config(out)/jpeg-encoding-quality:
    if let Some(value) = matches.value_of(ARG_JPEG_ENCODING_QUALITY) {
        let requested_jpeg_quality = u8::from_str(value)
            .map_err(|_| {
                "JPEG Encoding quality should be a value between 1 and 100 (inclusive).".to_string()
            })
            .and_then(validate_jpeg_quality)?;
        builder = builder.jpeg_quality(requested_jpeg_quality);
    }

    // config(out)/pnm-encoding-type:
    if matches.is_present(ARG_PNM_ENCODING_ASCII) {
        builder = builder.pnm_format_type(true);
    }

    // image-operations:
    //
    // Image operations are a bit more involved.
    // Thanks to clap, we know either ARG_APPLY_OPERATIONS xor GROUP_IMAGE_OPERATIONS
    // will be the method of providing an image operations program.
    //
    // However with Arg::multiple(true) and Arg::number_of_values(n) we can set to allow multiple
    // operations, like: --blur --blur (multiple is ok), and --crop 0 0 1 1 (number of values = 4),
    // but Clap also allows: --crop 0 0 --crop 1 1 (multiple is ok and number of values = 4).
    //
    // We want to set multiple to true, because image operations can be repeated and can have different
    // effects than the first time.
    // But since the effects can be different, we need to know the order in which arguments are
    // provided. Luckily Clap does tell us the indices of values if we ask for them.
    // If we use --crop 0 0 --blur 1 --crop 1 1, the order of the operations would be undefined, not to
    // say perhaps feel not logical for a user. Therefor, we enforce left to right ordering of
    // operations and require all values to be provided at once, after the operation argument.
    //
    // There is an edge case which we can't (as far as I am aware) handle without looking within the
    // argv ourselves: --crop 0 0 1 1 --crop, is valid according to Clap. However, since we do not
    // receive the amount of times --crop was defined, but rather all the separate provided values for
    // the name of the argument, we just know that for `crop` we have values 0,0,1,1.
    let program = if let Some(script) = matches.value_of(ARG_APPLY_OPERATIONS) {
        sic_parser::parse_script(script)?
    } else {
        let mut tree: IndexTree = BTreeMap::new();
        build_ast_from_matches(matches, &mut tree)?
    };
    builder = builder.image_operations_program(program);

    Ok(builder.build())
}

fn build_ast_from_matches(
    matches: &ArgMatches,
    tree: &mut IndexTree,
) -> Result<Vec<Statement>, String> {
    let operations = vec![
        // operations
        OperationId::Blur,
        OperationId::Brighten,
        OperationId::Contrast,
        OperationId::Crop,
        OperationId::Filter3x3,
        OperationId::FlipH,
        OperationId::FlipV,
        OperationId::Grayscale,
        OperationId::HueRotate,
        OperationId::Invert,
        OperationId::Resize,
        OperationId::Rotate90,
        OperationId::Rotate180,
        OperationId::Rotate270,
        OperationId::Unsharpen,
        // modifiers
        OperationId::ModResizeSamplingFilter,
        OperationId::ModResizePreserveAspectRatio,
    ];
    ast_extend_with_operation(tree, matches, operations)?;

    // Build!
    ast_from_index_tree(tree)
}

fn ast_extend_with_operation<T: IntoIterator<Item = OperationId>>(
    tree: &mut IndexTree,
    matches: &ArgMatches,
    operations: T,
) -> Result<(), String> {
    for operation in operations {
        let argc = operation.takes_number_of_arguments();
        let ops = mk_ops(operation, matches);
        extend_index_tree_with_unification(tree, ops, argc)?;
    }

    Ok(())
}

fn mk_ops(op: OperationId, matches: &ArgMatches) -> Option<IndexedOps> {
    let argc = op.takes_number_of_arguments();
    match argc {
        0 => op_valueless!(matches, op),
        _n => op_with_values!(matches, op),
    }
}

fn ast_from_index_tree(tree: &mut IndexTree) -> Result<Vec<Statement>, String> {
    tree.iter()
        .map(|(_index, op)| match op {
            Op::Bare(id) => {
                let empty: &[&str; 0] = &[];
                id.mk_statement(empty)
            }
            Op::WithValues(id, values) => id.mk_statement(values),
        })
        .collect::<Result<Vec<Statement>, String>>()
}

#[cfg(test)]
mod tests {
    use super::*;
    use sic_image_engine::engine::Statement;
    use sic_image_engine::Operation;
    use std::collections::BTreeMap;

    macro_rules! assert_match {
        ($iter:expr, $clause:pat, $assert:expr) => {{
            match $iter.next().unwrap() {
                $clause => $assert,
                err => panic!(format!(
                    "Assertion: {} failed. Value found was: {:?}",
                    stringify!($assert),
                    err
                )),
            }
        }};
    }

    #[test]
    fn build_from_args_all() {
        let input = "sic -i in -o out \
                     --blur 1 \
                     --brighten 2 \
                     --contrast 3 \
                     --crop 0 0 2 2 \
                     --filter3x3 0 1 2 3 4 5 6 7 8 \
                     --flip-horizontal \
                     --flip-vertical \
                     --grayscale \
                     --hue-rotate -90 \
                     --invert \
                     --resize 10 10 \
                     --rotate90 \
                     --rotate180 \
                     --rotate270 \
                     --unsharpen 1.5 1";

        let input = input.split_ascii_whitespace();
        let matches = cli().get_matches_from(input);
        let mut tree: IndexTree = BTreeMap::new();
        let ast = build_ast_from_matches(&matches, &mut tree);
        let ast = ast.unwrap();
        let mut iter = ast.iter();

        assert_match!(
            iter,
            Statement::Operation(Operation::Blur(n)),
            assert_eq!(*n, 1f32)
        );

        assert_match!(
            iter,
            Statement::Operation(Operation::Brighten(n)),
            assert_eq!(*n, 2i32)
        );

        assert_match!(
            iter,
            Statement::Operation(Operation::Contrast(n)),
            assert_eq!(*n, 3f32)
        );

        assert_match!(
            iter,
            Statement::Operation(Operation::Crop(n)),
            assert_eq!(*n, (0u32, 0u32, 2u32, 2u32))
        );

        assert_match!(
            iter,
            Statement::Operation(Operation::Filter3x3(n)),
            assert_eq!(*n, [0f32, 1f32, 2f32, 3f32, 4f32, 5f32, 6f32, 7f32, 8f32])
        );

        assert_match!(iter, Statement::Operation(Operation::FlipHorizontal), ());

        assert_match!(iter, Statement::Operation(Operation::FlipVertical), ());

        assert_match!(iter, Statement::Operation(Operation::GrayScale), ());

        assert_match!(
            iter,
            Statement::Operation(Operation::HueRotate(n)),
            assert_eq!(*n, -90i32)
        );

        assert_match!(iter, Statement::Operation(Operation::Invert), ());

        assert_match!(
            iter,
            Statement::Operation(Operation::Resize(n)),
            assert_eq!(*n, (10u32, 10u32))
        );

        assert_match!(iter, Statement::Operation(Operation::Rotate90), ());

        assert_match!(iter, Statement::Operation(Operation::Rotate180), ());

        assert_match!(iter, Statement::Operation(Operation::Rotate270), ());

        assert_match!(
            iter,
            Statement::Operation(Operation::Unsharpen(n)),
            assert_eq!(*n, (1.5f32, 1i32))
        );

        assert_eq!(iter.next(), None);
    }

    #[test]
    fn mk_ops_0() {
        let input = "sic -i in -o out \
                     --rotate180";

        let input = input.split_ascii_whitespace();
        let matches = cli().get_matches_from(input);

        let ops = mk_ops(OperationId::Rotate180, &matches);
        let ops = ops.unwrap();
        let (i, v) = ops.get(0).unwrap();
        assert_eq!(*i, 5usize);
        assert_eq!(*v, Op::Bare(OperationId::Rotate180))
    }

    #[test]
    fn mk_ops_n() {
        let input = "sic -i in -o out --unsharpen 1.5 2";

        let input = input.split_ascii_whitespace();
        let matches = cli().get_matches_from(input);

        // note that at mk_ops no unification of arguments has taken place.
        let ops = mk_ops(OperationId::Unsharpen, &matches);

        let ops = ops.unwrap();
        let (i, v) = ops.get(0).unwrap();
        assert_eq!(*i, 6usize);
        assert_eq!(
            *v,
            Op::WithValues(OperationId::Unsharpen, vec![String::from("1.5")])
        );

        let (i, v) = ops.get(1).unwrap();
        assert_eq!(*i, 7usize);
        assert_eq!(
            *v,
            Op::WithValues(OperationId::Unsharpen, vec![String::from("2")])
        );
    }

    #[test]
    fn ast_from_index_tree_empty() {
        let mut tree: IndexTree = BTreeMap::new();
        let ast = ast_from_index_tree(&mut tree);

        assert!(ast.unwrap().is_empty())
    }

    #[test]
    fn ast_from_index_tree_with_vals() {
        let mut tree: IndexTree = BTreeMap::new();
        tree.insert(
            1,
            Op::WithValues(OperationId::Brighten, vec![String::from("10")]),
        );
        tree.insert(2, Op::Bare(OperationId::FlipV));
        tree.insert(
            3,
            Op::WithValues(OperationId::HueRotate, vec![String::from("-90")]),
        );
        let ast = ast_from_index_tree(&mut tree);

        let iter = ast.unwrap();
        let mut iter = iter.iter();

        // can't assert eq, because Operation does not implement Eq, since f32 doesn't support it
        assert_match!(
            iter,
            Statement::Operation(Operation::Brighten(n)),
            assert_eq!(*n, 10)
        );

        assert_match!(iter, Statement::Operation(Operation::FlipVertical), ());

        assert_match!(
            iter,
            Statement::Operation(Operation::HueRotate(n)),
            assert_eq!(*n, -90)
        );

        assert_eq!(iter.next(), None);
    }
}

use std::path::Path;

use sic_core::image;

use crate::conversion::{AutomaticColorTypeAdjustment, ConversionWriter};

pub fn export<P: AsRef<Path>>(
    image: &image::DynamicImage,
    method: ExportMethod<P>,
    format: image::ImageOutputFormat,
    export_settings: ExportSettings,
) -> Result<(), String> {
    let writer = ConversionWriter::new(image);
    writer.write(method, format, export_settings.adjust_color_type)
}

#[derive(Debug)]
pub struct ExportSettings {
    pub adjust_color_type: AutomaticColorTypeAdjustment,
}

#[derive(Debug)]
pub enum ExportMethod<P: AsRef<Path>> {
    File(P),
    StdoutBytes,
}

// enum variants on type aliases are currently experimental, so we use a function here instead.
pub fn use_stdout_bytes_as_export_method() -> ExportMethod<EmptyPath> {
    ExportMethod::StdoutBytes
}

pub struct EmptyPath;

impl AsRef<Path> for EmptyPath {
    fn as_ref(&self) -> &Path {
        Path::new("")
    }
}

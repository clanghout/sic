#[macro_use]
pub mod app;

pub mod cli_parse;

pub fn get_tool_name() -> &'static str {
    env!("CARGO_PKG_NAME")
}

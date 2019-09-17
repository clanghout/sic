use sic_lib::app::cli::build_app_config;
use sic_lib::app::run_mode::{run, run_display_licenses};
use sic_lib::cli_parse::prototype;

//+begin: debug
#[allow(unreachable_code)]
//+end: debug
fn main() -> Result<(), String> {
    //+begin: debug
    let vec = prototype(std::env::args());

    for (i, token) in vec.iter().enumerate() {
        eprintln!("#{}, token: {:?}\n\n", i, token);
    }

    std::process::exit(0);
    //+end: debug

    let app = sic_lib::app::cli::cli();
    let matches = app.get_matches();

    let license_display = matches.is_present("license") || matches.is_present("dep_licenses");

    let configuration = build_app_config(&matches)?;

    if license_display {
        run_display_licenses(&configuration)
    } else {
        run(&matches, &configuration)
    }
}

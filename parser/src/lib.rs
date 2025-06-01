use std::sync::Once;

use log::LevelFilter;
use simple_logger::SimpleLogger;

pub mod ast;
pub mod parser;

static INIT: Once = Once::new();
pub fn init_logger() {
    INIT.call_once(|| {
        SimpleLogger::new()
            .with_level(LevelFilter::Info)
            .init()
            .unwrap();
    });
}

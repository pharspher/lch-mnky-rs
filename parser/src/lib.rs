use std::sync::Once;

pub mod ast;
pub mod parser;

static INIT: Once = Once::new();
pub fn init_logger() {
    INIT.call_once(|| {
        // SimpleLogger::new()
        //     .with_level(LevelFilter::Info)
        //     .init()
        //     .unwrap();

        tracing_subscriber::fmt()
            .with_max_level(tracing::Level::DEBUG)
            .with_target(false)
            .init();
    });
}

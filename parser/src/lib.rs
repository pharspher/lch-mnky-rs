use std::cell::RefCell;
use std::sync::Once;
use tracing::Level;
use tracing_subscriber::FmtSubscriber;
use tracing_subscriber::fmt::format::FmtSpan;

pub mod ast;
pub mod parser;

static INIT: Once = Once::new();
pub fn init_logger() {
    INIT.call_once(|| {
        let subscriber = FmtSubscriber::builder()
            .with_max_level(Level::TRACE)
            .with_span_events(FmtSpan::ENTER | FmtSpan::EXIT)
            .finish();
        tracing::subscriber::set_global_default(subscriber)
            .expect("Failed to set global default subscriber");
    });
}

thread_local! {
    static INDENT_STACK: RefCell<Vec<Option<&'static str>>> = const { RefCell::new(vec![]) };
}

pub struct IndentGuard;
impl IndentGuard {
    pub fn new(label: Option<&'static str>) -> Self {
        INDENT_STACK.with(|stack| {
            if let Ok(mut ref_stack) = stack.try_borrow_mut() {
                ref_stack.push(label);
            } else {
                tracing::warn!("Failed to init IndentGuard: RefCell is already borrowed");
            }
        });
        IndentGuard
    }

    pub fn indent() -> String {
        INDENT_STACK.with(|stack| {
            if let Ok(ref_stack) = stack.try_borrow() {
                " ".repeat(ref_stack.len() * 2)
            } else {
                String::new()
            }
        })
    }

    pub fn label() -> Option<&'static str> {
        INDENT_STACK.with(|stack| {
            if let Ok(ref_stack) = stack.try_borrow() {
                ref_stack.last().copied().flatten()
            } else {
                None
            }
        })
    }
}
impl Drop for IndentGuard {
    fn drop(&mut self) {
        INDENT_STACK.with(|stack| {
            stack.borrow_mut().pop();
        });
    }
}

#[macro_export]
macro_rules! enter {
    () => {
        let _indent_guard = $crate::IndentGuard::new(None);
    };

    ($label:literal) => {
        let _indent_guard = $crate::IndentGuard::new(Some($label));
    };
}

#[macro_export]
macro_rules! info {
    ($($arg:tt)*) => {
        #[cfg(feature = "indent-log")]
        {
            let indent = $crate::IndentGuard::indent();
            let label = $crate::IndentGuard::label();
            if let Some(label) = label {
                tracing::info!("{}{} {}", indent, label, format!($($arg)*));
            } else {
                tracing::info!("{}{}", indent, format!($($arg)*));
            }
        }

        #[cfg(not(feature = "indent-log"))]
        {
            tracing::info!($($arg)*);
        }
    };
}

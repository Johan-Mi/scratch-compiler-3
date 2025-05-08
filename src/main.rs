#![forbid(unsafe_code)]
#![deny(
    clippy::allow_attributes,
    clippy::allow_attributes_without_reason,
    clippy::let_underscore_untyped,
    unused_results
)]
#![warn(clippy::nursery, clippy::pedantic)]

mod codegen;
mod mir;

fn main() {
    let program = todo!();
    codegen::compile(&program);
}

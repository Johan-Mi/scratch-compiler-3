#![forbid(unsafe_code)]
#![deny(
    clippy::allow_attributes,
    clippy::allow_attributes_without_reason,
    clippy::let_underscore_untyped
)]
#![warn(clippy::nursery, clippy::pedantic)]

mod mir;

fn main() {
    println!("Hello, world!");
}

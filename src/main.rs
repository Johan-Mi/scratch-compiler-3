#![forbid(unsafe_code)]
#![deny(
    clippy::allow_attributes,
    clippy::allow_attributes_without_reason,
    clippy::let_underscore_untyped,
    unused_results
)]
#![warn(clippy::nursery, clippy::pedantic)]

mod ast;
mod codegen;
mod diagnostics;
mod hir;
mod mir;
mod parser;

fn main() {
    let source_code = "";
    let cst = parser::parse(source_code);
    let ast = ast::Ast::cast(&cst);
    let hir = hir::lower(&ast);
    let mir = mir::lower(&hir);
    codegen::compile(&mir);
}

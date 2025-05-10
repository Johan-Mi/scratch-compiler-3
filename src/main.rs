#![deny(unsafe_code)]
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
#[expect(
    unsafe_code,
    reason = "`SyntaxKind` cast uses `transmute`, which is not truly required but is way less boilerplate-y than the safe way to do it"
)]
mod parser;

fn main() {
    let mut code_map = codemap::CodeMap::new();
    let source_file = code_map.add_file(String::new(), String::new());
    let mut diagnostics = diagnostics::Diagnostics::default();
    let cst = parser::parse(&source_file, &mut diagnostics);
    let ast = ast::Ast::cast(&cst);
    let hir = hir::lower(&ast);
    let mir = mir::lower(&hir);
    codegen::compile(&mir);
}

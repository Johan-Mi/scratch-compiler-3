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

use codemap::CodeMap;
use diagnostics::Diagnostics;
use std::process::ExitCode;

fn main() -> ExitCode {
    let mut code_map = codemap::CodeMap::new();
    let mut diagnostics = Diagnostics::default();
    let res = real_main(&mut code_map, &mut diagnostics);
    diagnostics.show(&code_map);
    match res {
        Ok(()) => ExitCode::SUCCESS,
        Err(()) => ExitCode::FAILURE,
    }
}

fn real_main(code_map: &mut CodeMap, diagnostics: &mut Diagnostics) -> Result<(), ()> {
    let args = std::env::args().skip(1);
    if args.len() == 0 {
        diagnostics.error("no source files provided", []);
        return Err(());
    }

    let mut hir = hir::Program::default();
    for source_path in args {
        let source_code = std::fs::read_to_string(&source_path).map_err(|err| {
            diagnostics.error("failed to read source code", []);
            diagnostics.note(err.to_string(), []);
        })?;
        let source_file = code_map.add_file(source_path, source_code);
        let cst = parser::parse(&source_file, diagnostics);
        let ast: ast::Document = rowan::ast::AstNode::cast(cst).unwrap();
        hir.merge(hir::lower(&ast));
    }

    let mir = mir::lower(&hir);
    codegen::compile(&mir, "project.sb3").map_err(|err| {
        diagnostics.error("failed to create project file", []);
        diagnostics.note(err.to_string(), []);
    })
}

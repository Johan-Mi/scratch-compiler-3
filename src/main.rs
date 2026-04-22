mod ast;
mod codegen;
#[cfg(debug_assertions)]
mod debug;
mod diagnostics;
mod either;
mod mir;
mod name;
mod parser;
mod ty;

use crate::ast::Node as _;
use codemap::CodeMap;
use diagnostics::Diagnostics;
use std::{collections::HashMap, process::ExitCode};

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

    let mut string_literals = HashMap::new();
    let mut builder = cst::Builder::default();

    let builtins = include_str!("builtins.sc3");
    let builtins = code_map.add_file("<builtins>".to_owned(), builtins.to_owned());
    builder.start_node(parser::K::Program, builtins.span);
    parser::parse(
        &builtins,
        &mut builder,
        &mut string_literals,
        diagnostics,
        true,
    );

    for source_path in args {
        let source_code = std::fs::read_to_string(&source_path).map_err(|err| {
            diagnostics.error("failed to read source code", []);
            diagnostics.note(err.to_string(), []);
        })?;
        let source_file = code_map.add_file(source_path, source_code);
        parser::parse(
            &source_file,
            &mut builder,
            &mut string_literals,
            diagnostics,
            false,
        );
    }

    builder.finish_node();
    let cst = builder.build();
    let ast = ast::Program::cast(cst.root()).unwrap();

    #[cfg(debug_assertions)]
    if std::env::var_os("DUMP_CST").is_some() {
        debug::print_cst(ast.syntax(), 0);
    }

    let resolved_variables = crate::name::resolve(ast, code_map, diagnostics);
    let typing = ty::check(ast, code_map, &resolved_variables, diagnostics);

    if diagnostics.have_errors() {
        return Err(());
    }

    let layouts = ty::layout::s(ast, &typing.type_expressions, diagnostics);

    if diagnostics.have_errors() {
        return Err(());
    }

    let mut mir = mir::lower(ast, code_map, &resolved_variables, &typing, &layouts);
    mir::linearity::spill(&mut mir);

    codegen::compile(code_map, &string_literals, ast, &mir, "project.sb3").map_err(|err| {
        diagnostics.error("failed to create project file", []);
        diagnostics.note(err.to_string(), []);
    })
}

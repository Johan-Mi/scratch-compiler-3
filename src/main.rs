mod ast;
mod codegen;
#[cfg(debug_assertions)]
mod debug;
mod diagnostics;
mod mir;
mod name;
mod parser;
mod ty;

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

    let builtins = include_str!("builtins.sc3");
    let builtins = code_map.add_file("<builtins>".to_owned(), builtins.to_owned());
    let builtins = parser::parse(&builtins, &mut string_literals, diagnostics);

    let csts = std::iter::once(Ok(builtins))
        .chain(args.map(|source_path| {
            let source_code = std::fs::read_to_string(&source_path).map_err(|err| {
                diagnostics.error("failed to read source code", []);
                diagnostics.note(err.to_string(), []);
            })?;
            let source_file = code_map.add_file(source_path, source_code);
            Ok(parser::parse(
                &source_file,
                &mut string_literals,
                diagnostics,
            ))
        }))
        .collect::<Result<Vec<_>, _>>()?;

    #[cfg(debug_assertions)]
    if std::env::var("DUMP_CST").is_ok() {
        for cst in &csts {
            debug::print_cst(cst.root(), 0);
        }
    }

    ty::check(&csts, code_map, diagnostics);

    if diagnostics.have_errors() {
        return Err(());
    }

    let mut mir = mir::lower();
    mir::dce::perform(&mut mir);

    codegen::compile(code_map, &csts, &mir, "project.sb3").map_err(|err| {
        diagnostics.error("failed to create project file", []);
        diagnostics.note(err.to_string(), []);
    })
}

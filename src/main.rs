mod ast;
mod codegen;
#[cfg(debug_assertions)]
mod debug;
mod diagnostics;
mod hir;
mod mir;
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

    let csts = args
        .map(|source_path| {
            let source_code = std::fs::read_to_string(&source_path).map_err(|err| {
                diagnostics.error("failed to read source code", []);
                diagnostics.note(err.to_string(), []);
            })?;
            let source_file = code_map.add_file(source_path, source_code);
            Ok(parser::parse(&source_file, diagnostics))
        })
        .collect::<Result<Vec<_>, _>>()?;

    #[cfg(debug_assertions)]
    if std::env::var("DUMP_CST").is_ok() {
        for cst in &csts {
            debug::print_cst(cst.root(), 0);
        }
    }

    let mut hir = hir::Program;
    let asts: Vec<_> = csts
        .iter()
        .map(|cst| ast::Node::cast(cst.root()).unwrap())
        .inspect(|ast| hir.merge(hir::lower(ast)))
        .collect();

    if !diagnostics.successful() {
        return Err(());
    }

    let mut mir = mir::lower(&hir);
    mir::dce::perform(&mut mir);

    codegen::compile(code_map, &asts, &mir, "project.sb3").map_err(|err| {
        diagnostics.error("failed to create project file", []);
        diagnostics.note(err.to_string(), []);
    })
}

use crate::{ast, mir};
use std::{error::Error, fs::File};

pub fn compile(
    code_map: &codemap::CodeMap,
    asts: &[ast::Document],
    mir: &mir::Program,
    output_path: &str,
) -> Result<(), Box<dyn Error>> {
    let mut project = sb3_builder::Project::default();
    let output_file = File::create(output_path)?;
    for sprite in asts
        .iter()
        .flat_map(|it| it.sprites())
        .filter_map(ast::Sprite::name)
    {
        let span = sprite.span();
        let file = code_map.find_file(span.low());
        let sprite = project.add_sprite(file.source_slice(span).to_owned());
    }
    project.finish(output_file)
}

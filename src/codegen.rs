use crate::{ast, mir};
use std::{error::Error, fs::File};

pub fn compile(
    asts: &[ast::Document],
    mir: &mir::Program,
    output_path: &str,
) -> Result<(), Box<dyn Error>> {
    let mut project = sb3_builder::Project::default();
    let output_file = File::create(output_path)?;
    for sprite in asts
        .iter()
        .flat_map(ast::Document::sprites)
        .filter_map(|it| it.name())
    {
        project.add_sprite(sprite.to_string());
    }
    project.finish(output_file)
}

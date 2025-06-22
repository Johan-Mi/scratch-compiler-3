use crate::mir;
use std::{error::Error, fs::File};

pub fn compile(program: &mir::Program, output_path: &str) -> Result<(), Box<dyn Error>> {
    let project = sb3_builder::Project::default();
    let output_file = File::create(output_path)?;
    project.finish(output_file)
}

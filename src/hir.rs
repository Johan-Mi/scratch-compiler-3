mod lowering;

pub use lowering::lower;

use crate::ast;

#[derive(Default)]
pub struct Program {
    sprites: Vec<ast::Sprite>,
}

impl Program {
    pub fn merge(&mut self, other: Program) {
        let Self { sprites } = other;
        self.sprites.extend(sprites);
    }
}

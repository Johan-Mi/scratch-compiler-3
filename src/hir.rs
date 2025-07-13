mod lowering;

pub use lowering::lower;

use crate::ast;

#[derive(Default)]
pub struct Program {
    sprites: Vec<ast::Sprite>,
}

impl Program {
    pub fn merge(&mut self, other: Self) {
        let Self { sprites } = other;
        self.sprites.extend(sprites);
    }
}

use crate::{hir, mir};
use map::Map;

pub fn lower(hir: &hir::Program) -> mir::Program {
    // TODO
    mir::Program {
        parameters: Map::default(),
        basic_blocks: Map::default(),
        ops: Map::default(),
        variables: Map::default(),
        lists: Map::default(),
        returns: Map::default(),
    }
}

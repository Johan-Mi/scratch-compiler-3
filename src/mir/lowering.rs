use crate::{hir, mir};
use beach_map::BeachMap;

pub fn lower(hir: &hir::Program) -> mir::Program {
    // TODO
    mir::Program {
        parameters: BeachMap::new(),
        basic_blocks: BeachMap::new(),
        ops: BeachMap::new(),
        variables: BeachMap::new(),
    }
}

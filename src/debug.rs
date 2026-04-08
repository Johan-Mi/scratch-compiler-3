use crate::parser::K;

pub fn print_cst(node: cst::Node<K>, indent: usize) {
    println!("{:indent$}{:?}", "", node.kind(), indent = indent);
    node.children()
        .for_each(|child| print_cst(child, indent + 2));
}

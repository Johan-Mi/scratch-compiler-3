use crate::parser::SyntaxNode;

pub fn print_cst(node: SyntaxNode, indent: usize) {
    println!("{:indent$}{:?}", "", node.kind(), indent = indent);
    node.children()
        .for_each(|child| print_cst(child, indent + 2));
}

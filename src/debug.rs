use crate::parser::SyntaxNode;

pub fn print_cst(node: SyntaxNode, indent: usize) {
    println!("{:indent$}{:?}", "", node.kind(), indent = indent);
    for child in node.children() {
        print_cst(child, indent + 2);
    }
}

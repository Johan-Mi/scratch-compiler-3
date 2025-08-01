use crate::parser::{
    SyntaxKind::{self, *},
    SyntaxNode,
};

pub trait Node<'src>: Sized {
    fn cast(syntax: SyntaxNode<'src>) -> Option<Self>;

    fn syntax(self) -> SyntaxNode<'src>;
}

macro_rules! node {
    ($Name:ident: $kind:expr) => {
        #[derive(Clone, Copy)]
        pub struct $Name<'src> {
            syntax: SyntaxNode<'src>,
        }

        impl<'src> Node<'src> for $Name<'src> {
            fn cast(syntax: SyntaxNode<'src>) -> Option<Self> {
                (syntax.kind() == $kind).then_some(Self { syntax })
            }

            fn syntax(self) -> SyntaxNode<'src> {
                self.syntax
            }
        }
    };
}

node!(Document: DOCUMENT);

impl<'src> Document<'src> {
    pub fn structs(self) -> impl Iterator<Item = Struct<'src>> {
        children(self.syntax)
    }

    pub fn sprites(self) -> impl Iterator<Item = Sprite<'src>> {
        children(self.syntax)
    }

    pub fn functions(self) -> impl Iterator<Item = Function<'src>> {
        children(self.syntax)
    }

    pub fn lets(self) -> impl Iterator<Item = Let<'src>> {
        children(self.syntax)
    }
}

node!(Struct: STRUCT);

impl<'src> Struct<'src> {
    pub fn name(self) -> Option<SyntaxNode<'src>> {
        token(self.syntax, IDENTIFIER)
    }

    pub fn fields(self) -> impl Iterator<Item = Field<'src>> {
        children(self.syntax)
    }
}

node!(Field: FIELD_DEFINITION);

impl<'src> Field<'src> {
    pub fn name(self) -> SyntaxNode<'src> {
        token(self.syntax, IDENTIFIER).unwrap()
    }

    pub fn ty(self) -> Option<Expression<'src>> {
        child(self.syntax)
    }
}

node!(Sprite: SPRITE);

impl<'src> Sprite<'src> {
    pub fn name(self) -> Option<SyntaxNode<'src>> {
        token(self.syntax, IDENTIFIER)
    }

    pub fn costume_lists(self) -> impl Iterator<Item = CostumeList<'src>> {
        children(self.syntax)
    }

    pub fn functions(self) -> impl Iterator<Item = Function<'src>> {
        children(self.syntax)
    }

    pub fn lets(self) -> impl Iterator<Item = Let<'src>> {
        children(self.syntax)
    }
}

node!(CostumeList: COSTUME_LIST);

impl<'src> CostumeList<'src> {
    pub fn iter(self) -> impl Iterator<Item = Costume<'src>> {
        children(self.syntax)
    }
}

node!(Costume: COSTUME);

impl<'src> Costume<'src> {
    pub fn name(self) -> Option<SyntaxNode<'src>> {
        self.syntax
            .children()
            .take_while(|it| it.kind() != COLON)
            .find(|it| it.kind() == STRING)
    }

    pub fn path(self) -> Option<SyntaxNode<'src>> {
        let colon = token(self.syntax, COLON)?;
        self.syntax
            .children()
            .find(|it| it.span().low() >= colon.span().high() && it.kind() == STRING)
    }
}

node!(Function: FN);

impl<'src> Function<'src> {
    pub fn kw_inline(self) -> Option<SyntaxNode<'src>> {
        token(self.syntax, KW_INLINE)
    }

    pub fn name(self) -> Option<SyntaxNode<'src>> {
        token(self.syntax, IDENTIFIER)
    }

    pub fn tag(self) -> Option<SyntaxNode<'src>> {
        token(self.syntax, STRING)
    }

    pub fn generics(self) -> Option<Generics<'src>> {
        child(self.syntax)
    }

    pub fn parameters(self) -> Option<FunctionParameters<'src>> {
        child(self.syntax)
    }

    pub fn return_ty(self) -> Option<Expression<'src>> {
        child(self.syntax)
    }

    pub fn body(self) -> Option<Block<'src>> {
        child(self.syntax)
    }
}

node!(Generics: GENERICS);

impl<'src> Generics<'src> {
    pub fn iter(self) -> impl Iterator<Item = SyntaxNode<'src>> {
        self.syntax.children().filter(|it| it.kind() == IDENTIFIER)
    }
}

node!(FunctionParameters: FUNCTION_PARAMETERS);

impl<'src> FunctionParameters<'src> {
    pub fn parameters(self) -> impl Iterator<Item = Parameter<'src>> {
        children(self.syntax)
    }
}

node!(Parameter: PARAMETER);

impl<'src> Parameter<'src> {
    pub fn external_name(self) -> Option<ExternalParameterName<'src>> {
        child(self.syntax)
    }

    pub fn internal_name(self) -> Option<SyntaxNode<'src>> {
        token(self.syntax, IDENTIFIER)
    }

    pub fn ty(self) -> Option<Expression<'src>> {
        child(self.syntax)
    }
}

node!(ExternalParameterName: EXTERNAL_PARAMETER_NAME);

impl<'src> ExternalParameterName<'src> {
    pub fn identifier(self) -> SyntaxNode<'src> {
        token(self.syntax, IDENTIFIER).unwrap()
    }
}

node!(Block: BLOCK);

impl<'src> Block<'src> {
    pub fn statements(self) -> impl Iterator<Item = Statement<'src>> {
        children(self.syntax)
    }
}

pub enum Statement<'src> {
    Let(Let<'src>),
    If(If<'src>),
    Repeat(Repeat<'src>),
    Forever(Forever<'src>),
    While(While<'src>),
    Until(Until<'src>),
    For(For<'src>),
    Return(Return<'src>),
    Expr(Expression<'src>),
}

impl<'src> Node<'src> for Statement<'src> {
    fn cast(node: SyntaxNode<'src>) -> Option<Self> {
        match node.kind() {
            LET => Node::cast(node).map(Self::Let),
            IF => Node::cast(node).map(Self::If),
            REPEAT => Node::cast(node).map(Self::Repeat),
            FOREVER => Node::cast(node).map(Self::Forever),
            WHILE => Node::cast(node).map(Self::While),
            UNTIL => Node::cast(node).map(Self::Until),
            FOR => Node::cast(node).map(Self::For),
            RETURN => Node::cast(node).map(Self::Return),
            _ => Expression::cast(node).map(Self::Expr),
        }
    }

    fn syntax(self) -> SyntaxNode<'src> {
        match self {
            Self::Let(inner) => inner.syntax,
            Self::If(inner) => inner.syntax,
            Self::Repeat(inner) => inner.syntax,
            Self::Forever(inner) => inner.syntax,
            Self::While(inner) => inner.syntax,
            Self::Until(inner) => inner.syntax,
            Self::For(inner) => inner.syntax,
            Self::Return(inner) => inner.syntax,
            Self::Expr(inner) => inner.syntax(),
        }
    }
}

node!(Let: LET);

impl<'src> Let<'src> {
    pub fn variable(self) -> Option<SyntaxNode<'src>> {
        token(self.syntax, IDENTIFIER)
    }

    pub fn value(self) -> Option<Expression<'src>> {
        child(self.syntax)
    }
}

node!(If: IF);

impl<'src> If<'src> {
    pub fn condition(self) -> Option<Expression<'src>> {
        child(self.syntax)
    }

    pub fn then(self) -> Option<Block<'src>> {
        child(self.syntax)
    }

    pub fn else_clause(self) -> Option<ElseClause<'src>> {
        child(self.syntax)
    }
}

node!(ElseClause: ELSE_CLAUSE);

impl<'src> ElseClause<'src> {
    pub fn block(self) -> Option<Block<'src>> {
        child(self.syntax)
    }

    pub fn if_(self) -> Option<If<'src>> {
        child(self.syntax)
    }
}

node!(Repeat: REPEAT);

impl<'src> Repeat<'src> {
    pub fn times(self) -> Option<Expression<'src>> {
        child(self.syntax)
    }

    pub fn body(self) -> Option<Block<'src>> {
        child(self.syntax)
    }
}

node!(Forever: FOREVER);

impl<'src> Forever<'src> {
    pub fn body(self) -> Option<Block<'src>> {
        child(self.syntax)
    }
}

node!(While: WHILE);

impl<'src> While<'src> {
    pub fn condition(self) -> Option<Expression<'src>> {
        child(self.syntax)
    }

    pub fn body(self) -> Option<Block<'src>> {
        child(self.syntax)
    }
}

node!(Until: UNTIL);

impl<'src> Until<'src> {
    pub fn condition(self) -> Option<Expression<'src>> {
        child(self.syntax)
    }

    pub fn body(self) -> Option<Block<'src>> {
        child(self.syntax)
    }
}

node!(For: FOR);

impl<'src> For<'src> {
    pub fn variable(self) -> Option<SyntaxNode<'src>> {
        token(self.syntax, IDENTIFIER)
    }

    pub fn times(self) -> Option<Expression<'src>> {
        child(self.syntax)
    }

    pub fn body(self) -> Option<Block<'src>> {
        child(self.syntax)
    }
}

node!(Return: RETURN);

impl<'src> Return<'src> {
    pub fn expression(self) -> Option<Expression<'src>> {
        child(self.syntax)
    }
}

pub enum Expression<'src> {
    Parenthesized(ParenthesizedExpression<'src>),
    Variable(Variable<'src>),
    FunctionCall(FunctionCall<'src>),
    BinaryOperation(BinaryOperation<'src>),
    NamedArgument(NamedArgument<'src>),
    Literal(Literal<'src>),
    Lvalue(Lvalue<'src>),
    GenericTypeInstantiation(GenericTypeInstantiation<'src>),
    ListLiteral(ListLiteral<'src>),
    TypeAscription(TypeAscription<'src>),
    MethodCall(MethodCall<'src>),
}

impl<'src> Node<'src> for Expression<'src> {
    fn cast(node: SyntaxNode<'src>) -> Option<Self> {
        match node.kind() {
            PARENTHESIZED_EXPRESSION => Node::cast(node).map(Self::Parenthesized),
            VARIABLE => Node::cast(node).map(Self::Variable),
            FUNCTION_CALL => Node::cast(node).map(Self::FunctionCall),
            BINARY_EXPRESSION => Node::cast(node).map(Self::BinaryOperation),
            NAMED_ARGUMENT => Node::cast(node).map(Self::NamedArgument),
            LITERAL => Node::cast(node).map(Self::Literal),
            LVALUE => Node::cast(node).map(Self::Lvalue),
            GENERIC_TYPE_INSTANTIATION => Node::cast(node).map(Self::GenericTypeInstantiation),
            LIST_LITERAL => Node::cast(node).map(Self::ListLiteral),
            TYPE_ASCRIPTION => Node::cast(node).map(Self::TypeAscription),
            METHOD_CALL => Node::cast(node).map(Self::MethodCall),
            _ => None,
        }
    }

    fn syntax(self) -> SyntaxNode<'src> {
        match self {
            Self::Variable(inner) => inner.syntax,
            Self::FunctionCall(inner) => inner.syntax,
            Self::BinaryOperation(inner) => inner.syntax,
            Self::Parenthesized(inner) => inner.syntax,
            Self::NamedArgument(inner) => inner.syntax,
            Self::Literal(inner) => inner.syntax,
            Self::Lvalue(inner) => inner.syntax,
            Self::GenericTypeInstantiation(inner) => inner.syntax,
            Self::ListLiteral(inner) => inner.syntax,
            Self::TypeAscription(inner) => inner.syntax,
            Self::MethodCall(inner) => inner.syntax,
        }
    }
}

node!(ParenthesizedExpression: PARENTHESIZED_EXPRESSION);

impl<'src> ParenthesizedExpression<'src> {
    pub fn inner(self) -> Option<Expression<'src>> {
        child(self.syntax)
    }
}

node!(Variable: VARIABLE);

impl<'src> Variable<'src> {
    pub fn identifier(self) -> SyntaxNode<'src> {
        token(self.syntax, IDENTIFIER).unwrap()
    }
}

node!(FunctionCall: FUNCTION_CALL);

impl<'src> FunctionCall<'src> {
    pub fn name(self) -> SyntaxNode<'src> {
        token(self.syntax, IDENTIFIER).unwrap()
    }

    pub fn args(self) -> Arguments<'src> {
        child(self.syntax).unwrap()
    }
}

node!(Arguments: ARGUMENTS);

impl<'src> Arguments<'src> {
    pub fn iter(self) -> impl Iterator<Item = Expression<'src>> {
        children(self.syntax)
    }
}

node!(BinaryOperation: BINARY_EXPRESSION);

impl<'src> BinaryOperation<'src> {
    pub fn operator(self) -> SyntaxNode<'src> {
        self.syntax
            .children()
            .find(|child| child.kind().is_binary_operator())
            .unwrap()
    }

    pub fn lhs(self) -> Option<Expression<'src>> {
        let operator = self.operator().span().low();
        self.syntax
            .children()
            .take_while(|child| child.span().high() <= operator)
            .find_map(Node::cast)
    }

    pub fn rhs(self) -> Option<Expression<'src>> {
        let operator = self.operator().span().high();
        self.syntax
            .children()
            .skip_while(|child| child.span().low() < operator)
            .find_map(Node::cast)
    }
}

node!(NamedArgument: NAMED_ARGUMENT);

impl<'src> NamedArgument<'src> {
    pub fn name(self) -> SyntaxNode<'src> {
        token(self.syntax, IDENTIFIER).unwrap()
    }

    pub fn value(self) -> Option<Expression<'src>> {
        child(self.syntax)
    }
}

node!(Literal: LITERAL);

node!(Lvalue: LVALUE);

impl<'src> Lvalue<'src> {
    pub fn inner(self) -> Option<Expression<'src>> {
        child(self.syntax)
    }
}

node!(GenericTypeInstantiation: GENERIC_TYPE_INSTANTIATION);

impl<'src> GenericTypeInstantiation<'src> {
    pub fn generic(self) -> Expression<'src> {
        child(self.syntax).unwrap()
    }

    pub fn type_parameters(self) -> TypeParameters<'src> {
        child(self.syntax).unwrap()
    }
}

node!(TypeParameters: TYPE_PARAMETERS);

impl<'src> TypeParameters<'src> {
    pub fn iter(self) -> impl Iterator<Item = Expression<'src>> {
        children(self.syntax)
    }
}

node!(ListLiteral: LIST_LITERAL);

impl<'src> ListLiteral<'src> {
    pub fn iter(self) -> impl Iterator<Item = Expression<'src>> {
        children(self.syntax)
    }

    pub fn lbracket(self) -> SyntaxNode<'src> {
        token(self.syntax, LBRACKET).unwrap()
    }
}

node!(TypeAscription: TYPE_ASCRIPTION);

impl<'src> TypeAscription<'src> {
    pub fn operator(self) -> SyntaxNode<'src> {
        token(self.syntax, KW_AS).unwrap()
    }

    pub fn inner(self) -> Option<Expression<'src>> {
        let operator = self.operator().span().low();
        self.syntax
            .children()
            .take_while(|child| child.span().high() <= operator)
            .find_map(Node::cast)
    }

    pub fn ty(self) -> Option<Expression<'src>> {
        let operator = self.operator().span().high();
        self.syntax
            .children()
            .skip_while(|child| child.span().low() < operator)
            .find_map(Node::cast)
    }
}

node!(MethodCall: METHOD_CALL);

impl<'src> MethodCall<'src> {
    pub fn dot(self) -> SyntaxNode<'src> {
        token(self.syntax, DOT).unwrap()
    }

    pub fn caller(self) -> Expression<'src> {
        let operator = self.dot().span().low();
        self.syntax
            .children()
            .take_while(|child| child.span().high() <= operator)
            .find_map(Node::cast)
            .unwrap()
    }

    pub fn rhs(self) -> Option<Expression<'src>> {
        let operator = self.dot().span().high();
        self.syntax
            .children()
            .skip_while(|child| child.span().low() < operator)
            .find_map(Node::cast)
    }
}

fn child<'src, N: Node<'src>>(syntax: SyntaxNode<'src>) -> Option<N> {
    syntax.children().find_map(N::cast)
}

fn children<'src, N: Node<'src>>(syntax: SyntaxNode<'src>) -> impl Iterator<Item = N> {
    syntax.children().filter_map(N::cast)
}

fn token(syntax: SyntaxNode<'_>, kind: SyntaxKind) -> Option<SyntaxNode<'_>> {
    syntax.children().find(|it| it.kind() == kind)
}

use crate::parser::{K, SyntaxNode};

pub trait Node<'src>: Sized {
    fn cast(syntax: SyntaxNode<'src>) -> Option<Self>;

    fn syntax(self) -> SyntaxNode<'src>;
}

macro_rules! node {
    ($Name:ident) => {
        #[derive(Clone, Copy)]
        pub struct $Name<'src> {
            syntax: SyntaxNode<'src>,
        }

        impl<'src> Node<'src> for $Name<'src> {
            fn cast(syntax: SyntaxNode<'src>) -> Option<Self> {
                (syntax.kind() == K::$Name).then_some(Self { syntax })
            }

            fn syntax(self) -> SyntaxNode<'src> {
                self.syntax
            }
        }
    };
}

node!(Document);

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

node!(Struct);

impl<'src> Struct<'src> {
    pub fn name(self) -> Option<SyntaxNode<'src>> {
        token(self.syntax, K::Identifier)
    }

    pub fn fields(self) -> impl Iterator<Item = FieldDefinition<'src>> {
        children(self.syntax)
    }
}

node!(FieldDefinition);

impl<'src> FieldDefinition<'src> {
    pub fn name(self) -> SyntaxNode<'src> {
        token(self.syntax, K::Identifier).unwrap()
    }

    pub fn ty(self) -> Option<Expression<'src>> {
        child(self.syntax)
    }
}

node!(Sprite);

impl<'src> Sprite<'src> {
    pub fn name(self) -> Option<SyntaxNode<'src>> {
        token(self.syntax, K::Identifier)
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

node!(CostumeList);

impl<'src> CostumeList<'src> {
    pub fn iter(self) -> impl Iterator<Item = Costume<'src>> {
        children(self.syntax)
    }
}

node!(Costume);

impl<'src> Costume<'src> {
    pub fn name(self) -> Option<SyntaxNode<'src>> {
        self.syntax
            .children()
            .take_while(|it| it.kind() != K::Colon)
            .find(|it| it.kind() == K::String)
    }

    pub fn path(self) -> Option<SyntaxNode<'src>> {
        let colon = token(self.syntax, K::Colon)?;
        self.syntax
            .children()
            .find(|it| it.span().low() >= colon.span().high() && it.kind() == K::String)
    }
}

node!(Function);

impl<'src> Function<'src> {
    pub fn kw_inline(self) -> Option<SyntaxNode<'src>> {
        token(self.syntax, K::KwInline)
    }

    pub fn name(self) -> Option<SyntaxNode<'src>> {
        token(self.syntax, K::Identifier)
    }

    pub fn tag(self) -> Option<SyntaxNode<'src>> {
        token(self.syntax, K::String)
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

node!(Generics);

impl<'src> Generics<'src> {
    pub fn iter(self) -> impl Iterator<Item = SyntaxNode<'src>> {
        self.syntax
            .children()
            .filter(|it| it.kind() == K::Identifier)
    }
}

node!(FunctionParameters);

impl<'src> FunctionParameters<'src> {
    pub fn iter(self) -> impl Iterator<Item = Parameter<'src>> {
        children(self.syntax)
    }
}

node!(Parameter);

impl<'src> Parameter<'src> {
    pub fn external_name(self) -> Option<ExternalParameterName<'src>> {
        child(self.syntax)
    }

    pub fn internal_name(self) -> Option<SyntaxNode<'src>> {
        token(self.syntax, K::Identifier)
    }

    pub fn ty(self) -> Option<Expression<'src>> {
        child(self.syntax)
    }
}

node!(ExternalParameterName);

impl<'src> ExternalParameterName<'src> {
    pub fn identifier(self) -> SyntaxNode<'src> {
        token(self.syntax, K::Identifier).unwrap()
    }
}

node!(Block);

impl<'src> Block<'src> {
    pub fn statements(self) -> impl Iterator<Item = Statement<'src>> {
        children(self.syntax)
    }
}

#[derive(Clone, Copy)]
pub enum Statement<'src> {
    Let(Let<'src>),
    If(If<'src>),
    Repeat(Repeat<'src>),
    Forever(Forever<'src>),
    While(While<'src>),
    Until(Until<'src>),
    For(For<'src>),
    Return(Return<'src>),
    Expression(Expression<'src>),
}

impl<'src> Node<'src> for Statement<'src> {
    fn cast(node: SyntaxNode<'src>) -> Option<Self> {
        match node.kind() {
            K::Let => Node::cast(node).map(Self::Let),
            K::If => Node::cast(node).map(Self::If),
            K::Repeat => Node::cast(node).map(Self::Repeat),
            K::Forever => Node::cast(node).map(Self::Forever),
            K::While => Node::cast(node).map(Self::While),
            K::Until => Node::cast(node).map(Self::Until),
            K::For => Node::cast(node).map(Self::For),
            K::Return => Node::cast(node).map(Self::Return),
            _ => Expression::cast(node).map(Self::Expression),
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
            Self::Expression(inner) => inner.syntax(),
        }
    }
}

node!(Let);

impl<'src> Let<'src> {
    pub fn variable(self) -> Option<SyntaxNode<'src>> {
        token(self.syntax, K::Identifier)
    }

    pub fn value(self) -> Option<Expression<'src>> {
        child(self.syntax)
    }
}

node!(If);

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

node!(ElseClause);

impl<'src> ElseClause<'src> {
    pub fn block(self) -> Option<Block<'src>> {
        child(self.syntax)
    }

    pub fn if_(self) -> Option<If<'src>> {
        child(self.syntax)
    }
}

node!(Repeat);

impl<'src> Repeat<'src> {
    pub fn times(self) -> Option<Expression<'src>> {
        child(self.syntax)
    }

    pub fn body(self) -> Option<Block<'src>> {
        child(self.syntax)
    }
}

node!(Forever);

impl<'src> Forever<'src> {
    pub fn body(self) -> Option<Block<'src>> {
        child(self.syntax)
    }
}

node!(While);

impl<'src> While<'src> {
    pub fn condition(self) -> Option<Expression<'src>> {
        child(self.syntax)
    }

    pub fn body(self) -> Option<Block<'src>> {
        child(self.syntax)
    }
}

node!(Until);

impl<'src> Until<'src> {
    pub fn condition(self) -> Option<Expression<'src>> {
        child(self.syntax)
    }

    pub fn body(self) -> Option<Block<'src>> {
        child(self.syntax)
    }
}

node!(For);

impl<'src> For<'src> {
    pub fn variable(self) -> Option<SyntaxNode<'src>> {
        token(self.syntax, K::Identifier)
    }

    pub fn times(self) -> Option<Expression<'src>> {
        child(self.syntax)
    }

    pub fn body(self) -> Option<Block<'src>> {
        child(self.syntax)
    }
}

node!(Return);

impl<'src> Return<'src> {
    pub fn expression(self) -> Option<Expression<'src>> {
        child(self.syntax)
    }
}

#[derive(Clone, Copy)]
pub enum Expression<'src> {
    Parenthesized(Parenthesized<'src>),
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
            K::Parenthesized => Node::cast(node).map(Self::Parenthesized),
            K::Variable => Node::cast(node).map(Self::Variable),
            K::FunctionCall => Node::cast(node).map(Self::FunctionCall),
            K::BinaryOperation => Node::cast(node).map(Self::BinaryOperation),
            K::NamedArgument => Node::cast(node).map(Self::NamedArgument),
            K::Literal => Node::cast(node).map(Self::Literal),
            K::Lvalue => Node::cast(node).map(Self::Lvalue),
            K::GenericTypeInstantiation => Node::cast(node).map(Self::GenericTypeInstantiation),
            K::ListLiteral => Node::cast(node).map(Self::ListLiteral),
            K::TypeAscription => Node::cast(node).map(Self::TypeAscription),
            K::MethodCall => Node::cast(node).map(Self::MethodCall),
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

node!(Parenthesized);

impl<'src> Parenthesized<'src> {
    pub fn inner(self) -> Option<Expression<'src>> {
        child(self.syntax)
    }
}

node!(Variable);

impl<'src> Variable<'src> {
    pub fn identifier(self) -> SyntaxNode<'src> {
        token(self.syntax, K::Identifier).unwrap()
    }
}

node!(FunctionCall);

impl<'src> FunctionCall<'src> {
    pub fn name(self) -> SyntaxNode<'src> {
        token(self.syntax, K::Identifier).unwrap()
    }

    pub fn args(self) -> Arguments<'src> {
        child(self.syntax).unwrap()
    }
}

node!(Arguments);

impl<'src> Arguments<'src> {
    pub fn iter(self) -> impl Iterator<Item = Expression<'src>> {
        children(self.syntax)
    }
}

node!(BinaryOperation);

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

node!(NamedArgument);

impl<'src> NamedArgument<'src> {
    pub fn name(self) -> SyntaxNode<'src> {
        token(self.syntax, K::Identifier).unwrap()
    }

    pub fn value(self) -> Option<Expression<'src>> {
        child(self.syntax)
    }
}

node!(Literal);

impl<'src> Literal<'src> {
    pub fn token(self) -> SyntaxNode<'src> {
        self.syntax.children().next().unwrap()
    }
}

node!(Lvalue);

impl<'src> Lvalue<'src> {
    pub fn inner(self) -> Option<Expression<'src>> {
        child(self.syntax)
    }
}

node!(GenericTypeInstantiation);

impl<'src> GenericTypeInstantiation<'src> {
    pub fn generic(self) -> Expression<'src> {
        child(self.syntax).unwrap()
    }

    pub fn type_parameters(self) -> TypeParameters<'src> {
        child(self.syntax).unwrap()
    }
}

node!(TypeParameters);

impl<'src> TypeParameters<'src> {
    pub fn iter(self) -> impl Iterator<Item = Expression<'src>> {
        children(self.syntax)
    }
}

node!(ListLiteral);

impl<'src> ListLiteral<'src> {
    pub fn iter(self) -> impl Iterator<Item = Expression<'src>> {
        children(self.syntax)
    }

    pub fn lbracket(self) -> SyntaxNode<'src> {
        token(self.syntax, K::Lbracket).unwrap()
    }
}

node!(TypeAscription);

impl<'src> TypeAscription<'src> {
    pub fn operator(self) -> SyntaxNode<'src> {
        token(self.syntax, K::KwAs).unwrap()
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

node!(MethodCall);

impl<'src> MethodCall<'src> {
    pub fn dot(self) -> SyntaxNode<'src> {
        token(self.syntax, K::Dot).unwrap()
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

fn token(syntax: SyntaxNode<'_>, kind: K) -> Option<SyntaxNode<'_>> {
    syntax.children().find(|it| it.kind() == kind)
}

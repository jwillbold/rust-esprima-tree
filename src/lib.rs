mod errors;

#[macro_use]
extern crate serde_derive;
#[macro_use]
extern crate serde;
#[macro_use]
extern crate serde_json;
extern crate serde_test;

// Based on https://developer.mozilla.org/en-US/docs/Mozilla/Projects/SpiderMonkey/Parser_API
pub mod jsast {

    #![allow(dead_code)]
    #![feature(custom_attribute)]
    // #![plugin(serde_macros)]
    // #![plugin(serde)]

    use serde_test::{Token, assert_tokens};
    use serde::ser::{Serialize, Serializer, SerializeStruct};

    use errors::jsast::ParsingError;
    use errors::jsast::ParsingErrorKind;
    use errors::jsast::SerilizationError;
    use errors::jsast::SerilizationErrorKind;

    type Identifier = String;
    type Label = String;

    // interface Node {
    //     type: string;
    //     loc: SourceLocation | null;
    // }
    trait Node: Sized {
        fn to_json(&self) -> Result<serde_json::Value, SerilizationError>;
        fn from_json(json: serde_json::Value) -> Result<Self, ParsingError>;
    }

    // enum Node {
    //     Program(Program),
    //     Function(Function),
    //     Statement(Statement),
    //     Decleration(Decleration),
    //     Expression(Expression),
    //     Pattern(Pattern),
    //     Clause(Clause),
    //     Identifier(Identifier),
    //     Literal(Literal),
    //     Operator(Operator),
    // }

    #[cfg(test)]
    fn check_parsing<T>(json: serde_json::Value) where T: Node {
        let parsed = T::from_json(json.clone()).unwrap();
        assert_eq!(parsed.to_json().unwrap(), json);
    }

    // #[cfg(test)]
    // fn check_json_parsing<T>(json: serde_json::Value) where T: Node {
    //     let parsed = T::from_json(json.clone()).unwrap();
    //     assert_eq!(parsed.to_json().unwrap(), json);
    // }


    // interface Program <: Node {
    //     type: "Program";
    //     body: [ Statement ];
    // }

    #[derive(Debug)]
    #[derive(PartialEq)]
    #[derive(Deserialize)]
    // #[derive(Serialize, Deserialize)]
    struct Program {
        // type: &'static str = "Program",
        // type: &'static str = "Program",
        body: String
        // body: Vec<Statement>,
    }

    impl Serialize for Program {
        fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
            where  S: Serializer,
        {
            let mut state = serializer.serialize_struct("Program", 1)?;
            // state.serialize_field("type", "Program")?;
            state.serialize_field("body", &self.body)?;
            state.end()
        }
    }

    #[test]
    fn test_program_parse_and_serialize() {
        let json = serde_json::json!({
            "type": "Program",
            "body": "[]",
        });

        println!("json: {}", json);

        let prog = Program {
            body: "[]".to_string()
        };


        let json_prog = serde_json::to_string(&prog).unwrap();

        println!("json_prog: {}", json_prog);

        let prog_json: Program = serde_json::from_str(json_prog.as_str()).unwrap();

        assert_eq!(prog_json, prog);

        // check_parsing::<Program>(json);

        assert_tokens(&prog, &[
            Token::Struct { name: "Program", len: 1},
            Token::String("body"),
            Token::Str("[]"),
            Token::StructEnd
        ]);
    }


    #[serde(tag = "type")]
    #[derive(Serialize, Deserialize)]
    enum Expr {
        #[serde(rename = "ThisExpression")]
        This
    }

    #[test]
    fn test_expr_de_se() {
        let e = Expr::This;

        println!("Expr::This: {}", serde_json::to_string(&e).unwrap());
    }



    // impl Node for Program {
    //     fn to_json(&self) -> Result<serde_json::Value, SerilizationError> {
    //         Ok(json!({
    //             "type": "Program",
    //             "body": self.body.iter()
    //                              .map(|stmt| stmt.to_json())
    //                              .collect::<Result<serde_json::Value, SerilizationError>>()?
    //         }))
    //     }
    //
    //     fn from_json(json: serde_json::Value) -> Result<Self, ParsingError> {
    //         assert_eq!(json["type"], "Program");
    //
    //         let p = Program {
    //             body: match json["body"] {
    //                 serde_json::Value::Array(ref a) => {
    //                     Ok(a.iter().map(|stmt| Statement::from_json(stmt.to_owned()))
    //                         .collect::<Result<Vec<Statement>, ParsingError>>()?)
    //                 },
    //                 _ => Err(ParsingError::new(ParsingErrorKind::UnexpectedDataType))
    //             }?
    //         };
    //
    //         Ok(p)
    //     }
    // }

    // #[test]
    // fn test_program_parse_and_serialize() {
    //     let json = serde_json::json!({
    //         "type": "Program",
    //         "body": [],
    //     });
    //
    //     check_parsing::<Program>(json);
    // }

    enum BlockStatementOrExpression {
        Block(BlockStatement),
        Expression(Expression)
    }

    // interface Function <: Node {
    //     id: Identifier | null;
    //     params: [ Pattern ];
    //     defaults: [ Expression ];
    //     rest: Identifier | null;
    //     body: BlockStatement | Expression;
    //     generator: boolean;
    //     expression: boolean;
    // }
    struct Function {
        id: Option<Identifier>,
        params: Vec<Pattern>,
        defaults: Vec<Expression>,
        rest: Option<Identifier>,
        body: BlockStatementOrExpression,
        generator: bool,
        expression: bool
    }

    // interface Statement <: Node { }
    enum Statement {
        Empty,
        Block(BlockStatement),
        Expression(Expression),
        Decleration(Decleration),
        If(IfStatement),
        Break(Option<Label>),
        Continue(Option<Label>),
        With(WithStatement),
        Swich(SwitchStatement),
        Return(Option<Expression>),
        Throw(ThrowStatement),
        Try(TryStatement),
        While(WhileStatement),
        DoWhile(DoWhileStatement),
        For(ForStatement),
        ForIn(ForInStatement),
        ForOf(ForOfStatement),
        Let(LetStatement),
        Debugger,
    }

    impl Node for Statement {
        fn to_json(&self) -> Result<serde_json::Value, SerilizationError> {
            match &self {
                Statement::Empty => Ok(json!({
                    "type": "EmptyStatment",
                })),
                _ => Err(SerilizationError::new(SerilizationErrorKind::ImplementationMissing))
            }
        }

        fn from_json(json: serde_json::Value) -> Result<Self, ParsingError> {
            match json["type"] {
                serde_json::Value::String(ref s) => match s.as_str() {
                    "EmptyStatment" => Ok(Statement::Empty),
                    _ => Err(ParsingError::new(ParsingErrorKind::UnknownType))
                },
                _ => Err(ParsingError::new(ParsingErrorKind::UnexpectedDataType))
            }
        }
    }

    // interface BlockStatement <: Statement {
    //     type: "BlockStatement";
    //     body: [ Statement ];
    // }
    type BlockStatement = Vec<Expression>;

    // interface LabeledStatement <: Statement {
    //     type: "LabeledStatement";
    //     label: Identifier;
    //     body: Statement;
    // }
    struct IfStatement {
        label: Identifier,
        // body: Statement,
    }


    // interface BreakStatement <: Statement {
    //     type: "BreakStatement";
    //     label: Identifier | null;
    // }
    // struct BreakStatement = Option<Label>;

    // interface ContinueStatement <: Statement {
    //     type: "ContinueStatement";
    //     label: Identifier | null;
    // }
    // type ContinueStatement = Option<Label>;

    // interface WithStatement <: Statement {
    //     type: "WithStatement";
    //     object: Expression;
    //     body: Statement;
    // }
    struct WithStatement {
        object: Expression,
        body: Expression,
    }

    // interface SwitchStatement <: Statement {
    //     type: "SwitchStatement";
    //     discriminant: Expression;
    //     cases: [ SwitchCase ];
    //     lexical: boolean;
    // }
    struct SwitchStatement {
        discriminant: Expression,
    }

    // interface ReturnStatement <: Statement {
    //     type: "ReturnStatement";
    //     argument: Expression | null;
    // }
    // type ReturnStatement = Option<Expression>

    // interface ThrowStatement <: Statement {
    //     type: "ThrowStatement";
    //     argument: Expression;
    // }
    struct ThrowStatement {
        argument: Expression,
    }

    // interface TryStatement <: Statement {
    //     type: "TryStatement";
    //     block: BlockStatement;
    //     handler: CatchClause | null;
    //     guardedHandlers: [ CatchClause ];
    //     finalizer: BlockStatement | null;
    // }
    struct TryStatement {

    }

    // interface WhileStatement <: Statement {
    //     type: "WhileStatement";
    //     test: Expression;
    //     body: Statement;
    // }
    struct WhileStatement {

    }

    // interface DoWhileStatement <: Statement {
    //     type: "DoWhileStatement";
    //     body: Statement;
    //     test: Expression;
    // }
    struct DoWhileStatement {

    }

    // interface ForStatement <: Statement {
    //     type: "ForStatement";
    //     init: VariableDeclaration | Expression | null;
    //     test: Expression | null;
    //     update: Expression | null;
    //     body: Statement;
    // }
    struct ForStatement {

    }

    // interface ForInStatement <: Statement {
    //     type: "ForInStatement";
    //     left: VariableDeclaration |  Expression;
    //     right: Expression;
    //     body: Statement;
    //     each: boolean;
    // }
    struct ForInStatement {

    }

    // interface ForOfStatement <: Statement {
    //     type: "ForOfStatement";
    //     left: VariableDeclaration |  Expression;
    //     right: Expression;
    //     body: Statement;
    // }
    struct ForOfStatement {

    }

    // interface LetStatement <: Statement {
    //     type: "LetStatement";
    //     head: [ VariableDeclarator ];
    //     body: Statement;
    // }
    struct LetStatement {
        head: Vec<VariableDeclarator>,
        // body: Statement
    }

    // interface DebuggerStatement <: Statement {
    //     type: "DebuggerStatement";
    // }
    // struct DebuggerStatement { }


    // interface Declaration <: Statement { }
    enum Decleration {
        Function(FunctionDecleration),
        Varibale(VariableDeclaration),
        VariableDeclarator(VariableDeclaration)
    }

    // interface FunctionDeclaration <: Function, Declaration {
    //     type: "FunctionDeclaration";
    //     id: Identifier;
    //     params: [ Pattern ];
    //     defaults: [ Expression ];
    //     rest: Identifier | null;
    //     body: BlockStatement | Expression;
    //     generator: boolean;
    //     expression: boolean;
    // }
    type FunctionDecleration = Function;

    // interface VariableDeclaration <: Declaration {
    //     type: "VariableDeclaration";
    //     declarations: [ VariableDeclarator ];
    //     kind: "var" | "let" | "const";
    // }
    struct VariableDeclaration {
        declarations: Vec<VariableDeclarator>,
        kind: VariableDeclarationKind
    }

    enum VariableDeclarationKind {
        Var,
        Let,
        Const
    }

    // interface VariableDeclarator <: Node {
    //     type: "VariableDeclarator";
    //     id: Pattern;
    //     init: Expression | null;
    // }
    struct VariableDeclarator {
        id: Pattern,
        init: Option<Expression>,
    }

    // interface Expression <: Node, Pattern { }
    enum Expression {
        This,
        Array(Option<Vec<Expression>>),
        Object(Vec<PropertyExpression>),
        Property(PropertyExpression),
        Function(FunctionExpression),
        Arrow(ArrowExpression),
        Sequence(Vec<Expression>),
        Unary(UnaryExpression),
        Binary(BinaryExpression),
        Assignment(AssignmentExpression),
        Update(UpdateExpression),
        Logical(LogicalExpression),
        Conditional(ConditionalExpression),
        New(NewExpression),
        Call(CallExpression),
        Member(MemberExpression)
    }

    // interface Property <: Node {
    //     type: "Property";
    //     key: Literal | Identifier;
    //     value: Expression;
    //     kind: "init" | "get" | "set";
    // }
    struct PropertyExpression {
        // value: Expression
        kind: PropertyExpressionKind
    }

    enum PropertyExpressionKind {
        Init,
        Get,
        Set
    }

    // interface FunctionExpression <: Function, Expression {
    //     type: "FunctionExpression";
    //     id: Identifier | null;
    //     params: [ Pattern ];
    //     defaults: [ Expression ];
    //     rest: Identifier | null;
    //     body: BlockStatement | Expression;
    //     generator: boolean;
    //     expression: boolean;
    // }
    struct FunctionExpression {

    }

    // interface ArrowExpression <: Function, Expression {
    //     type: "ArrowExpression";
    //     params: [ Pattern ];
    //     defaults: [ Expression ];
    //     rest: Identifier | null;
    //     body: BlockStatement | Expression;
    //     generator: boolean;
    //     expression: boolean;
    // }
    struct ArrowExpression {

    }

    // interface SequenceExpression <: Expression {
    //     type: "SequenceExpression";
    //     expressions: [ Expression ];
    // }
    // type SequenceExpression = Vec<Expression>;

    // interface UnaryExpression <: Expression {
    //     type: "UnaryExpression";
    //     operator: UnaryOperator;
    //     prefix: boolean;
    //     argument: Expression;
    // }
    struct UnaryExpression {
        operator: UnaryOperator,
        prefix: bool,
        // argument: Expression,
    }

    // interface BinaryExpression <: Expression {
    //     type: "BinaryExpression";
    //     operator: BinaryOperator;
    //     left: Expression;
    //     right: Expression;
    // }
    struct BinaryExpression {

    }

    // interface AssignmentExpression <: Expression {
    //     type: "AssignmentExpression";
    //     operator: AssignmentOperator;
    //     left: Pattern;
    //     right: Expression;
    // }
    struct AssignmentExpression {

    }

    // interface UpdateExpression <: Expression {
    //     type: "UpdateExpression";
    //     operator: UpdateOperator;
    //     argument: Expression;
    //     prefix: boolean;
    // }
    struct UpdateExpression {

    }

    // interface LogicalExpression <: Expression {
    //     type: "LogicalExpression";
    //     operator: LogicalOperator;
    //     left: Expression;
    //     right: Expression;
    // }
    struct LogicalExpression {

    }

    // interface ConditionalExpression <: Expression {
    //     type: "ConditionalExpression";
    //     test: Expression;
    //     alternate: Expression;
    //     consequent: Expression;
    // }
    struct ConditionalExpression {

    }

    // interface NewExpression <: Expression {
    //     type: "NewExpression";
    //     callee: Expression;
    //     arguments: [ Expression ];
    // }
    struct NewExpression {

    }

    // interface CallExpression <: Expression {
    //     type: "CallExpression";
    //     callee: Expression;
    //     arguments: [ Expression ];
    // }
    struct CallExpression {

    }

    // interface MemberExpression <: Expression {
    //     type: "MemberExpression";
    //     object: Expression;
    //     property: Identifier | Expression;
    //     computed: boolean;
    // }
    struct MemberExpression {

    }


    // interface Pattern <: Node { }
    enum Pattern {
        Object(ObjectPattern),
        Array(ArrayPattern)
    }

    // interface ObjectPattern <: Pattern {
    //     type: "ObjectPattern";
    //     properties: [ { key: Literal | Identifier, value: Pattern } ];
    // }
    struct ObjectPattern {
        // properties: Vec<(, Pattern)>
    }

    // interface ArrayPattern <: Pattern {
    //     type: "ArrayPattern";
    //     elements: [ Pattern | null ];
    // }
    struct ArrayPattern {

    }


    enum Clause {

    }


    enum Literal {
        // TODO: Is it safe to use the String keyword here?
        String(String),
        Boolean(bool),
        Null,
        // TODO: Is this how JS represents numbers?
        Number(f64),
        Regex(String),
    }

    enum Operator {
        Unary(UnaryOperator),
        Bianry(BinaryOperator),
        Logical(LogicalOperator),
        Assignment(AssignmentOperator),
        Update(UpdateOperator)
    }

    // enum UnaryOperator {
    //     "-" | "+" | "!" | "~" | "typeof" | "void" | "delete"
    // }
    enum UnaryOperator {
        Minus,
        Plus,
        Not,
        Xor,
        Typeof,
        Void,
        Delete
    }

    // enum BinaryOperator {
    //     "==" | "!=" | "===" | "!=="
    //          | "<" | "<=" | ">" | ">="
    //          | "<<" | ">>" | ">>>"
    //          | "+" | "-" | "*" | "/" | "%"
    //          | "|" | "^" | "&" | "in"
    //          | "instanceof" | ".."
    // }
    enum BinaryOperator {

    }

    // enum LogicalOperator {
    //     "||" | "&&"
    // }
    enum LogicalOperator {
        Or,
        And,
    }

    // enum AssignmentOperator {
    //     "=" | "+=" | "-=" | "*=" | "/=" | "%="
    //         | "<<=" | ">>=" | ">>>="
    //         | "|=" | "^=" | "&="
    // }
    enum AssignmentOperator {

    }

    // enum UpdateOperator {
    //     "++" | "--"
    // }
    enum UpdateOperator {
        Inc,
        Dec,
    }

    // impl serde::Serialize for Statement {
    //     fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    //         where S: serde::Serializer,
    //     {
    //         serializer.serialize_str(v: &str)
    //     }
    // }


}

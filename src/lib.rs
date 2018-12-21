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


    // #[serde(tag = "type")]
    // #[derive(Serialize, Deserialize)]
    // enum Expr {
    //     #[serde(rename = "ThisExpression")]
    //     This
    // }
    //
    // #[test]
    // fn test_expr_de_se() {
    //     let e = Expr::This;
    //
    //     println!("Expr::This: {}", serde_json::to_string(&e).unwrap());
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
        // Block(BlockStatement),
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


    #[derive(Serialize, Deserialize)]
    #[serde(tag="type")]
    enum Stmt {
        #[serde(rename="EmptyStatement")]
        Empty,
        #[serde(rename="BlockStatement")]
        Block(BlockStmt),
        // Expression(Expression),
        // Decleration(Decleration),
        #[serde(rename="IfStatement")]
        If(IfStmt),
        #[serde(rename="LabeledStatement")]
        Labled(LabledStmt),
        #[serde(rename="BreakStatement")]
        Break(BreakStmt),
        #[serde(rename="ContinueStatement")]
        Continue(ContinueStmt),
        #[serde(rename="WithStatement")]
        With(WithStmt),
        #[serde(rename="SwitchStatement")]
        Switch(SwitchStmt),
        #[serde(rename="ReturnStatement")]
        Return(ReturnStmt),
        #[serde(rename="ThrowStatement")]
        Throw(ThrowStmt),
        #[serde(rename="TryStatement")]
        Try(TryStmt),
        #[serde(rename="WhileStatement")]
        While(WhileStmt),
        #[serde(rename="DoWhileStatement")]
        DoWhile(DoWhileStmt),
        #[serde(rename="ForStatement")]
        For(ForStmt),
        #[serde(rename="ForInStatement")]
        ForIn(ForInStmt),
        #[serde(rename="ForOfStatement")]
        ForOf(ForOfStatement),
        #[serde(rename="LetStatement")]
        Let(LetStmt),
        #[serde(rename="DebuggerStatement")]
        Debugger(DebuggerStmt),
    }

    #[cfg(test)]
    fn check_se<T>(t: T, json: serde_json::Value) where T: serde::Serialize{ // + serde::Deserialize
        assert_eq!(serde_json::to_value(&t).unwrap(), json);
        // assert_eq!(t, serde_json::from_value::<T>(json).unwrap());
    }

    #[test]
    fn test_statement_se_de() {
        check_se(Stmt::Empty, json!({"type": "EmptyStatement"}));
        check_se(Stmt::Block(BlockStmt{body:vec![]}),
                 json!({"type": "BlockStatement", "body": []}));
        assert_eq!(serde_json::to_value(&Stmt::If(IfStmt{// test: Expr(),
                                                         consequent: Box::new(Stmt::Empty),
                                                         alternate: Some(Box::new(Stmt::Empty))})).unwrap(),
                    json!({"type": "IfStatement",
                            "consequent": {"type": "EmptyStatement"},
                            "alternate": {"type": "EmptyStatement"}}));
        assert_eq!(serde_json::to_value(&Stmt::If(IfStmt{// test: Expr(),
                                                         consequent: Box::new(Stmt::Empty),
                                                         alternate: None})).unwrap(),
                    json!({"type": "IfStatement",
                            "consequent": {"type": "EmptyStatement"},
                            "alternate": serde_json::Value::Null}));
        assert_eq!(serde_json::to_value(&Stmt::Labled(LabledStmt{label: "lbl".to_string(),
                                                         body: Box::new(Stmt::Empty)})).unwrap(),
                    json!({"type": "LabeledStatement",
                            "label": "lbl",
                            "body": {"type": "EmptyStatement"}}));
        assert_eq!(serde_json::to_value(&Stmt::Break(BreakStmt{label: None})).unwrap(),
                    json!({"type": "BreakStatement", "label": serde_json::Value::Null}));
        assert_eq!(serde_json::to_value(&Stmt::Continue(ContinueStmt{label: Some("lbl".to_string())})).unwrap(),
                    json!({"type": "ContinueStatement", "label": "lbl"}));

        // assert_eq!(serde_json::to_value(&Stmt::With(BreakStmt{label: None})).unwrap(),
        //             json!({"type": "BreakStatement", "label": serde_json::Value::Null}));

        assert_eq!(serde_json::to_value(&Stmt::Switch(SwitchStmt{cases: vec![], lexical: false})).unwrap(),
                    json!({"type": "SwitchStatement", "cases": [],
                           "lexical": false}));

        // assert_eq!(serde_json::to_value(&Stmt::Switch(SwitchStmt{cases: vec![], lexical: false})).unwrap(),
        //            json!({"type": "ReturnStatement", "cases": [],
        //                   "lexical": false}));

        // assert_eq!(serde_json::to_value(&Stmt::Throw(ThrowStmt{cases: vec![], lexical: false})).unwrap(),
        //            json!({"type": "ReturnStatement", "cases": [],
        //                   "lexical": false}));

        assert_eq!(serde_json::to_value(&Stmt::Try(TryStmt{
                                                    block: BlockStmt{body: vec![]},
                                                    handler: None,
                                                    guarded_handlers: vec![],
                                                    finalizer: Some(BlockStmt{body: vec![]})})).unwrap(),
                   json!({"type": "TryStatement",
                          "block": {"body": []},
                          "handler": serde_json::Value::Null,
                          "guardedHandlers": [],
                          "finalizer": {"body": []}}));

        assert_eq!(serde_json::to_value(&Stmt::While(WhileStmt{body: Box::new(Stmt::Empty)})).unwrap(),
                    json!({"type": "WhileStatement", "body": {"type": "EmptyStatement"}}));

        assert_eq!(serde_json::to_value(&Stmt::For(ForStmt{init: None, body: Box::new(Stmt::Empty)})).unwrap(),
                    json!({"type": "ForStatement", "init": serde_json::Value::Null, "body": {"type": "EmptyStatement"}}));

        // assert_eq!(serde_json::to_value(&Stmt::ForIn(ForInStmt{left: ForStmtInit::Decl(VariableDeclaration{declarations: vec![],
        //                                                                                                    kind: VariableDeclarationKind::Let}),
        //                                                        body: Box::new(Stmt::Empty),
        //                                                        each: false})).unwrap(),
        //             json!({"type": "ForInStatement",
        //                     "left": {"declerations": [], "kind": "let"},
        //                     "body": {"type": "EmptyStatement"},
        //                     "each": false}));

        // TODO: ForOfStatement


        assert_eq!(serde_json::to_value(&Stmt::Let(LetStmt{head: vec![], body: Box::new(Stmt::Empty)})).unwrap(),
                    json!({"type": "LetStatement", "head": [], "body": {"type": "EmptyStatement"}}));
    }

    // interface BlockStatement <: Statement {
    //     type: "BlockStatement";
    //     body: [ Statement ];
    // }
    #[derive(Serialize, Deserialize)]
    struct BlockStmt {
        body: Vec<Stmt>
    }

    // interface IfStatement <: Statement {
    //     type: "IfStatement";
    //     test: Expression;
    //     consequent: Statement;
    //     alternate: Statement | null;
    // }
    #[derive(Serialize, Deserialize)]
    struct IfStmt {
        // test: Expr, // TODO: Expr
        consequent: Box<Stmt>,
        alternate: Option<Box<Stmt>>
    }

    // interface LabeledStatement <: Statement {
    //     type: "LabeledStatement";
    //     label: Identifier;
    //     body: Statement;
    // }
    #[derive(Serialize, Deserialize)]
    struct LabledStmt {
        label: Identifier,
        body: Box<Stmt>,
    }

    // interface BreakStatement <: Statement {
    //     type: "BreakStatement";
    //     label: Identifier | null;
    // }
    #[derive(Serialize, Deserialize)]
    struct BreakStmt {
        label: Option<Identifier>,
    }

    // interface ContinueStatement <: Statement {
    //     type: "ContinueStatement";
    //     label: Identifier | null;
    // }
    #[derive(Serialize, Deserialize)]
    struct ContinueStmt {
        label: Option<Identifier>
    }

    // interface WithStatement <: Statement {
    //     type: "WithStatement";
    //     object: Expression;
    //     body: Statement;
    // }
    #[derive(Serialize, Deserialize)]
    struct WithStmt {
        // object: Expr, //TODO: Expr
        // body: Expr,
    }

    // interface SwitchStatement <: Statement {
    //     type: "SwitchStatement";
    //     discriminant: Expression;
    //     cases: [ SwitchCase ];
    //     lexical: boolean;
    // }
    #[derive(Serialize, Deserialize)]
    struct SwitchStmt {
        // discriminant: Expr, //TODO: Expr
        cases: Vec<SwitchCase>,
        lexical: bool
    }

    // interface ReturnStatement <: Statement {
    //     type: "ReturnStatement";
    //     argument: Expression | null;
    // }
    #[derive(Serialize, Deserialize)]
    struct ReturnStmt {
        // argument: Option<Expr> //TODO: Expr
    }

    // interface ThrowStatement <: Statement {
    //     type: "ThrowStatement";
    //     argument: Expression;
    // }
    #[derive(Serialize, Deserialize)]
    struct ThrowStmt {
        // argument: Expr, //TODO: Expr
    }

    // interface TryStatement <: Statement {
    //     type: "TryStatement";
    //     block: BlockStatement;
    //     handler: CatchClause | null;
    //     guardedHandlers: [ CatchClause ];
    //     finalizer: BlockStatement | null;
    // }
    #[derive(Serialize, Deserialize)]
    struct TryStmt {
        block: BlockStmt,
        handler: Option<CatchClause>,
        #[serde(rename="guardedHandlers")]
        guarded_handlers: Vec<CatchClause>,
        finalizer: Option<BlockStmt>
    }

    // interface WhileStatement <: Statement {
    //     type: "WhileStatement";
    //     test: Expression;
    //     body: Statement;
    // }
    #[derive(Serialize, Deserialize)]
    struct WhileStmt {
        // test: Expr, TODO: Expr
        body: Box<Stmt>
    }

    // interface DoWhileStatement <: Statement {
    //     type: "DoWhileStatement";
    //     body: Statement;
    //     test: Expression;
    // }
    #[derive(Serialize, Deserialize)]
    struct DoWhileStmt {
        body: Box<Stmt>,
        // test: Expr, // TODO: Expr
    }

    // interface ForStatement <: Statement {
    //     type: "ForStatement";
    //     init: VariableDeclaration | Expression | null;
    //     test: Expression | null;
    //     update: Expression | null;
    //     body: Statement;
    // }
    #[derive(Serialize, Deserialize)]
    struct ForStmt {
        init: Option<ForStmtInit>,
        // test: Option<Expr>, TODO: Expr
        // update: Option<Expr>, TODO: Expr
        body: Box<Stmt>
    }

    // interface ForInStatement <: Statement {
    //     type: "ForInStatement";
    //     left: VariableDeclaration |  Expression;
    //     right: Expression;
    //     body: Statement;
    //     each: boolean;
    // }
    #[derive(Serialize, Deserialize)]
    struct ForInStmt {
        left: ForStmtInit,
        // right: Expr, TODO: Expr
        body: Box<Stmt>,
        each: bool
    }

    // interface ForOfStatement <: Statement {
    //     type: "ForOfStatement";
    //     left: VariableDeclaration |  Expression;
    //     right: Expression;
    //     body: Statement;
    // }
    #[derive(Serialize, Deserialize)]
    struct ForOfStatement {
        left: ForStmtInit,
        // right: Expr, TODO: Expr
        body: Box<Stmt>
    }

    #[derive(Serialize, Deserialize)]
    #[serde(untagged)]
    enum ForStmtInit {
        Decl(VariableDeclaration),
        // Expr(Expr), TODO: Expr
    }

    // interface LetStatement <: Statement {
    //     type: "LetStatement";
    //     head: [ VariableDeclarator ];
    //     body: Statement;
    // }
    #[derive(Serialize, Deserialize)]
    struct LetStmt {
        head: Vec<VariableDeclarator>,
        body: Box<Stmt>
    }

    // interface DebuggerStatement <: Statement {
    //     type: "DebuggerStatement";
    // }
    #[derive(Serialize, Deserialize)]
    struct DebuggerStmt {
    }


    // interface Declaration <: Statement { }
    // #[derive(Serialize, Deserialize)]
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
    #[derive(Serialize, Deserialize)]
    struct VariableDeclaration {
        declarations: Vec<VariableDeclarator>,
        kind: VariableDeclarationKind
    }

    #[derive(Serialize, Deserialize)]
    enum VariableDeclarationKind {
        #[serde(rename="var")]
        Var,
        #[serde(rename="let")]
        Let,
        #[serde(rename="const")]
        Const
    }

    #[test]
    fn test_vardec_se_de() {
        // assert_eq!(serde_json::to_value(&VariableDeclaration{declarations: vec![], kind: VariableDeclarationKind::Let}).unwrap(),
        //           json!({
        //               "declerations": [],
        //               "kind": "let"
        //           }));

        // assert_eq!(serde_json::to_value(&VariableDeclaration{declarations: vec![], kind: VariableDeclarationKind::Var}).unwrap(),
        //             json!({
        //                 "declerations": [],
        //                 "kind": "var".to_string()
        //             }));
    }

    // interface VariableDeclarator <: Node {
    //     type: "VariableDeclarator";
    //     id: Pattern;
    //     init: Expression | null;
    // }
    #[derive(Serialize, Deserialize)]
    struct VariableDeclarator {
        // id: Pattern, TODO: Pattern
        // init: Option<Expr>, TODO: Expr
    }

    // TODO: Remove
    enum Expression {

    }

    // interface Expression <: Node, Pattern { }
    #[derive(Serialize, Deserialize)]
    #[serde(tag="type")]
    enum Expr {
        #[serde(rename="ThisExpression")]
        This,
        // #[serde(rename="ArrayExpression")]
        // Array(ArrayExpr),
        #[serde(rename="ObjectExpression")]
        Object(ObjectExpr),
        // Property(PropertyExpression),
        // Function(FunctionExpression),
        // Arrow(ArrowExpression),
        // Sequence(Vec<Expression>),
        // Unary(UnaryExpression),
        // Binary(BinaryExpression),
        // Assignment(AssignmentExpression),
        // Update(UpdateExpression),
        // Logical(LogicalExpression),
        // Conditional(ConditionalExpression),
        // New(NewExpression),
        // Call(CallExpression),
        // Member(MemberExpression)
    }

    #[test]
    fn test_expr_se_de() {
        assert_eq!(serde_json::to_value(&Expr::This).unwrap(), json!({"type": "ThisExpression"}));
    }

    // interface ArrayExpression <: Expression {
    //     type: "ArrayExpression";
    //     elements: [ Expression | null ];
    // }
    #[derive(Serialize, Deserialize)]
    struct ArrayExpr {
        elements: Option<Vec<Expr>>
    }

    // interface ObjectExpression <: Expression {
    //     type: "ObjectExpression";
    //     properties: [ Property ];
    // }
    #[derive(Serialize, Deserialize)]
    struct ObjectExpr {
        // properties: Vec<Property>
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
        operator: UnaryOp,
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


    #[derive(Serialize, Deserialize)]
    #[serde(tag="type")]
    enum Clause {
        #[serde(rename="SwitchCase")]
        Swtich(SwitchCase),
        #[serde(rename="CatchClause")]
        Catch(CatchClause)
    }

    // interface SwitchCase <: Node {
    //     type: "SwitchCase";
    //     test: Expression | null;
    //     consequent: [ Statement ];
    // }
    #[derive(Serialize, Deserialize)]
    struct SwitchCase {
        // test: Option<Expr>
        consequent: Vec<Stmt>
    }

    // interface CatchClause <: Node {
    //     type: "CatchClause";
    //     param: Pattern;
    //     guard: Expression | null;
    //     body: BlockStatement;
    // }
    #[derive(Serialize, Deserialize)]
    struct CatchClause {
        // param: Pattern, // TODO: Pattern
        // buard: Option<Expr>, // TODO: Expr
        body: BlockStmt
    }


    // #[derive(Serialize, Deserialize)]
    // #[serde(tag="type")]
    enum Literal {
        Str(String),
        Bool(bool),
        Null,
        // TODO: Is this how JS represents numbers?
        Num(f64),
        Regex(String),
    }


    #[derive(Serialize, Deserialize)]
    #[serde(untagged)]
    enum Operator {
        Unary(UnaryOp),
        // Bianry(BinaryOperator),
        Logical(LogicalOp),
        // Assignment(AssignmentOperator),
        Update(UpdateOp)
    }

    #[test]
    fn test_operator_se_de() {
        assert_eq!(serde_json::to_value(&Operator::Unary(UnaryOp::Plus)).unwrap(),
                    json!({"operator": "+"}));
        assert_eq!(serde_json::to_value(&Operator::Logical(LogicalOp::Or)).unwrap(),
                    json!({"operator": "||"}));
        assert_eq!(serde_json::to_value(&Operator::Update(UpdateOp::Inc)).unwrap(),
                    json!({"operator": "++"}));
    }


    // enum UnaryOperator {
    //     "-" | "+" | "!" | "~" | "typeof" | "void" | "delete"
    // }
    #[derive(Serialize, Deserialize)]
    #[serde(tag="operator")]
    enum UnaryOp {
        #[serde(rename="-")]
        Minus,
        #[serde(rename="+")]
        Plus,
        #[serde(rename="!")]
        Not,
        #[serde(rename="~")]
        Xor,
        #[serde(rename="typeof")]
        Typeof,
        #[serde(rename="void")]
        Void,
        #[serde(rename="delete")]
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
    #[derive(Serialize, Deserialize)]
    #[serde(tag="operator")]
    enum LogicalOp {
        #[serde(rename="||")]
        Or,
        #[serde(rename="&&")]
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
    #[derive(Serialize, Deserialize)]
    #[serde(tag="operator")]
    enum UpdateOp {
        #[serde(rename="++")]
        Inc,
        #[serde(rename="++")]
        Dec
    }
}

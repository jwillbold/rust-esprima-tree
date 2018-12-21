#[macro_use]
extern crate serde_derive;
extern crate serde;
#[macro_use]
extern crate serde_json;
extern crate serde_test;

// Based on https://developer.mozilla.org/en-US/docs/Mozilla/Projects/SpiderMonkey/Parser_API
pub mod jsast {

    #![allow(dead_code)]
    
    type Identifier = String;
    type Label = String;

    // interface Node {
    //     type: string;
    //     loc: SourceLocation | null;
    // }


    // interface Program <: Node {
    //     type: "Program";
    //     body: [ Statement ];
    // }
    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    struct Program {
        // type: &'static str = "Program",
        body: String
        // body: Vec<Statement>,
    }


    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    #[serde(tag="type")]
    enum BlockStatementOrExpression {
        #[serde(rename="BlockStatement")]
        Block(BlockStmt),
        #[serde(rename="Expression")]
        Expr(Box<Expr>)
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
        defaults: Vec<Expr>,
        rest: Option<Identifier>,
        body: BlockStatementOrExpression,
        generator: bool,
        expression: bool
    }


    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    #[serde(tag="type")]
    enum Stmt {
        #[serde(rename="EmptyStatement")]
        Empty,
        #[serde(rename="BlockStatement")]
        Block(BlockStmt),
        // Expression(Expr),
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
        ForOf(ForOfStmt),
        #[serde(rename="LetStatement")]
        Let(LetStmt),
        #[serde(rename="DebuggerStatement")]
        Debugger(DebuggerStmt),
    }

    #[cfg(test)]
    fn check_se_de<T>(t: T, json: serde_json::Value) where for<'de> T: serde::Serialize +
                                                                       serde::Deserialize<'de> +
                                                                       std::fmt::Debug +
                                                                       std::cmp::PartialEq{
        assert_eq!(serde_json::to_value(&t).unwrap(), json);
        assert_eq!(t, serde_json::from_value::<T>(json).unwrap());
    }

    #[test]
    fn test_statement_se_de() {
        check_se_de(Stmt::Empty, json!({"type": "EmptyStatement"}));

        check_se_de(Stmt::Block(BlockStmt{body:vec![]}),
                    json!({"type": "BlockStatement", "body": []}));

        check_se_de(Stmt::If(IfStmt{test: Expr::This,
                                    consequent: Box::new(Stmt::Empty),
                                    alternate: Some(Box::new(Stmt::Empty))}),
                    json!({"type": "IfStatement",
                            "test": {"type": "ThisExpression"},
                            "consequent": {"type": "EmptyStatement"},
                            "alternate": {"type": "EmptyStatement"}}));

        check_se_de(Stmt::If(IfStmt{test: Expr::This,
                                    consequent: Box::new(Stmt::Empty),
                                    alternate: None}),
                    json!({"type": "IfStatement",
                            "test": {"type": "ThisExpression"},
                            "consequent": {"type": "EmptyStatement"},
                            "alternate": serde_json::Value::Null}));
        check_se_de(Stmt::Labled(LabledStmt{label: "lbl".to_string(),
                                            body: Box::new(Stmt::Empty)}),
                    json!({"type": "LabeledStatement",
                            "label": "lbl",
                            "body": {"type": "EmptyStatement"}}));
        check_se_de(Stmt::Break(BreakStmt{label: None}),
                    json!({"type": "BreakStatement", "label": serde_json::Value::Null}));

        check_se_de(Stmt::Continue(ContinueStmt{label: Some("lbl".to_string())}),
                    json!({"type": "ContinueStatement", "label": "lbl"}));

        check_se_de(Stmt::With(WithStmt{object: Expr::This, body: Expr::This}),
                    json!({"type": "WithStatement",
                            "object": {"type": "ThisExpression"},
                            "body": {"type": "ThisExpression"}}));

        check_se_de(Stmt::Switch(SwitchStmt{discriminant: Expr::This, cases: vec![], lexical: false}),
                    json!({"type": "SwitchStatement",
                            "discriminant": {"type": "ThisExpression"},
                            "cases": [],
                            "lexical": false}));

        check_se_de(Stmt::Return(ReturnStmt{argument: None}),
                    json!({"type": "ReturnStatement", "argument": serde_json::Value::Null}));

        check_se_de(Stmt::Throw(ThrowStmt{argument: Expr::This}),
                    json!({"type": "ThrowStatement", "argument": Expr::This}));

        check_se_de(Stmt::Try(TryStmt{block: BlockStmt{body: vec![]},
                                      handler: None,
                                      guarded_handlers: vec![],
                                      finalizer: Some(BlockStmt{body: vec![]})}),
                   json!({"type": "TryStatement",
                          "block": {"body": []},
                          "handler": serde_json::Value::Null,
                          "guardedHandlers": [],
                          "finalizer": {"body": []}}));

        check_se_de(Stmt::While(WhileStmt{test: Expr::This, body: Box::new(Stmt::Empty)}),
                    json!({"type": "WhileStatement",
                            "test": {"type": "ThisExpression"},
                            "body": {"type": "EmptyStatement"}}));
        check_se_de(Stmt::DoWhile(DoWhileStmt{body: Box::new(Stmt::Empty), test: Expr::This}),
                    json!({"type": "DoWhileStatement",
                            "body": {"type": "EmptyStatement"},
                            "test": {"type": "ThisExpression"}}));

        check_se_de(Stmt::For(ForStmt{init: None, test: None, update: None, body: Box::new(Stmt::Empty)}),
                    json!({"type": "ForStatement",
                           "init": serde_json::Value::Null,
                           "test": serde_json::Value::Null,
                           "update": serde_json::Value::Null,
                           "body": {"type": "EmptyStatement"}}));

        check_se_de(Stmt::ForIn(ForInStmt{left: ForStmtInit::Decl(VariableDeclaration{
                                                                    declarations: vec![],
                                                                    kind: VariableDeclarationKind::Let}),
                                           right: Expr::This,
                                           body: Box::new(Stmt::Empty),
                                           each: false}),
                    json!({"type": "ForInStatement",
                            "left": {"declarations": [], "kind": "let"},
                            "right": {"type": "ThisExpression"},
                            "body": {"type": "EmptyStatement"},
                            "each": false}));

        check_se_de(Stmt::ForOf(ForOfStmt{left: ForStmtInit::Expr(Expr::This),
                                           right: Expr::This,
                                           body: Box::new(Stmt::Empty)}),
                    json!({"type": "ForOfStatement",
                            "left": {"type": "ThisExpression"},
                            "right": {"type": "ThisExpression"},
                            "body": {"type": "EmptyStatement"}}));

        check_se_de(Stmt::Let(LetStmt{head: vec![], body: Box::new(Stmt::Empty)}),
                    json!({"type": "LetStatement",
                           "head": [],
                           "body": {"type": "EmptyStatement"}}));
    }

    // interface BlockStatement <: Statement {
    //     type: "BlockStatement";
    //     body: [ Statement ];
    // }
    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    struct BlockStmt {
        body: Vec<Stmt>
    }

    // interface IfStatement <: Statement {
    //     type: "IfStatement";
    //     test: Expression;
    //     consequent: Statement;
    //     alternate: Statement | null;
    // }
    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    struct IfStmt {
        test: Expr,
        consequent: Box<Stmt>,
        alternate: Option<Box<Stmt>>
    }

    // interface LabeledStatement <: Statement {
    //     type: "LabeledStatement";
    //     label: Identifier;
    //     body: Statement;
    // }
    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    struct LabledStmt {
        label: Identifier,
        body: Box<Stmt>,
    }

    // interface BreakStatement <: Statement {
    //     type: "BreakStatement";
    //     label: Identifier | null;
    // }
    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    struct BreakStmt {
        label: Option<Identifier>,
    }

    // interface ContinueStatement <: Statement {
    //     type: "ContinueStatement";
    //     label: Identifier | null;
    // }
    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    struct ContinueStmt {
        label: Option<Identifier>
    }

    // interface WithStatement <: Statement {
    //     type: "WithStatement";
    //     object: Expression;
    //     body: Statement;
    // }
    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    struct WithStmt {
        object: Expr,
        body: Expr,
    }

    // interface SwitchStatement <: Statement {
    //     type: "SwitchStatement";
    //     discriminant: Expression;
    //     cases: [ SwitchCase ];
    //     lexical: boolean;
    // }
    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    struct SwitchStmt {
        discriminant: Expr,
        cases: Vec<SwitchCase>,
        lexical: bool
    }

    // interface ReturnStatement <: Statement {
    //     type: "ReturnStatement";
    //     argument: Expression | null;
    // }
    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    struct ReturnStmt {
        argument: Option<Expr>
    }

    // interface ThrowStatement <: Statement {
    //     type: "ThrowStatement";
    //     argument: Expression;
    // }
    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    struct ThrowStmt {
        argument: Expr
    }

    // interface TryStatement <: Statement {
    //     type: "TryStatement";
    //     block: BlockStatement;
    //     handler: CatchClause | null;
    //     guardedHandlers: [ CatchClause ];
    //     finalizer: BlockStatement | null;
    // }
    #[derive(Serialize, Deserialize, PartialEq, Debug)]
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
    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    struct WhileStmt {
        test: Expr,
        body: Box<Stmt>
    }

    // interface DoWhileStatement <: Statement {
    //     type: "DoWhileStatement";
    //     body: Statement;
    //     test: Expression;
    // }
    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    struct DoWhileStmt {
        body: Box<Stmt>,
        test: Expr,
    }

    // interface ForStatement <: Statement {
    //     type: "ForStatement";
    //     init: VariableDeclaration | Expression | null;
    //     test: Expression | null;
    //     update: Expression | null;
    //     body: Statement;
    // }
    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    struct ForStmt {
        init: Option<ForStmtInit>,
        test: Option<Expr>,
        update: Option<Expr>,
        body: Box<Stmt>
    }

    // interface ForInStatement <: Statement {
    //     type: "ForInStatement";
    //     left: VariableDeclaration |  Expression;
    //     right: Expression;
    //     body: Statement;
    //     each: boolean;
    // }
    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    struct ForInStmt {
        left: ForStmtInit,
        right: Expr,
        body: Box<Stmt>,
        each: bool
    }

    // interface ForOfStatement <: Statement {
    //     type: "ForOfStatement";
    //     left: VariableDeclaration |  Expression;
    //     right: Expression;
    //     body: Statement;
    // }
    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    struct ForOfStmt {
        left: ForStmtInit,
        right: Expr,
        body: Box<Stmt>
    }

    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    #[serde(untagged)]
    enum ForStmtInit {
        Decl(VariableDeclaration),
        Expr(Expr),
    }

    // interface LetStatement <: Statement {
    //     type: "LetStatement";
    //     head: [ VariableDeclarator ];
    //     body: Statement;
    // }
    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    struct LetStmt {
        head: Vec<VariableDeclarator>,
        body: Box<Stmt>
    }

    // interface DebuggerStatement <: Statement {
    //     type: "DebuggerStatement";
    // }
    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    struct DebuggerStmt {
    }


    // interface Declaration <: Statement { }
    // #[derive(Serialize, Deserialize, PartialEq, Debug)]
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
    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    struct VariableDeclaration {
        declarations: Vec<VariableDeclarator>,
        kind: VariableDeclarationKind
    }

    #[derive(Serialize, Deserialize, PartialEq, Debug)]
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
        check_se_de(VariableDeclaration{declarations: vec![], kind: VariableDeclarationKind::Let},
                    json!({"declarations": [], "kind": "let"}));

        check_se_de(VariableDeclaration{declarations: vec![], kind: VariableDeclarationKind::Var},
                    json!({"declarations": [], "kind": "var".to_string()}));
    }

    // interface VariableDeclarator <: Node {
    //     type: "VariableDeclarator";
    //     id: Pattern;
    //     init: Expression | null;
    // }
    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    struct VariableDeclarator {
        id: Pattern,
        init: Option<Expr>
    }

    // interface Expression <: Node, Pattern { }
    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    #[serde(tag="type")]
    enum Expr {
        #[serde(rename="ThisExpression")]
        This,
        #[serde(rename="ArrayExpression")]
        Array(ArrayExpr),
        #[serde(rename="ObjectExpression")]
        Object(ObjectExpr),
        #[serde(rename="FunctionExpression")]
        Function(FunctionExpr),
        #[serde(rename="ArrowExpression")]
        Arrow(ArrowExpr),
        #[serde(rename="SeqeunceExpression")]
        Sequence(SequenceExpr),
        #[serde(rename="UnaryExpression")]
        Unary(UnaryExpr),
        #[serde(rename="BinaryExpression")]
        Binary(BinaryExpr),
        #[serde(rename="AssignmentExpression")]
        Assignment(AssignmentExpr),
        #[serde(rename="UpdateExpression")]
        Update(UpdateExpr),
        #[serde(rename="LogicalExpression")]
        Logical(LogicalExpr),
        #[serde(rename="ConditionalExpression")]
        Conditional(ConditionalExpr),
        #[serde(rename="NewExpression")]
        New(NewExpr),
        #[serde(rename="CallExpression")]
        Call(CallExpr),
        #[serde(rename="MemberExpression")]
        Member(MemberExpr)
    }

    #[test]
    fn test_expr_se_de() {
        check_se_de(Expr::This, json!({"type": "ThisExpression"}));
        check_se_de(Expr::Array(ArrayExpr{elements: vec![]}),
                    json!({"type": "ArrayExpression", "elements": []}));
        check_se_de(Expr::Object(ObjectExpr{properties: vec![]}),
                    json!({"type": "ObjectExpression", "properties": []}));

        check_se_de(Expr::Function(FunctionExpr{id: None,
                                                params: vec![],
                                                defaults: vec![],
                                                rest: None,
                                                body: BlockStatementOrExpression::Block(BlockStmt{body: vec![]}),
                                                generator: false,
                                                expression: false}),
                    json!({"type": "FunctionExpression",
                            "id": serde_json::Value::Null,
                            "params": [],
                            "defaults": [],
                            "rest": serde_json::Value::Null,
                            "body": {"type": "BlockStatement", "body": []},
                            "generator": false,
                            "expression": false}));

        check_se_de(Expr::Sequence(SequenceExpr{expressions: vec![]}),
                    json!({"type": "SeqeunceExpression", "expressions": []}));
        check_se_de(Expr::Unary(UnaryExpr{operator: UnaryOp::Plus,
                                          prefix: false,
                                          argument: Box::new(Expr::This)}),
                    json!({"type": "UnaryExpression",
                            "operator": "+",
                            "prefix": false,
                            "argument": {"type": "ThisExpression"}}));
        check_se_de(Expr::Binary(BinaryExpr{operator: BinaryOp::Eq,
                                            left: Box::new(Expr::This),
                                            right: Box::new(Expr::This)}),
                    json!({"type": "BinaryExpression",
                            "operator": "==",
                            "left": {"type": "ThisExpression"},
                            "right": {"type": "ThisExpression"}}));
        check_se_de(Expr::Assignment(AssignmentExpr{operator: AssignmentOp::Assign,
                                            left: Pattern::Array(ArrayPattern{elements: vec![]}),
                                            right: Box::new(Expr::This)}),
                    json!({"type": "AssignmentExpression",
                            "operator": "=",
                            "left": {"type": "ArrayPattern", "elements": []},
                            "right": {"type": "ThisExpression"}}));
        check_se_de(Expr::Update(UpdateExpr{operator: UpdateOp::Inc,
                                            argument: Box::new(Expr::This),
                                            prefix: false}),
                    json!({"type": "UpdateExpression",
                            "operator": "++",
                            "argument": {"type": "ThisExpression"},
                            "prefix": false}));
        check_se_de(Expr::Logical(LogicalExpr{operator: LogicalOp::And,
                                              left: Box::new(Expr::This),
                                              right: Box::new(Expr::This)}),
                    json!({"type": "LogicalExpression",
                            "operator": "&&",
                            "left": {"type": "ThisExpression"},
                            "right": {"type": "ThisExpression"}}));
        check_se_de(Expr::Conditional(ConditionalExpr{test: Box::new(Expr::This),
                                              alternate: Box::new(Expr::This),
                                              consequent: Box::new(Expr::This)}),
                    json!({"type": "ConditionalExpression",
                            "test": {"type": "ThisExpression"},
                            "alternate": {"type": "ThisExpression"},
                            "consequent": {"type": "ThisExpression"}}));
        check_se_de(Expr::New(NewExpr{callee: Box::new(Expr::This),
                                      arguments: vec![]}),
                    json!({"type": "NewExpression",
                            "callee": {"type": "ThisExpression"},
                            "arguments": []}));
        check_se_de(Expr::Call(CallExpr{callee: Box::new(Expr::This),
                                       arguments: vec![]}),
                    json!({"type": "CallExpression",
                            "callee": {"type": "ThisExpression"},
                            "arguments": []}));
        check_se_de(Expr::Member(MemberExpr{object: Box::new(Expr::This),
                                            property: MemberExprProp::Ident("id".to_string()),
                                            computed: true}),
                    json!({"type": "MemberExpression",
                            "object": {"type": "ThisExpression"},
                            "property": "id",
                            "computed": true}));
    }

    // interface ArrayExpression <: Expression {
    //     type: "ArrayExpression";
    //     elements: [ Expression | null ];
    // }
    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    struct ArrayExpr {
        elements: Vec<Option<Expr>>
    }

    // interface ObjectExpression <: Expression {
    //     type: "ObjectExpression";
    //     properties: [ Property ];
    // }
    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    struct ObjectExpr {
        properties: Vec<Property>
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
    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    struct FunctionExpr {
        id: Option<Identifier>,
        params: Vec<Pattern>,
        defaults: Vec<Expr>,
        rest: Option<Identifier>,
        body: BlockStatementOrExpression,
        generator: bool,
        expression: bool
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
    type ArrowExpr = FunctionExpr;

    // interface SequenceExpression <: Expression {
    //     type: "SequenceExpression";
    //     expressions: [ Expression ];
    // }
    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    struct SequenceExpr {
        expressions: Vec<Expr>
    }

    // interface UnaryExpression <: Expression {
    //     type: "UnaryExpression";
    //     operator: UnaryOperator;
    //     prefix: boolean;
    //     argument: Expression;
    // }
    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    struct UnaryExpr {
        operator: UnaryOp,
        prefix: bool,
        argument: Box<Expr>,
    }

    // interface BinaryExpression <: Expression {
    //     type: "BinaryExpression";
    //     operator: BinaryOperator;
    //     left: Expression;
    //     right: Expression;
    // }
    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    struct BinaryExpr {
        operator: BinaryOp,
        left: Box<Expr>,
        right: Box<Expr>
    }

    // interface AssignmentExpression <: Expression {
    //     type: "AssignmentExpression";
    //     operator: AssignmentOperator;
    //     left: Pattern;
    //     right: Expression;
    // }
    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    struct AssignmentExpr {
        operator: AssignmentOp,
        left: Pattern,
        right: Box<Expr>
    }

    // interface UpdateExpression <: Expression {
    //     type: "UpdateExpression";
    //     operator: UpdateOperator;
    //     argument: Expression;
    //     prefix: boolean;
    // }
    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    struct UpdateExpr {
        operator: UpdateOp,
        argument: Box<Expr>,
        prefix: bool
    }

    // interface LogicalExpression <: Expression {
    //     type: "LogicalExpression";
    //     operator: LogicalOperator;
    //     left: Expression;
    //     right: Expression;
    // }
    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    struct LogicalExpr {
        operator: LogicalOp,
        left: Box<Expr>,
        right: Box<Expr>
    }

    // interface ConditionalExpression <: Expression {
    //     type: "ConditionalExpression";
    //     test: Expression;
    //     alternate: Expression;
    //     consequent: Expression;
    // }
    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    struct ConditionalExpr {
        test: Box<Expr>,
        alternate: Box<Expr>,
        consequent: Box<Expr>,
    }

    // interface NewExpression <: Expression {
    //     type: "NewExpression";
    //     callee: Expression;
    //     arguments: [ Expression ];
    // }
    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    struct NewExpr {
        callee: Box<Expr>,
        arguments: Vec<Expr>
    }

    // interface CallExpression <: Expression {
    //     type: "CallExpression";
    //     callee: Expression;
    //     arguments: [ Expression ];
    // }
    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    struct CallExpr {
        callee: Box<Expr>,
        arguments: Vec<Expr>
    }

    // interface MemberExpression <: Expression {
    //     type: "MemberExpression";
    //     object: Expression;
    //     property: Identifier | Expression;
    //     computed: boolean;
    // }
    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    struct MemberExpr {
        object: Box<Expr>,
        property: MemberExprProp,
        computed: bool
    }

    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    #[serde(untagged)]
    enum MemberExprProp {
        Ident(Identifier),
        Expr(Box<Expr>),
    }


    // interface Property <: Node {
    //     type: "Property";
    //     key: Literal | Identifier;
    //     value: Expression;
    //     kind: "init" | "get" | "set";
    // }
    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    struct Property {
        key: PropertyKey,
        value: Expr,
        kind: PropertyKind
    }

    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    enum PropertyKey {
        Literal(Literal),
        Identifier(Identifier)
    }

    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    enum PropertyKind {
        #[serde(rename="init")]
        Init,
        #[serde(rename="get")]
        Get,
        #[serde(rename="set")]
        Set
    }


    // interface Pattern <: Node { }
    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    #[serde(tag="type")]
    enum Pattern {
        #[serde(rename="ObjectPattern")]
        Object(ObjectPattern),
        #[serde(rename="ArrayPattern")]
        Array(ArrayPattern)
    }

    // interface ObjectPattern <: Pattern {
    //     type: "ObjectPattern";
    //     properties: [ { key: Literal | Identifier, value: Pattern } ];
    // }
    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    struct ObjectPattern {
        properties: Vec<ObjectPatternProp>
    }

    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    struct ObjectPatternProp {
        key: PropertyKey,
        value: Pattern
    }

    // interface ArrayPattern <: Pattern {
    //     type: "ArrayPattern";
    //     elements: [ Pattern | null ];
    // }
    type ArrayPattern = ArrayExpr;



    #[derive(Serialize, Deserialize, PartialEq, Debug)]
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
    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    struct SwitchCase {
        test: Option<Expr>,
        consequent: Vec<Stmt>
    }

    // interface CatchClause <: Node {
    //     type: "CatchClause";
    //     param: Pattern;
    //     guard: Expression | null;
    //     body: BlockStatement;
    // }
    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    struct CatchClause {
        param: Pattern,
        buard: Option<Expr>,
        body: BlockStmt
    }


    // interface Literal <: Node, Expression {
    //     type: "Literal";
    //     value: string | boolean | null | number | RegExp;
    // }
    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    enum Literal {
        Str(String),
        Bool(bool),
        Null,
        // TODO: Is this how JS represents numbers?
        Num(f64),
        Regex(String),
    }

    // enum UnaryOperator {
    //     "-" | "+" | "!" | "~" | "typeof" | "void" | "delete"
    // }
    #[derive(Serialize, Deserialize, PartialEq, Debug)]
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
    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    enum BinaryOp {
        #[serde(rename="==")]
        Eq,
        #[serde(rename="!=")]
        Neq,
        #[serde(rename="===")]
        Seq,
        #[serde(rename="!===")]
        Sneq,
        #[serde(rename="<")]
        Less,
        #[serde(rename="<=")]
        LessOrEq,
        #[serde(rename=">")]
        Greater,
        #[serde(rename=">=")]
        GreaterOrEq,
        #[serde(rename="<<")]
        LShift,
        #[serde(rename=">>")]
        RShift,
        #[serde(rename=">>>")]
        URshift,
        #[serde(rename="+")]
        Plus,
        #[serde(rename="-")]
        Minus,
        #[serde(rename="*")]
        Mul,
        #[serde(rename="/")]
        Div,
        #[serde(rename="%")]
        Mod,
        #[serde(rename="|")]
        Or,
        #[serde(rename="^")]
        Xor,
        #[serde(rename="&")]
        And,
        #[serde(rename="in")]
        In,
        #[serde(rename="instanceof")]
        Instanceof,
        // #[serde(rename="..")]
        // DotDot, Thisis E4x specific
    }

    // enum LogicalOperator {
    //     "||" | "&&"
    // }
    #[derive(Serialize, Deserialize, PartialEq, Debug)]
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
    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    enum AssignmentOp {
        #[serde(rename="=")]
        Assign,
        #[serde(rename="+=")]
        AddAssign,
        #[serde(rename="-=")]
        SubAssign,
        #[serde(rename="*=")]
        MulAssign,
        #[serde(rename="/=")]
        DivAssign,
        #[serde(rename="%=")]
        ModAssign,
        #[serde(rename="<<=")]
        LShiftAssign,
        #[serde(rename=">>=")]
        RShiftAssign,
        #[serde(rename=">>>=")]
        URShiftAssign,
        #[serde(rename="|=")]
        OrAssign,
        #[serde(rename="^=")]
        XorAssign,
        #[serde(rename="&=")]
        AndAssign,
    }

    // enum UpdateOperator {
    //     "++" | "--"
    // }
    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    enum UpdateOp {
        #[serde(rename="++")]
        Inc,
        #[serde(rename="++")]
        Dec
    }
}

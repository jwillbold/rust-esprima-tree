#[macro_use]
extern crate serde_derive;
extern crate serde;
#[macro_use]
extern crate serde_json;
extern crate serde_test;

// Based on https://esprima.readthedocs.io/en/latest/syntax-tree-format.html
pub mod estree {

    #![allow(unused_imports)]
    #![allow(dead_code)]
    use serde::ser::{Serialize, Serializer, SerializeStruct};
    use serde::de::{Deserialize, Deserializer};


    #[cfg(test)]
    fn check_se_de<T>(t: T, json: serde_json::Value) where for<'de> T: serde::Serialize +
                                                                       serde::Deserialize<'de> +
                                                                       std::fmt::Debug +
                                                                       std::cmp::PartialEq{
        assert_eq!(serde_json::to_value(&t).unwrap(), json);
        assert_eq!(t, serde_json::from_value::<T>(json).unwrap());
    }

    #[cfg(test)]
    fn check_se<T>(t: T, json: serde_json::Value) where T: serde::Serialize +
                                                          std::fmt::Debug +
                                                          std::cmp::PartialEq {
        assert_eq!(serde_json::to_value(&t).unwrap(), json);
    }

    // type BindingPattern = ArrayPattern | ObjectPattern;
    // TODO: tests
    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    enum BindingPattern {
        // interface ArrayPattern {
        //     type: 'ArrayPattern';
        //     elements: ArrayPatternElement[];
        // }
        #[serde(rename="ArrayPattern")]
        Array{elements: Vec<ArrayPatternElement>},

        // interface ObjectPattern {
        //     type: 'ObjectPattern';
        //     properties: Property[];
        // }
        #[serde(rename="ObjectPattern")]
        Object{properties: Vec<Property>}
    }

    // type ArrayPatternElement = AssignmentPattern | Identifier | BindingPattern | RestElement | null;
    // TODO tests
    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    #[serde(tag="type")]
    enum ArrayPatternElement {
        #[serde(rename="AssignmentPattern")]
        Assignment(AssignmentPattern),
        #[serde(rename="Identifier")]
        Ident(Identifier),
        #[serde(rename="BindingPattern")]
        Binding(BindingPattern),

        // interface RestElement {
        //     type: 'RestElement';
        //     argument: Identifier | BindingPattern;
        // }
        #[serde(rename="RestElement")]
        RestElement{argument: IdentOrPattern}, // TODO argument as obj
        Null,
    }

    // Gets serialized as object
    // TODO as obj
    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    #[serde(tag="type")]
    enum IdentOrPattern {
        #[serde(rename="Identifier")]
        Ident(Identifier),
        #[serde(rename="BindingPattern")]
        Pattern(Box<BindingPattern>)
    }

    // interface AssignmentPattern {
    //     type: 'AssignmentPattern';
    //     left: Identifier | BindingPattern;
    //     right: Expression;
    // }
    // TODO as obj func
    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    struct AssignmentPattern {
        left: IdentOrPattern,
        right: Expr
    }

    fn serialize_assignpat_as_obj<S>(ident: &Identifier, s: S) -> Result<S::Ok, S::Error>
        where S: Serializer {
        let mut state = s.serialize_struct("Identifier", 2)?;
        state.serialize_field("type", "Identifier")?;
        state.serialize_field("name", &ident.name)?;
        state.end()
    }


    // type Expression = ThisExpression | Identifier | Literal |
    //     ArrayExpression | ObjectExpression | FunctionExpression | ArrowFunctionExpression | ClassExpression |
    //     TaggedTemplateExpression | MemberExpression | Super | MetaProperty |
    //     NewExpression | CallExpression | UpdateExpression | AwaitExpression | UnaryExpression |
    //     BinaryExpression | LogicalExpression | ConditionalExpression |
    //     YieldExpression | AssignmentExpression | SequenceExpression;
    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    #[serde(tag="type")]
    enum Expr {
        #[serde(rename="ThisExpression")]
        This,
        #[serde(rename="Identifier")]
        Ident(Identifier),
        #[serde(rename="Literal")]
        Literal(Literal),
        #[serde(rename="ArrayExpression")]
        Array(ArrayExpr),
        #[serde(rename="ObjectExpression")]
        Object(ObjectExpr),
        #[serde(rename="FunctionExpression")]
        Function(FunctionExpr),
        #[serde(rename="ArrowFunctionExpression")]
        ArrowFunc(ArrowFuncExpr), // TODO test
        #[serde(rename="ArrowFunctionExpression")]
        Class(ClassExpr), // TODO test
        #[serde(rename="TaggedTemplateExpression")]
        TaggedTemplate, // (TaggedTemplateExpr) TODO
        #[serde(rename="MemberExpression")]
        Member(MemberExpr),
        Super,
        #[serde(rename="MetaProperty")]
        MetaProperty(MetaProperty), // TODO test
        #[serde(rename="NewExpression")]
        New(NewExpr),
        #[serde(rename="CallExpression")]
        Call(CallExpr), // TODO test
        #[serde(rename="UpdateExpression")]
        Update(UpdateExpr),
        #[serde(rename="AwaitExpression")]
        Await(AwaitExpr), // TODO test
        #[serde(rename="UnaryExpression")]
        Unary(UnaryExpr), // TODO test
        #[serde(rename="BinaryExpression")]
        Binary(BinaryExpr), // TODO test
        #[serde(rename="LogicalExpression")]
        Logical(LogicalExpr), // TODO test
        #[serde(rename="ConditionalExpression")]
        Conditional(ConditionalExpr), // TODO test
        #[serde(rename="YieldExpression")]
        Yield(YieldExpr), // TODO test
        #[serde(rename="AssignmentExpression")]
        Assignment(AssignmentExpr), // TODO test
        #[serde(rename="SequenceExpression")]
        Sequence(SequenceExpr) // TODO test
    }

    #[test]
    fn test_expr_se_de() {
        check_se_de(Expr::This, json!({"type": "ThisExpression"}));
        check_se_de(Expr::Ident(Identifier{name: "test".into()}),
                    json!({"type": "Identifier", "name": "test"}));
        check_se_de(Expr::Literal(Literal{value: LiteralKind::Bool(false),
                                          raw: "false".into(),
                                          regex: None}),
                    json!({"type": "Literal", "value": false, "raw": "false"}));
        // TODO
        // check_se_de(Expr::Literal(Literal{value: LiteralKind::RegEx("/.*/g".into()),
        //                                  raw: "/.*/g".into(),
        //                                  regex: Some(LiteralRegex{pattern: ".*".into(),
        //                                                           flags: "g".into()})}),
        //             json!({"type": "Literal",
        //                     "value": "/.*/g",
        //                     "raw": "/.*/g",
        //                     "regex": {"pattern": ".*", "flags": "g"}}));

        check_se_de(Expr::Array(
                        ArrayExpr{elements: vec![
                            ArrayExprElement::Expr(
                                Expr::Literal(
                                    Literal{value: LiteralKind::Num(0.0),
                                                    raw: "0".into(),
                                                    regex: None}
                        ))]}),
                    json!({"type": "ArrayExpression",
                            "elements": [ {
                                    "type": "Literal",
                                    "value": 0.0,
                                    "raw": "0"
                                }]
                            }));

        check_se_de(Expr::Object(ObjectExpr{properties: vec![Property{
                                                                key: Expr::Ident(Identifier{
                                                                        name: "ArrowRight".into()}),
                                                                computed: false,
                                                                value: Some(Expr::This),
                                                                kind: PropertyKind::Init,
                                                                shorthand: false}]}),
                    json!({"type": "ObjectExpression", "properties": [
                                                            {"type": "Property",
                                                                "key": {
                                                                    "type": "Identifier",
                                                                    "name": "ArrowRight"
                                                                },
                                                                "computed": false,
                                                                "value": {
                                                                    "type": "ThisExpression"
                                                                },
                                                                "kind": "init",
                                                                "method": false,
                                                                "shorthand": false}]}));

        check_se_de(Expr::Function(FunctionExpr{id: None,
                                                 params: vec![],
                                                 body: BlockStmt{body: vec![]},
                                                 generator: false,
                                                 expression: false,
                                                 async: false}),
                    json!({"type": "FunctionExpression",
                            "id": null,
                            "params": [],
                            "body": {
                                "type": "BlockStatement",
                                "body": []
                            },
                            "generator": false,
                            "expression": false,
                            "async": false
                        }));

        check_se_de(Expr::Member(MemberExpr{computed: false,
                                            object: Box::new(Expr::This),
                                            property: Box::new(Expr::Ident(Identifier{name: "snake".into()}))}),
                    json!({"type": "MemberExpression",
                                "computed": false,
                                "object": {
                                    "type": "ThisExpression"
                                },
                                "property": {
                                    "type": "Identifier",
                                    "name": "snake"
                                }}));

        check_se_de(Expr::New(NewExpr{callee: Box::new(Expr::This), arguments: vec![]}),
                    json!({"type": "NewExpression",
                            "callee": {"type": "ThisExpression"},
                            "arguments": []}));
        check_se_de(Expr::Update(UpdateExpr{operator: UpdateOp::Inc,
                                            argument: Box::new(Expr::This),
                                            prefix: false}),
                    json!({"type": "UpdateExpression",
                            "operator": "++",
                            "argument": {"type": "ThisExpression"},
                            "prefix": false}));
    }

    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    struct Identifier {
        pub name: String
    }

    fn serialize_ident_as_obj<S>(ident: &Identifier, s: S) -> Result<S::Ok, S::Error>
        where S: Serializer {
        let mut state = s.serialize_struct("Identifier", 2)?;
        state.serialize_field("type", "Identifier")?;
        state.serialize_field("name", &ident.name)?;
        state.end()
    }

    fn serialize_ident_as_opt_obj<S>(ident: &Option<Identifier>, s: S) -> Result<S::Ok, S::Error>
        where S: Serializer {
        match ident {
            Some(x) => serialize_ident_as_obj(x, s),
            None => s.serialize_none(),
        }
    }


    // interface Literal {
    //     type: 'Literal';
    //     value: boolean | number | string | RegExp | null;
    //     raw: string;
    //     regex?: { pattern: string, flags: string };
    // }
    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    struct Literal {
        value: LiteralKind,
        raw: String,
        #[serde(skip_serializing_if = "Option::is_none")]
        regex: Option<LiteralRegex>
    }

    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    #[serde(untagged)]
    enum LiteralKind {
        Bool(bool),
        Num(f64),
        Str(String),
        RegEx(String),
        Null
    }

    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    struct LiteralRegex {
        pattern: String,
        flags: String,
    }

    // interface ArrayExpression {
    //     type: 'ArrayExpression';
    //     elements: ArrayExpressionElement[];
    // }
    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    struct ArrayExpr {
        elements: Vec<ArrayExprElement>
    }

    // type ArrayExpressionElement = Expression | SpreadElement;
    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    #[serde(untagged)]
    enum ArrayExprElement {
        Expr(Expr),
        #[serde(serialize_with="spreadelement_as_obj")]
        Spread(SpreadElement)
    }

    // interface ObjectExpression {
    //     type: 'ObjectExpression';
    //     properties: Property[];
    // }
    #[derive(Deserialize, PartialEq, Debug)]
    struct ObjectExpr {
        properties: Vec<Property>
    }

    impl serde::Serialize for ObjectExpr {
        fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error> where S: Serializer {
            let mut state = serializer.serialize_struct("ObjectExpr", 2)?;
            state.serialize_field("type", "ObjectExpression")?;
            state.serialize_field("properties", &self.properties)?;
            state.end()
        }
    }

    // interface Property {
    //     type: 'Property';
    //     key: Expression;
    //     computed: boolean;
    //     value: Expression | null;
    //     kind: 'get' | 'set' | 'init';
    //     method: false;
    //     shorthand: boolean;
    // }
    #[derive(Deserialize, PartialEq, Debug)]
    struct Property {
        key: Expr,
        computed: bool,
        value: Option<Expr>,
        kind: PropertyKind,
        // method: bool, // This field is constant false
        shorthand: bool
    }

    impl serde::Serialize for Property {
        fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error> where S: Serializer {
            let mut state = serializer.serialize_struct("Property", 7)?;
            state.serialize_field("type", "Property")?;
            state.serialize_field("key", &self.key)?;
            state.serialize_field("computed", &self.computed)?;
            state.serialize_field("value", &self.value)?;
            state.serialize_field("kind", &self.kind)?;
            state.serialize_field("method", &false)?;
            state.serialize_field("shorthand", &self.shorthand)?;
            state.end()
        }
    }

    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    enum PropertyKind {
        #[serde(rename="get")]
        Get,
        #[serde(rename="set")]
        Set,
        #[serde(rename="init")]
        Init
    }

    // interface FunctionExpression {
    //     type: 'FunctionExpression';
    //     id: Identifier | null;
    //     params: FunctionParameter[];
    //     body: BlockStatement;
    //     generator: boolean;
    //     async: boolean;
    //     expression: boolean;
    // }
    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    struct FunctionExpr {
        #[serde(serialize_with="serialize_ident_as_opt_obj")]
        id: Option<Identifier>,
        params: Vec<FunctionParam>,
        #[serde(serialize_with="blockstmt_as_obj")]
        body: BlockStmt,
        generator: bool,
        async: bool,
        expression: bool,
    }

    // type FunctionParameter = AssignmentPattern | Identifier | BindingPattern;
    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    #[serde(rename="type")]
    enum FunctionParam {
        #[serde(rename="AssignmentPattern")]
        Assignment(AssignmentPattern),
        #[serde(rename="Identifier")]
        Ident(Identifier),
        #[serde(rename="BindingPattern")]
        Binding(BindingPattern)
    }

    // interface ArrowFunctionExpression {
    //     type: 'ArrowFunctionExpression';
    //     id: Identifier | null;
    //     params: FunctionParameter[];
    //     body: BlockStatement | Expression;
    //     generator: boolean;
    //     async: boolean;
    //     expression: false;
    // }

    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    struct ArrowFuncExpr {
        #[serde(serialize_with="serialize_ident_as_opt_obj")]
        id: Option<Identifier>,
        params: Vec<FunctionParam>,
        body: ArrowFuncExprBody,
        generator: bool,
        async: bool,
        expression: bool
    }

    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    enum ArrowFuncExprBody {
        #[serde(serialize_with="blockstmt_as_obj")]
        Block(BlockStmt),
        Expr(Box<Expr>)
    }

    // interface ClassExpression {
    //     type: 'ClassExpression';
    //     id: Identifier | null;
    //     superClass: Identifier | null;
    //     body: ClassBody;
    // }
    type ClassExpr = ClassDecl;

    // interface TaggedTemplateExpression {
    //     type: 'TaggedTemplateExpression';
    //     readonly tag: Expression;
    //     readonly quasi: TemplateLiteral;
    // }
    // TODO

    // interface MemberExpression {
    //     type: 'MemberExpression';
    //     computed: boolean;
    //     object: Expression;
    //     property: Expression;
    // }
    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    struct MemberExpr {
        computed: bool,
        object: Box<Expr>,
        property: Box<Expr>,
    }

    // interface MetaProperty {
    //     type: 'MetaProperty';
    //     meta: Identifier;
    //     property: Identifier;
    // }
    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    struct MetaProperty {
        meta: Identifier,
        property: Identifier
    }

    // interface CallExpression {
    //     type: 'CallExpression';
    //     callee: Expression | Import;
    //     arguments: ArgumentListElement[];
    // }
    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    struct CallExpr {
        callee: CallExprCallee,
        argument: Vec<ArgumentListElement>
    }

    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    enum CallExprCallee {
        Expr(Box<Expr>),
        Import,
    }

    // interface Import {
    //     type: 'Import';
    // }

    // interface NewExpression {
    //     type: 'NewExpression';
    //     callee: Expression;
    //     arguments: ArgumentListElement[];
    // }
    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    struct NewExpr {
        callee: Box<Expr>,
        arguments: Vec<ArgumentListElement>,
    }

    // type ArgumentListElement = Expression | SpreadElement;
    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    #[serde(untagged)]
    enum ArgumentListElement {
        Expr(Box<Expr>),
        #[serde(serialize_with="spreadelement_as_obj")]
        Spread(SpreadElement),
    }

    // interface SpreadElement {
    //     type: 'SpreadElement';
    //     argument: Expression;
    // }
    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    struct SpreadElement {
        argument: Box<Expr>
    }

    fn spreadelement_as_obj<S>(spread: &SpreadElement, s: S) -> Result<S::Ok, S::Error>
        where S: Serializer {
        let mut state = s.serialize_struct("SpreadElement", 2)?;
        state.serialize_field("type", "SpreadElement")?;
        state.serialize_field("argument", &spread.argument)?;
        state.end()
    }

    // interface UpdateExpression {
    //     type: 'UpdateExpression';
    //     operator: '++' | '--';
    //     argument: Expression;
    //     prefix: boolean;
    // }
    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    struct UpdateExpr {
        operator: UpdateOp,
        argument: Box<Expr>,
        prefix: bool
    }

    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    enum UpdateOp {
        #[serde(rename="++")]
        Inc,
        #[serde(rename="--")]
        Dec
    }

    // interface AwaitExpression {
    //     type: 'AwaitExpression';
    //     argument: Expression;
    // }
    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    struct AwaitExpr {
        argument: Box<Expr>
    }

    // interface UnaryExpression {
    //     type: 'UnaryExpression';
    //     operator: '+' | '-' | '~' | '!' | 'delete' | 'void' | 'typeof';
    //     argument: Expression;
    //     prefix: true;
    // }
    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    struct UnaryExpr {
        operator: UnaryOp,
        argument: Box<Expr>,
        // TODO: true
        prefix: bool
    }

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

    // interface BinaryExpression {
    //     type: 'BinaryExpression';
    //     operator: 'instanceof' | 'in' | '+' | '-' | '*' | '/' | '%' | '**' |
    //         '|' | '^' | '&' | '==' | '!=' | '===' | '!==' |
    //         '<' | '>' | '<=' | '<<' | '>>' | '>>>';
    //     left: Expression;
    //     right: Expression;
    // }
    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    struct BinaryExpr {
        operator: BinaryOp,
        left: Box<Expr>,
        right: Box<Expr>
    }

    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    #[serde(untagged)]
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
    }

    // interface LogicalExpression {
    //     type: 'LogicalExpression';
    //     operator: '||' | '&&';
    //     left: Expression;
    //     right: Expression;
    // }
    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    struct LogicalExpr {
        operator: LogicalOp,
        left: Box<Expr>,
        right: Box<Expr>
    }

    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    enum LogicalOp {
        #[serde(rename="||")]
        Or,
        #[serde(rename="&&")]
        And,
    }

    // interface ConditionalExpression {
    //     type: 'ConditionalExpression';
    //     test: Expression;
    //     consequent: Expression;
    //     alternate: Expression;
    // }
    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    struct ConditionalExpr {
        test: Box<Expr>,
        consequent: Box<Expr>,
        alternate: Box<Expr>
    }

    // interface YieldExpression {
    //     type: 'YieldExpression';
    //     argument: Expression | null;
    //     delegate: boolean;
    // }
    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    struct YieldExpr {
        argument: Option<Box<Expr>>,
        delegate: bool,
    }

    // interface AssignmentExpression {
    //     type: 'AssignmentExpression';
    //     operator: '=' | '*=' | '**=' | '/=' | '%=' | '+=' | '-=' |
    //         '<<=' | '>>=' | '>>>=' | '&=' | '^=' | '|=';
    //     left: Expression;
    //     right: Expression;
    // }
    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    struct AssignmentExpr {
        operator: AssignmentOp,
        left: Box<Expr>,
        right: Box<Expr>
    }

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

    // interface SequenceExpression {
    //     type: 'SequenceExpression';
    //     expressions: Expression[];
    // }
    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    struct SequenceExpr {
        expressions: Vec<Expr>
    }


    // type Statement = BlockStatement | BreakStatement | ContinueStatement |
    //     DebuggerStatement | DoWhileStatement | EmptyStatement |
    //     ExpressionStatement | ForStatement | ForInStatement |
    //     ForOfStatement | FunctionDeclaration | IfStatement |
    //     LabeledStatement | ReturnStatement | SwitchStatement |
    //     ThrowStatement | TryStatement | VariableDeclaration |
    //     WhileStatement | WithStatement;
    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    #[serde(tag="type")]
    enum Stmt {
        #[serde(rename="BlockStatement")]
        Block(BlockStmt),

        // interface BreakStatement {
        //     type: 'BreakStatement';
        //     label: Identifier | null;
        // }
        #[serde(rename="BreakStatement")]
        Break{label: Option<Identifier>},

        // interface ContinueStatement {
        //     type: 'ContinueStatement';
        //     label: Identifier | null;
        // }
        #[serde(rename="ContinueStatement")]
        Continue{label: Option<Identifier>},

        // interface DebuggerStatement {
        //     type: 'DebuggerStatement';
        // }
        #[serde(rename="DebuggerStatement")]
        Debugger,

        // interface DoWhileStatement {
        //     type: 'DoWhileStatement';
        //     body: Statement;
        //     test: Expression;
        // }
        #[serde(rename="DoWhileStatement")]
        DoWhile{body: Box<Stmt>,
                test: Expr},

        // interface EmptyStatement {
        //     type: 'EmptyStatement';
        // }
        #[serde(rename="EmptyStatement")]
        Empty,

        // interface ExpressionStatement {
        //     type: 'ExpressionStatement';
        //     expression: Expression;
        //     directive?: string;
        // }
        #[serde(rename="ExpressionStatement")]
        Expr{expression: Expr,
            #[serde(skip_serializing_if = "Option::is_some")]
            directive: Option<String>},

        // interface ForStatement {
        //     type: 'ForStatement';
        //     init: Expression | VariableDeclaration | null;
        //     test: Expression | null;
        //     update: Expression | null;
        //     body: Statement;
        // }
        #[serde(rename="ForStatement")]
        For{init: Option<FotStmtInit>,
            test: Option<Expr>,
            update: Option<Expr>,
            body: Box<Stmt>},

        // interface ForInStatement {
        //     type: 'ForInStatement';
        //     left: Expression;
        //     right: Expression;
        //     body: Statement;
        //     each: false; // TODO this must be constant false
        // }
        #[serde(rename="ForInStatement")]
        ForIn{left: Expr, right: Expr, body: Box<Stmt>, each: bool},

        // interface ForOfStatement {
        //     type: 'ForOfStatement';
        //     left: Expression;
        //     right: Expression;
        //     body: Statement;
        // }
        #[serde(rename="ForOfStatement")]
        ForOf{left: Expr, right: Expr, body: Box<Stmt>},

        #[serde(rename="FunctionDeclaration")]
        Function(FunctionDecl),

        // interface IfStatement {
        //     type: 'IfStatement';
        //     test: Expression;
        //     consequent: Statement;
        //     alternate?: Statement;
        // }
        #[serde(rename="IfStatement")]
        If{test: Expr,
           consequent: Box<Stmt>,
           alternate: Option<Box<Stmt>>},

        //  interface LabeledStatement {
        //     type: 'LabeledStatement';
        //     label: Identifier;
        //     body: Statement;
        // }
        #[serde(rename="LabeledStatement")]
        Labled{
             #[serde(serialize_with="serialize_ident_as_obj")]
             label: Identifier,
             body: Box<Stmt>},

        // interface ReturnStatement {
        //  type: 'ReturnStatement';
        //  argument: Expression | null;
        // }
        #[serde(rename="ReturnStmt")]
        Return{argument:Option<Expr>},

        // interface SwitchStatement {
        //     type: 'SwitchStatement';
        //     discriminant: Expression;
        //     cases: SwitchCase[];
        // }
        #[serde(rename="SwitchStatement")]
        Switch{discriminant: Expr, cases: Vec<SwitchCase>},

        // interface ThrowStatement {
        //     type: 'ThrowStatement';
        //     argument: Expression;
        // }
        #[serde(rename="ThrowStatement")]
        Throw{argument: Expr},

        // interface TryStatement {
        //     type: 'TryStatement';
        //     block: BlockStatement;
        //     handler: CatchClause | null;
        //     finalizer: BlockStatement | null;
        // }
        #[serde(rename="TryStatement")]
        Try{block: BlockStmt,
            handler: Option<CatchClause>,
            #[serde(serialize_with="blockstmt_as_obj")]
            finalizer: Option<BlockStmt>},

        #[serde(rename="VariableDeclaration")]
        VarDecl(VariableDecl),

        // interface WhileStatement {
        //     type: 'WhileStatement';
        //     test: Expression;
        //     body: Statement;
        // }
        #[serde(rename="WhileStatement")]
        While{test: Expr, body: Box<Stmt>},

        // interface WithStatement {
        //     type: 'WithStatement';
        //     object: Expression;
        //     body: Statement;
        // }
        #[serde(rename="WithStatement")]
        With{object: Expr, body: Box<Stmt>}
    }

    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    // #[serde(untagged)]
    enum FotStmtInit {
        Expr(Expr),
        // TODO as obj
        VarDecl(VariableDecl),
    }

    // interface SwitchCase {
    //     type: 'SwitchCase';
    //     test: Expression | null;
    //     consequent: Statement[];
    // }
    // TODO as obj
    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    struct SwitchCase {
        test: Option<Expr>,
        consequent: Vec<Stmt>
    }

    // interface CatchClause {
    //     type: 'CatchClause';
    //     param: Identifier | BindingPattern;
    //     body: BlockStatement;
    // }
    // TODO as obj
    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    struct CatchClause {
        param: IdentOrPattern,
        body: BlockStmt,
    }


    #[test]
    fn test_stmt_se_de() {
        check_se_de(Stmt::Block(BlockStmt{body: vec![
                                            StmtListItem::Stmt(Stmt::Block(
                                                BlockStmt{body: vec![]}))
                    ]}),
                    json!({"type": "BlockStatement", "body": [
                                        {"type": "BlockStatement", "body": []}]}));
        check_se_de(Stmt::Break{label: None},
                    json!({"type": "BreakStatement", "label": serde_json::Value::Null}));
    }

    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    struct BlockStmt {
        body: Vec<StmtListItem>
    }

    fn blockstmt_as_obj<S>(block: &BlockStmt, s: S) -> Result<S::Ok, S::Error>
        where S: Serializer {
        let mut state = s.serialize_struct("BlockStmt", 2)?;
        state.serialize_field("type", "BlockStatement")?;
        state.serialize_field("body", &block.body)?;
        state.end()
    }


    // type Declaration = ClassDeclaration | FunctionDeclaration |  VariableDeclaration;
    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    #[serde(tag="type")]
    enum Decl {
        #[serde(rename="ClassDeclaration")]
        Class(ClassDecl),
        #[serde(rename="FunctionDeclaration")]
        Function,
        #[serde(rename="VariableDeclaration")]
        Variable(VariableDecl),
    }

    #[test]
    fn tetst_decl_se_de() {
        check_se_de(Decl::Class(ClassDecl{id: Some(Identifier{name: "TestClass".into()}),
                                         super_class: None,
                                         body: ClassBody{body: vec![
                                             MethodDef {
                                                key: Some(Expr::Ident(Identifier{name: "func1".into()})),
                                                computed: false,
                                                // value:
                                                kind: MethodDefKind::Method,
                                                stat: false
                                             }
                                         ]}}),
                    json!({"type": "ClassDeclaration",
                            "id": {
                                "type": "Identifier",
                                "name": "TestClass"
                            },
                            "superClass": serde_json::Value::Null,
                            "body": {
                                "type": "ClassBody",
                                "body": [
                                    {
                                        "type": "MethodDefinition",
                                        "key": {
                                            "type": "Identifier",
                                            "name": "func1"
                                        },
                                        "computed": false,
                                        // "value": {
                                        //     "type": "FunctionExpression",
                                        //     "id": null,
                                        //     "params": [],
                                        //     "body": {
                                        //         "type": "BlockStatement",
                                        //         "body": []
                                        //     },
                                        //     "generator": false,
                                        //     "expression": false,
                                        //     "async": false
                                        // },
                                        "kind": "method",
                                        "static": false
                                    }
                                ]
                            }
                        }));
    }

    // interface ClassDeclaration {
    //     type: 'ClassDeclaration';
    //     id: Identifier | null;
    //     superClass: Identifier | null;
    //     body: ClassBody;
    // }
    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    struct ClassDecl {
        #[serde(serialize_with="serialize_ident_as_opt_obj")]
        id: Option<Identifier>,
        #[serde(rename="superClass")]
        #[serde(serialize_with="serialize_ident_as_opt_obj")]
        super_class: Option<Identifier>,
        #[serde(serialize_with="serialize_classbody_as_obj")]
        body: ClassBody,
    }

    fn serialize_classbody_as_obj<S>(cb: &ClassBody, s: S) -> Result<S::Ok, S::Error>
        where S: Serializer {
        let mut state = s.serialize_struct("ClassBody", 2)?;
        state.serialize_field("type", "ClassBody".into())?;
        state.serialize_field("body", &cb.body)?;
        state.end()
    }

    // interface ClassBody {
    //     type: 'ClassBody';
    //     body: MethodDefinition[];
    // }
    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    struct ClassBody {
        body: Vec<MethodDef>
    }

    // interface MethodDefinition {
    //     type: 'MethodDefinition';
    //     key: Expression | null;
    //     computed: boolean;
    //     value: FunctionExpression | null;
    //     kind: 'method' | 'constructor';
    //     static: boolean;
    // }
    #[derive(Deserialize, PartialEq, Debug)]
    struct MethodDef {
        key: Option<Expr>,
        computed: bool,
        // value: Option<FunctionExpr>, TODO as obj
        kind: MethodDefKind,
        #[serde(rename="static")]
        stat: bool
    }

    impl serde::Serialize for MethodDef {
        fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error> where S: Serializer {
            let mut state = serializer.serialize_struct("MethodDef", 5)?;
            state.serialize_field("type", "MethodDefinition")?;
            state.serialize_field("key", &self.key)?;
            state.serialize_field("computed", &self.computed)?;
            state.serialize_field("kind", &self.kind)?;
            state.serialize_field("static", &self.stat)?;
            state.end()
        }
    }

    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    enum MethodDefKind {
        #[serde(rename="method")]
        Method,
        #[serde(rename="constructor")]
        Constructor
    }

    // interface FunctionDeclaration {
    //     type: 'FunctionDeclaration';
    //     id: Identifier | null;
    //     params: FunctionParameter[];
    //     body: BlockStatement;
    //     generator: boolean;
    //     async: boolean;
    //     expression: false;
    // }
    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    struct FunctionDecl {
        #[serde(serialize_with="serialize_ident_as_opt_obj")]
        id: Option<Identifier>,
        params: Vec<FunctionParam>,
        body: BlockStmt, // TODO as obj
        generator: bool,
        async: bool,
        expression: bool, // TODO This must be constant false
    }

    // interface VariableDeclaration {
    //     type: 'VariableDeclaration';
    //     declarations: VariableDeclarator[];
    //     kind: 'var' | 'const' | 'let';
    // }
    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    struct VariableDecl {
        declarations: Vec<VariableDeclarator>,
        kind: VariableDeclKind,
    }

    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    #[serde(tag="type")]
    enum VariableDeclKind {
        #[serde(rename="var")]
        Var,
        #[serde(rename="const")]
        Const,
        #[serde(rename="let")]
        Let
    }

    // interface VariableDeclarator {
    //     type: 'VariableDeclarator';
    //     id: Identifier | BindingPattern;
    //     init: Expression | null;
    // }
    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    struct VariableDeclarator {
        id: IdentOrPattern,
        init: Option<Expr>
    }




    #[derive(Deserialize, PartialEq, Debug)]
    struct Program {
        body: ProgramType
    }

    impl serde::Serialize for Program {
        fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error> where S: Serializer {
            let mut state = serializer.serialize_struct("Color", 3)?;
            state.serialize_field("type", "Program")?;
            state.serialize_field("sourceType", match self.body {
                ProgramType::Script(_) => "script",
                ProgramType::Module(_) => "module",
            })?;
            state.serialize_field("body", &self.body)?;
            state.end()
        }
    }

    #[test]
    fn test_progman_se_de() {
        check_se_de(Program{body: ProgramType::Script(vec![])},
                    json!({"type": "Program",
                            "sourceType": "script",
                            "body": []}));
    }

    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    #[serde(untagged)]
    enum ProgramType {
        Script(Vec<StmtListItem>),
        Module(ModuleItem)
    }

    // type StatementListItem = Declaration | Statement;
    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    #[serde(untagged)]
    enum StmtListItem {
        Decl(Decl),
        Stmt(Stmt),
    }

    // type ModuleItem = ImportDeclaration | ExportDeclaration | StatementListItem;
    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    enum ModuleItem {
        // type ImportDeclaration {
        //     type: 'ImportDeclaration';
        //     specifiers: ImportSpecifier[];
        //     source: Literal;
        // }
        Import{specifiers: Vec<ImportSpecifier>, source: Literal},
        Export(ExportDecl),
        Stmts(StmtListItem)
    }

    // interface ImportSpecifier {
    //     type: 'ImportSpecifier' | 'ImportDefaultSpecifier' | 'ImportNamespaceSpecifier';
    //     local: Identifier;
    //     imported?: Identifier;
    // }
    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    struct ImportSpecifier {
        // local:
    }

    // type ExportDeclaration = ExportAllDeclaration | ExportDefaultDeclaration | ExportNamedDeclaration;
    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    #[serde(tag="type")]
    enum ExportDecl {
        // interface ExportAllDeclaration {
        //     type: 'ExportAllDeclaration';
        //     source: Literal;
        // }
        ExportAll{source: Literal},


        // interface ExportDefaultDeclaration {
        //     type: 'ExportDefaultDeclaration';
        //     declaration: Identifier | BindingPattern | ClassDeclaration | Expression | FunctionDeclaration;
        // }
        ExportDefault{declaration: ExportDefaultDeclKind},


        // interface ExportNamedDeclaration {
        //     type: 'ExportNamedDeclaration';
        //     declaration: ClassDeclaration | FunctionDeclaration | VariableDeclaration;
        //     specifiers: ExportSpecifier[];
        //     source: Literal;
        // }
        ExportNamed{decleration: Decl,
                    specifiers: Vec<ExportSpecifier>,
                    source: Literal}
    }

    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    #[serde(untagged)]
    enum ExportDefaultDeclKind {
        Ident(Identifier),
        Binding(BindingPattern),
        Class(ClassDecl),
        Expr(Expr),
        Function(FunctionDecl)
    }

    // interface ExportSpecifier {
    //     type: 'ExportSpecifier';
    //     exported: Identifier;
    //     local: Identifier;
    // };
    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    struct ExportSpecifier {
        #[serde(serialize_with="serialize_ident_as_obj")]
        exported: Identifier,
        #[serde(serialize_with="serialize_ident_as_obj")]
        local: Identifier
    }


}

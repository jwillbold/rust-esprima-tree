#![allow(unused_imports)]
use serde::ser::{Serialize, Serializer, SerializeStruct};
use serde::de::{Deserialize, Deserializer};

use declerations::*;
use statements::*;
use patterns::{AssignmentPattern, BindingPattern};
#[cfg(test)]
use helpers::{check_se_de};


// type Expression = ThisExpression | Identifier | Literal |
//     ArrayExpression | ObjectExpression | FunctionExpression | ArrowFunctionExpression | ClassExpression |
//     TaggedTemplateExpression | MemberExpression | Super | MetaProperty |
//     NewExpression | CallExpression | UpdateExpression | AwaitExpression | UnaryExpression |
//     BinaryExpression | LogicalExpression | ConditionalExpression |
//     YieldExpression | AssignmentExpression | SequenceExpression;
#[derive(Serialize, Deserialize, PartialEq, Debug)]
#[serde(tag="type")]
pub enum Expr {
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
    ArrowFunc(ArrowFuncExpr),

    #[serde(rename="ClassExpression")]
    Class(ClassExpr),

    // TODO test
    #[serde(rename="TaggedTemplateExpression")]
    TaggedTemplate(TaggedTemplateExpr),

    #[serde(rename="MemberExpression")]
    Member(MemberExpr),

    Super,

    #[serde(rename="MetaProperty")]
    MetaProperty(MetaProperty),

    #[serde(rename="NewExpression")]
    New(NewExpr),

    #[serde(rename="CallExpression")]
    Call(CallExpr),

    #[serde(rename="UpdateExpression")]
    Update(UpdateExpr),

     // TODO test
    #[serde(rename="AwaitExpression")]
    Await(AwaitExpr),

    #[serde(rename="UnaryExpression")]
    Unary(UnaryExpr),

    #[serde(rename="BinaryExpression")]
    Binary(BinaryExpr),

    #[serde(rename="LogicalExpression")]
    Logical(LogicalExpr),

    #[serde(rename="ConditionalExpression")]
    Conditional(ConditionalExpr),

    // TODO test
    #[serde(rename="YieldExpression")]
    Yield(YieldExpr),

    #[serde(rename="AssignmentExpression")]
    Assignment(AssignmentExpr),

    #[serde(rename="SequenceExpression")]
    Sequence(SequenceExpr)
}


#[derive(Serialize, Deserialize, PartialEq, Debug)]
#[serde(tag="type")]
pub struct Identifier {
    pub name: String
}

pub type Id = Identifier;

impl Identifier {
    pub fn new(s: &str) -> Self {
        Identifier{name: s.into()}
    }
}

// interface Literal {
//     type: 'Literal';
//     value: boolean | number | string | RegExp | null;
//     raw: string;
//     regex?: { pattern: string, flags: string };
// }
#[derive(Serialize, Deserialize, PartialEq, Debug)]
#[serde(tag="type")]
pub struct Literal {
    pub value: LiteralKind,
    pub raw: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub regex: Option<LiteralRegex>
}

pub type Lit = Literal;

impl Literal {
    pub fn new_str(s: &str) -> Self {
        Literal {
            value: LiteralKind::Str(s.to_string()),
            raw: s.into(),
            regex: None
        }
    }

    pub fn new_f(f: f64) -> Self {
        Literal {
            value: LiteralKind::Num(f),
            // This looks like an overkill, but it ensures that the number '2.0' actually
            // gets printed as '2.0'. In addition this should prevent any problem that could
            // occur due to different number serilaizations
            raw: serde_json::Value::Number(serde_json::Number::from_f64(f).unwrap()).to_string(),
            regex: None,
        }
    }

    pub fn new_int(i: i64) -> Self {
        Literal {
            value: LiteralKind::Int(i),
            raw: i.to_string(),
            regex: None
        }
    }
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
#[serde(untagged)]
pub enum LiteralKind {
    Bool(bool),
    // This is not described in the estree standard and is only used internally. If a number
    // is parsed as "2", it should also be serialized as 2. On the contrary if it was "2.0"
    // it should still be "2.0" after parsing and serializing it.
    Int(i64),
    Num(f64),
    Str(String),
    RegEx(String),
    Null
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
pub struct LiteralRegex {
    pub pattern: String,
    pub flags: String,
}

// interface ArrayExpression {
//     type: 'ArrayExpression';
//     elements: ArrayExpressionElement[];
// }
#[derive(Serialize, Deserialize, PartialEq, Debug)]
#[serde(tag="type", rename="ArrayExpression")]
pub struct ArrayExpr {
    pub elements: Vec<ArrayExprElement>
}

// type ArrayExpressionElement = Expression | SpreadElement;
#[derive(Serialize, Deserialize, PartialEq, Debug)]
#[serde(untagged)]
pub enum ArrayExprElement {
    Expr(Expr),
    Spread(SpreadElement)
}

// interface ObjectExpression {
//     type: 'ObjectExpression';
//     properties: Property[];
// }
#[derive(Serialize, Deserialize, PartialEq, Debug)]
#[serde(tag="type", rename="ObjectExpression")]
pub struct ObjectExpr {
    pub properties: Vec<Property>
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
pub struct Property {
    pub key: Expr,
    pub computed: bool,
    pub value: Option<Expr>,
    pub kind: PropertyKind,
    // method: bool, // This field is constant false
    pub shorthand: bool
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
pub enum PropertyKind {
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
#[serde(tag="type", rename="FunctionExpression")]
pub struct FunctionExpr {
    pub id: Option<Identifier>,
    pub params: Vec<FunctionParam>,
    pub body: BlockStmt,
    pub generator: bool,
    pub async: bool,
    pub expression: bool,
}

// type FunctionParameter = AssignmentPattern | Identifier | BindingPattern;
#[derive(Serialize, Deserialize, PartialEq, Debug)]
#[serde(tag="type")]
pub enum FunctionParam {
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
#[serde(tag="type", rename="ArrowFunctionExpression")]
pub struct ArrowFuncExpr {
    pub id: Option<Identifier>,
    pub params: Vec<FunctionParam>,
    pub body: ArrowFuncExprBody,
    pub generator: bool,
    pub async: bool,
    pub expression: bool
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
#[serde(untagged)]
pub enum ArrowFuncExprBody {
    Block(BlockStmt),
    Expr(Box<Expr>)
}

// interface ClassExpression {
//     type: 'ClassExpression';
//     id: Identifier | null;
//     superClass: Identifier | null;
//     body: ClassBody;
// }
#[derive(Serialize, Deserialize, PartialEq, Debug)]
#[serde(tag="type", rename="ClassExpression")]
pub struct ClassExpr {
    pub id: Option<Identifier>,
    #[serde(rename="superClass")]
    pub super_class: Option<Identifier>,
    pub body: ClassBody
}

// interface TaggedTemplateExpression {
//     type: 'TaggedTemplateExpression';
//     readonly tag: Expression;
//     readonly quasi: TemplateLiteral;
// }
#[derive(Serialize, Deserialize, PartialEq, Debug)]
#[serde(tag="type", rename="TaggedTemplateExpression")]
pub struct TaggedTemplateExpr {
    pub tag: Box<Expr>,
    pub quasi: TemplateLiteral
}

// interface TemplateElement {
//     type: 'TemplateElement';
//     value: { cooked: string; raw: string };
//     tail: boolean;
// }
#[derive(Serialize, Deserialize, PartialEq, Debug)]
pub struct TemplateElement {
    #[serde(flatten)]
    pub value: TemplateElementValue,
    pub tail: bool
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
pub struct TemplateElementValue {
    pub cooked: String,
    pub raw: String,
}

// interface TemplateLiteral {
//     type: 'TemplateLiteral';
//     quasis: TemplateElement[];
//     expressions: Expression[];
// }
#[derive(Serialize, Deserialize, PartialEq, Debug)]
pub struct TemplateLiteral {
    pub quasis: Vec<TemplateElement>,
    pub expressions: Vec<Expr>
}

// interface MemberExpression {
//     type: 'MemberExpression';
//     computed: boolean;
//     object: Expression;
//     property: Expression;
// }
#[derive(Serialize, Deserialize, PartialEq, Debug)]
#[serde(tag="type", rename="MemberExpression")]
pub struct MemberExpr {
    pub computed: bool,
    pub object: Box<Expr>,
    pub property: Box<Expr>
}

// TODO test
// interface MetaProperty {
//     type: 'MetaProperty';
//     meta: Identifier;
//     property: Identifier;
// }
#[derive(Serialize, Deserialize, PartialEq, Debug)]
#[serde(tag="type")]
pub struct MetaProperty {
    pub meta: Identifier,
    pub property: Identifier
}

// interface NewExpression {
//     type: 'NewExpression';
//     callee: Expression;
//     arguments: ArgumentListElement[];
// }
#[derive(Serialize, Deserialize, PartialEq, Debug)]
#[serde(tag="type", rename="NewExpression")]
pub struct NewExpr {
    pub callee: Box<Expr>,
    pub arguments: Vec<ArgumentListElement>,
}

// type ArgumentListElement = Expression | SpreadElement;
#[derive(Serialize, Deserialize, PartialEq, Debug)]
#[serde(untagged)]
pub enum ArgumentListElement {
    Expr(Box<Expr>),
    Spread(SpreadElement),
}

// interface SpreadElement {
//     type: 'SpreadElement';
//     argument: Expression;
// }
#[derive(Serialize, Deserialize, PartialEq, Debug)]
#[serde(tag="type")]
pub struct SpreadElement {
    pub argument: Box<Expr>
}

// interface CallExpression {
//     type: 'CallExpression';
//     callee: Expression | Import;
//     arguments: ArgumentListElement[];
// }
#[derive(Serialize, Deserialize, PartialEq, Debug)]
#[serde(tag="type", rename="CallExpression")]
pub struct CallExpr {
    pub callee: CallExprCallee,
    pub arguments: Vec<ArgumentListElement>
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
#[serde(untagged)]
pub enum CallExprCallee {
    Expr(Box<Expr>),

    // TODO
    // interface Import {
    //     type: 'Import';
    // }
    Import,
}

// interface UpdateExpression {
//     type: 'UpdateExpression';
//     operator: '++' | '--';
//     argument: Expression;
//     prefix: boolean;
// }
#[derive(Serialize, Deserialize, PartialEq, Debug)]
#[serde(tag="type", rename="UpdateExpression")]
pub struct UpdateExpr {
    pub operator: UpdateOp,
    pub argument: Box<Expr>,
    pub prefix: bool
}

// interface AwaitExpression {
//     type: 'AwaitExpression';
//     argument: Expression;
// }
#[derive(Serialize, Deserialize, PartialEq, Debug)]
#[serde(tag="type", rename="AwaitExpression")]
pub struct AwaitExpr {
    pub argument: Box<Expr>
}

// interface UnaryExpression {
//     type: 'UnaryExpression';
//     operator: '+' | '-' | '~' | '!' | 'delete' | 'void' | 'typeof';
//     argument: Expression;
//     prefix: true;
// }
#[derive(Serialize, Deserialize, PartialEq, Debug)]
#[serde(tag="type", rename="UnaryExpression")]
pub struct UnaryExpr {
    pub operator: UnaryOp,
    pub argument: Box<Expr>,
    // TODO: true
    pub prefix: bool
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
#[serde(tag="type", rename="BinaryExpression")]
pub struct BinaryExpr {
    pub operator: BinaryOp,
    pub left: Box<Expr>,
    pub right: Box<Expr>
}

// interface LogicalExpression {
//     type: 'LogicalExpression';
//     operator: '||' | '&&';
//     left: Expression;
//     right: Expression;
// }
#[derive(Serialize, Deserialize, PartialEq, Debug)]
#[serde(tag="type", rename="LogicalExpression")]
pub struct LogicalExpr {
    pub operator: LogicalOp,
    pub left: Box<Expr>,
    pub right: Box<Expr>
}

// interface ConditionalExpression {
//     type: 'ConditionalExpression';
//     test: Expression;
//     consequent: Expression;
//     alternate: Expression;
// }
#[derive(Serialize, Deserialize, PartialEq, Debug)]
#[serde(tag="type", rename="ConditionalExpression")]
pub struct ConditionalExpr {
    pub test: Box<Expr>,
    pub consequent: Box<Expr>,
    pub alternate: Box<Expr>
}

// interface YieldExpression {
//     type: 'YieldExpression';
//     argument: Expression | null;
//     delegate: boolean;
// }
#[derive(Serialize, Deserialize, PartialEq, Debug)]
#[serde(tag="type", rename="YieldExpression")]
pub struct YieldExpr {
    pub argument: Option<Box<Expr>>,
    pub delegate: bool,
}

// interface AssignmentExpression {
//     type: 'AssignmentExpression';
//     operator: '=' | '*=' | '**=' | '/=' | '%=' | '+=' | '-=' |
//         '<<=' | '>>=' | '>>>=' | '&=' | '^=' | '|=';
//     left: Expression;
//     right: Expression;
// }
#[derive(Serialize, Deserialize, PartialEq, Debug)]
#[serde(tag="type", rename="AssignmentExpression")]
pub struct AssignmentExpr {
    pub operator: AssignmentOp,
    pub left: Box<Expr>,
    pub right: Box<Expr>
}

// interface SequenceExpression {
//     type: 'SequenceExpression';
//     expressions: Expression[];
// }
#[derive(Serialize, Deserialize, PartialEq, Debug)]
#[serde(tag="type", rename="SequenceExpression")]
pub struct SequenceExpr {
    pub expressions: Vec<Expr>
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
pub enum UpdateOp {
    #[serde(rename="++")]
    Inc,
    #[serde(rename="--")]
    Dec
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
pub enum UnaryOp {
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

#[derive(Serialize, Deserialize, PartialEq, Debug)]
pub enum BinaryOp {
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
    #[serde(rename="**")]
    Exp,
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

#[derive(Serialize, Deserialize, PartialEq, Debug)]
pub enum LogicalOp {
    #[serde(rename="||")]
    Or,
    #[serde(rename="&&")]
    And,
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
pub enum AssignmentOp {
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

#[test]
fn test_expr_se_de() {
    check_se_de(Expr::This, json!({"type": "ThisExpression"}));

    check_se_de(Expr::Ident(Identifier{name: "test".into()}),
                json!({"type": "Identifier", "name": "test"}));

    check_se_de(Expr::Ident(Id::new("test")),
                json!({"type": "Identifier", "name": "test"}));

    check_se_de(Expr::Literal(Literal{value: LiteralKind::Bool(false),
                                      raw: "false".into(),
                                      regex: None}),
                json!({"type": "Literal", "value": false, "raw": "false"}));

    check_se_de(Expr::Literal(Lit::new_str("string")),
                json!({"type": "Literal", "value": "string", "raw": "string"}));

    check_se_de(Expr::Literal(Lit{value: LiteralKind::Num(1.0), raw: "1.0".into(), regex: None}),
                json!({"type": "Literal", "value": 1.0, "raw": "1.0"}));

    let int_lit_json = json!({"type": "Literal", "value": 2, "raw": "2"});
    let int_lit = serde_json::from_value::<Literal>(int_lit_json.clone()).unwrap();
    assert_eq!(serde_json::to_value(int_lit).unwrap(), int_lit_json);

    let float_lit_json = json!({"type": "Literal", "value": 2.0, "raw": "2.0"});
    let float_lit = serde_json::from_value::<Literal>(float_lit_json.clone()).unwrap();
    assert_eq!(serde_json::to_value(float_lit).unwrap(), float_lit_json);

    check_se_de(Expr::Literal(Lit{value: LiteralKind::Int(3), raw: "3".into(), regex: None}),
                json!({"type": "Literal", "value": 3, "raw": "3"}));

    check_se_de(Expr::Literal(Lit::new_f(2.0)),
                json!({"type": "Literal", "value": 2.0, "raw": "2.0"}));

    // TODO: implement custom parsing to seperate strins and regexs
    // check_se_de(Expr::Literal(Literal{value: LiteralKind::RegEx("/.*/g".into()),
    //                                   raw: "/.*/g".into(),
    //                                   regex: Some(LiteralRegex{pattern: ".*".into(),
    //                                                            flags: "g".into()})}),
    //             json!({"type": "Literal",
    //                     "value": "/.*/g",
    //                     "raw": "/.*/g",
    //                     "regex": {"pattern": ".*", "flags": "g"}}));

    check_se_de(Expr::Array(ArrayExpr{elements: vec![
                        ArrayExprElement::Expr(Expr::Literal(Lit::new_f(0.0)))
                    ]}),
                json!({"type": "ArrayExpression",
                        "elements": [ {
                                "type": "Literal",
                                "value": 0.0,
                                "raw": "0.0"
                            }]
                        }));

    check_se_de(Expr::Object(ObjectExpr{properties: vec![Property{
                                                    key: Expr::Ident(Id::new("ArrowRight")),
                                                    computed: false,
                                                    value: Some(Expr::This),
                                                    kind: PropertyKind::Init,
                                                    shorthand: false}]}),
                json!({"type": "ObjectExpression",
                        "properties": [
                        {
                            "type": "Property",
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

    // Arrow function expression
    check_se_de(Expr::ArrowFunc(ArrowFuncExpr{
                id: None,
                params: vec![ FunctionParam::Ident(Id::new("a")) ],
                body: ArrowFuncExprBody::Expr(Box::new(Expr::Update(UpdateExpr{
                    operator: UpdateOp::Inc,
                    argument: Box::new(Expr::Ident(Id::new("a"))),
                    prefix: false,
                }))),
                generator: false,
                async: false,
                expression: true,
            }),
        json!({
                "type": "ArrowFunctionExpression",
                "id": null,
                "params": [
                    {
                        "type": "Identifier",
                        "name": "a"
                    }
                ],
                "body": {
                    "type": "UpdateExpression",
                    "operator": "++",
                    "argument": {
                        "type": "Identifier",
                        "name": "a"
                    },
                    "prefix": false
                },
                "generator": false,
                "expression": true,
                "async": false
            }));

    check_se_de(Expr::Class(ClassExpr{
            id: None,
            super_class: None,
            body: ClassBody {
                body: vec![
                    MethodDef{
                        key: Some(Expr::Ident(Id::new("test"))),
                        computed: false,
                        value: Some(FunctionExpr{
                            id: None,
                            params: vec![],
                            body: BlockStmt{
                                body: vec![]
                            },
                            generator: false,
                            expression: false,
                            async: false,
                        }),
                        kind: MethodDefKind::Method,
                        stat: false,
                    }
                ]
            }
        }),
                json!({
            "type": "ClassExpression",
            "id": null,
            "superClass": null,
            "body": {
                "type": "ClassBody",
                "body": [
                    {
                        "type": "MethodDefinition",
                        "key": {
                            "type": "Identifier",
                            "name": "test"
                        },
                        "computed": false,
                        "value": {
                            "type": "FunctionExpression",
                            "id": null,
                            "params": [],
                            "body": {
                                "type": "BlockStatement",
                                "body": []
                            },
                            "generator": false,
                            "expression": false,
                            "async": false
                        },
                        "kind": "method",
                        "static": false
                    }
                ]
            }
        }));

    check_se_de(Expr::Member(MemberExpr{computed: false,
                                        object: Box::new(Expr::This),
                                        property: Box::new(Expr::Ident(Id::new("snake")))}),
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

    check_se_de(Expr::Call(CallExpr{
            callee: CallExprCallee::Expr(Box::new(Expr::Ident(Id::new("a")))),
            arguments: vec![
                ArgumentListElement::Expr(Box::new(Expr::Ident(Id::new("x")))),
                ArgumentListElement::Spread(SpreadElement{
                    argument: Box::new(Expr::Ident(Id::new("_")))
                })
            ]}),
                json!({
                "type": "CallExpression",
                "callee": {
                    "type": "Identifier",
                    "name": "a"
                },
                "arguments": [
                    {
                        "type": "Identifier",
                        "name": "x"
                    },
                    {
                        "type": "SpreadElement",
                        "argument": {
                            "type": "Identifier",
                            "name": "_"
                        }
                    }
                ]
            }));

    check_se_de(Expr::Update(UpdateExpr{operator: UpdateOp::Inc,
                                        argument: Box::new(Expr::This),
                                        prefix: false}),
                json!({"type": "UpdateExpression",
                        "operator": "++",
                        "argument": {"type": "ThisExpression"},
                        "prefix": false}));

    check_se_de(Expr::Unary(UnaryExpr{
            operator: UnaryOp::Delete,
            argument: Box::new(Expr::Ident(Id::new("x"))),
            prefix: true,
        }),json!({
            "type": "UnaryExpression",
            "operator": "delete",
            "argument": {
                "type": "Identifier",
                "name": "x"
            },
            "prefix": true
        }));

    check_se_de(Expr::Binary(BinaryExpr{
            operator: BinaryOp::And,
            left: Box::new(Expr::Ident(Id::new("a"))),
            right: Box::new(Expr::Ident(Id::new("b"))),
        }),json!({
            "type": "BinaryExpression",
            "operator": "&",
            "left": {
                "type": "Identifier",
                "name": "a"
            },
            "right": {
                "type": "Identifier",
                "name": "b"
            }
        }));

    check_se_de(Expr::Logical(LogicalExpr{
                operator: LogicalOp::And,
                left: Box::new(Expr::Ident(Id::new("a"))),
                right: Box::new(Expr::Ident(Id::new("b"))),
            }),
            json!({
            "type": "LogicalExpression",
            "operator": "&&",
            "left": {
                "type": "Identifier",
                "name": "a"
            },
            "right": {
                "type": "Identifier",
                "name": "b"
            }
        }));

    // Conditional expression
    check_se_de(Expr::Conditional(ConditionalExpr{
            test: Box::new(Expr::Ident(Id::new("a"))),
            consequent: Box::new(Expr::Literal(Lit::new_f(0.0))),
            alternate: Box::new(Expr::Literal(Lit::new_f(1.0)))
        }),
        json!({
                "type": "ConditionalExpression",
                "test": {
                    "type": "Identifier",
                    "name": "a"
                },
                "consequent": {
                    "type": "Literal",
                    "value": 0.0,
                    "raw": "0.0"
                },
                "alternate": {
                    "type": "Literal",
                    "value": 1.0,
                    "raw": "1.0"
                }
        }));

    // Assignment expression
    check_se_de(Expr::Assignment(AssignmentExpr{
            operator: AssignmentOp::Assign,
            left: Box::new(Expr::Ident(Identifier{name: "i".into()})),
            right: Box::new(Expr::Literal(Literal{
                value: LiteralKind::Int(0),
                raw: "0".into(),
                regex: None
            }))
        }),
        json!({
                "type": "AssignmentExpression",
                "operator": "=",
                "left": {
                    "type": "Identifier",
                    "name": "i"
                },
                "right": {
                    "type": "Literal",
                    "value": 0,
                    "raw": "0"
                }
            }));

    // Sequence expression
    check_se_de(Expr::Sequence(SequenceExpr{expressions: vec![
            Expr::Ident(Identifier{name: "a".into()}),
            Expr::Ident(Identifier{name: "b".into()}),
        ]}),
        json!({
                "type": "SequenceExpression",
                "expressions": [
                    {
                        "type": "Identifier",
                        "name": "a"
                    },
                    {
                        "type": "Identifier",
                        "name": "b"
                    }
                ]
        }));
}

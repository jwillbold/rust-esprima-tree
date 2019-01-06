#![allow(unused_imports)]
#![allow(dead_code)]
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

    // interface ArrayExpression {
    //     type: 'ArrayExpression';
    //     elements: ArrayExpressionElement[];
    // }
    #[serde(rename="ArrayExpression")]
    Array{elements: Vec<ArrayExprElement>},

    // interface ObjectExpression {
    //     type: 'ObjectExpression';
    //     properties: Property[];
    // }
    #[serde(rename="ObjectExpression")]
    Object{properties: Vec<Property>},

    #[serde(rename="FunctionExpression")]
    Function(FunctionExpr),

    #[serde(rename="ArrowFunctionExpression")]
    ArrowFunc(ArrowFuncExpr),

    #[serde(rename="ClassExpression")]
    Class(ClassExpr),

    // TODO test
    #[serde(rename="TaggedTemplateExpression")]
    TaggedTemplate(TaggedTemplateExpr),

    // interface MemberExpression {
    //     type: 'MemberExpression';
    //     computed: boolean;
    //     object: Expression;
    //     property: Expression;
    // }
    #[serde(rename="MemberExpression")]
    Member{
        computed: bool,
        object: Box<Expr>,
        property: Box<Expr>
    },

    Super,

    // TODO test
    // interface MetaProperty {
    //     type: 'MetaProperty';
    //     meta: Identifier;
    //     property: Identifier;
    // }
    #[serde(rename="MetaProperty")]
    MetaProperty{
        meta: Identifier,
        property: Identifier
    },

    // interface NewExpression {
    //     type: 'NewExpression';
    //     callee: Expression;
    //     arguments: ArgumentListElement[];
    // }
    #[serde(rename="NewExpression")]
    New{
        callee: Box<Expr>,
        arguments: Vec<ArgumentListElement>,
    },

    // interface CallExpression {
    //     type: 'CallExpression';
    //     callee: Expression | Import;
    //     arguments: ArgumentListElement[];
    // }
    #[serde(rename="CallExpression")]
    Call{
        callee: CallExprCallee,
        arguments: Vec<ArgumentListElement>
    },

    // interface UpdateExpression {
    //     type: 'UpdateExpression';
    //     operator: '++' | '--';
    //     argument: Expression;
    //     prefix: boolean;
    // }
    #[serde(rename="UpdateExpression")]
    Update{
        operator: UpdateOp,
        argument: Box<Expr>,
        prefix: bool
    },

     // TODO test
    // interface AwaitExpression {
    //     type: 'AwaitExpression';
    //     argument: Expression;
    // }
    #[serde(rename="AwaitExpression")]
    Await{
        argument: Box<Expr>
    },

    // interface UnaryExpression {
    //     type: 'UnaryExpression';
    //     operator: '+' | '-' | '~' | '!' | 'delete' | 'void' | 'typeof';
    //     argument: Expression;
    //     prefix: true;
    // }
    #[serde(rename="UnaryExpression")]
    Unary{
        operator: UnaryOp,
        argument: Box<Expr>,
        // TODO: true
        prefix: bool
    },

    // interface BinaryExpression {
    //     type: 'BinaryExpression';
    //     operator: 'instanceof' | 'in' | '+' | '-' | '*' | '/' | '%' | '**' |
    //         '|' | '^' | '&' | '==' | '!=' | '===' | '!==' |
    //         '<' | '>' | '<=' | '<<' | '>>' | '>>>';
    //     left: Expression;
    //     right: Expression;
    // }
    #[serde(rename="BinaryExpression")]
    Binary{
        operator: BinaryOp,
        left: Box<Expr>,
        right: Box<Expr>
    },

    // interface LogicalExpression {
    //     type: 'LogicalExpression';
    //     operator: '||' | '&&';
    //     left: Expression;
    //     right: Expression;
    // }
    #[serde(rename="LogicalExpression")]
    Logical{
        operator: LogicalOp,
        left: Box<Expr>,
        right: Box<Expr>
    },

    // interface ConditionalExpression {
    //     type: 'ConditionalExpression';
    //     test: Expression;
    //     consequent: Expression;
    //     alternate: Expression;
    // }
    #[serde(rename="ConditionalExpression")]
    Conditional{
        test: Box<Expr>,
        consequent: Box<Expr>,
        alternate: Box<Expr>
    },

    // TODO test
    // interface YieldExpression {
    //     type: 'YieldExpression';
    //     argument: Expression | null;
    //     delegate: boolean;
    // }
    #[serde(rename="YieldExpression")]
    Yield {
        argument: Option<Box<Expr>>,
        delegate: bool,
    },

    // interface AssignmentExpression {
    //     type: 'AssignmentExpression';
    //     operator: '=' | '*=' | '**=' | '/=' | '%=' | '+=' | '-=' |
    //         '<<=' | '>>=' | '>>>=' | '&=' | '^=' | '|=';
    //     left: Expression;
    //     right: Expression;
    // }
    #[serde(rename="AssignmentExpression")]
    Assignment{
        operator: AssignmentOp,
        left: Box<Expr>,
        right: Box<Expr>
    },

    // interface SequenceExpression {
    //     type: 'SequenceExpression';
    //     expressions: Expression[];
    // }
    #[serde(rename="SequenceExpression")]
    Sequence{
        expressions: Vec<Expr>
    }
}


#[derive(Serialize, Deserialize, PartialEq, Debug)]
pub struct Identifier {
    pub name: String
}

type Id = Identifier;

impl Identifier {
    fn new(s: &str) -> Self {
        Identifier{name: s.into()}
    }
}

pub fn serialize_ident_as_obj<S>(ident: &Identifier, s: S) -> Result<S::Ok, S::Error>
    where S: Serializer {
    let mut state = s.serialize_struct("Identifier", 2)?;
    state.serialize_field("type", "Identifier")?;
    state.serialize_field("name", &ident.name)?;
    state.end()
}

pub fn serialize_ident_as_opt_obj<S>(ident: &Option<Identifier>, s: S) -> Result<S::Ok, S::Error>
    where S: Serializer{
    match ident {
        Some(x) => serialize_ident_as_obj(x, s),
        None => s.serialize_none(),
    }
}

// fn serialize_as_opt<T, S>(f: &'static  &Fn(&T, S) -> Result<S::Ok, S::Error>)
//     -> impl Fn(&Option<T>, S) -> Result<S::Ok, S::Error>
//     where S: Serializer {
//     move |t: &Option<T>, s: S| -> Result<S::Ok, S::Error> {
//         match t {
//             Some(x) => f(x, s),
//             None => s.serialize_none()
//         }
//     }
// }



// interface Literal {
//     type: 'Literal';
//     value: boolean | number | string | RegExp | null;
//     raw: string;
//     regex?: { pattern: string, flags: string };
// }
#[derive(Serialize, Deserialize, PartialEq, Debug)]
pub struct Literal {
    pub value: LiteralKind,
    pub raw: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub regex: Option<LiteralRegex>
}

type Lit = Literal;

impl Literal {
    fn newf(f: f64) -> Self {
        Literal {
            value: LiteralKind::Num(f),
            raw: f.to_string(),
            regex: None,

        }
    }
}

// macro_rules! derive_private_shadow_struct {
//     (pub struct $si:ident {
//         $(pub $f:ident: $t:ty,)*
//     }, $tof:ident) => {
//         pub struct $si {
//             $(
//                 pub $f: $t
//             )*
//         }
//
//         pub fn $tof<S>(t: $si, s: S) -> Result<S::Ok, S::Error>
//             where S: Serializer {
//
//             #[derive(Serialize)]
//             #[serde(tag="type", rename="$si")]
//             struct PrivateShadowStruct<'a> {
//                 $(
//                     $f: &'a$t
//                 )*
//             }
//
//             PrivateShadowStruct {
//                 $(
//                     $f: &t.$f
//                 )*
//             }.serialize(s)
//         }
//     }
// }
//
// derive_private_shadow_struct!{
//     pub struct Test {
//         pub a: u64,
//     },
//     test_as_obj
// }


pub fn literal_as_obj<S>(lit: &Literal, s: S) -> Result<S::Ok, S::Error>
    where S: Serializer {
    #[derive(Serialize)]
    #[serde(tag="type", rename="Literal")]
    struct PrivateLiteral<'a> {
        pub value: &'a LiteralKind,
        pub raw: &'a String,
        #[serde(skip_serializing_if = "Option::is_none")]
        pub regex: &'a Option<LiteralRegex>
    }

    PrivateLiteral {
        value: &lit.value,
        raw: &lit.raw,
        regex: &lit.regex,
    }.serialize(s)
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
#[serde(untagged)]
pub enum LiteralKind {
    Bool(bool),
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

// type ArrayExpressionElement = Expression | SpreadElement;
#[derive(Serialize, Deserialize, PartialEq, Debug)]
#[serde(untagged)]
pub enum ArrayExprElement {
    Expr(Expr),
    Spread(SpreadElement)
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
pub struct FunctionExpr {
    #[serde(serialize_with="serialize_ident_as_opt_obj")]
    pub id: Option<Identifier>,
    pub params: Vec<FunctionParam>,
    #[serde(serialize_with="blockstmt_as_obj")]
    pub body: BlockStmt,
    pub generator: bool,
    pub async: bool,
    pub expression: bool,
}

fn funcexpr_as_obj<S>(func: &FunctionExpr, s: S) -> Result<S::Ok, S::Error>
    where S: Serializer {

    #[derive(Serialize)]
    #[serde(tag="type", rename="FunctionExpression")]
    struct FunctionExprShadow<'a> {
        #[serde(serialize_with="serialize_ident_as_opt_obj")]
        id: &'a Option<Identifier>,
        params: &'a Vec<FunctionParam>,
        #[serde(serialize_with="blockstmt_as_obj")]
        body: &'a BlockStmt,
        generator: &'a bool,
        async: &'a bool,
        expression: &'a bool,
    }

    FunctionExprShadow {
        id: &func.id,
        params: &func.params,
        body: &func.body,
        generator: &func.generator,
        async: &func.async,
        expression: &func.expression,
    }.serialize(s)
}

pub fn funcexpr_as_opt_obj<S>(f: &Option<FunctionExpr>, s: S) -> Result<S::Ok, S::Error>
    where S: Serializer {
    match f {
        Some(x) => funcexpr_as_obj(x, s),
        None => s.serialize_none()
    }
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
pub struct ArrowFuncExpr {
    #[serde(serialize_with="serialize_ident_as_opt_obj")]
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
#[derive(Serialize, Deserialize, PartialEq, Debug)]
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

#[derive(Serialize, Deserialize, PartialEq, Debug)]
#[serde(untagged)]
pub enum CallExprCallee {
    Expr(Box<Expr>),

    // interface Import {
    //     type: 'Import';
    // }
    Import,
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

    check_se_de(Expr::Array{elements: vec![
                        ArrayExprElement::Expr(
                            Expr::Literal(
                                Literal{value: LiteralKind::Num(0.0),
                                                raw: "0".into(),
                                                regex: None}
                    ))]},
                json!({"type": "ArrayExpression",
                        "elements": [ {
                                "type": "Literal",
                                "value": 0.0,
                                "raw": "0"
                            }]
                        }));

    check_se_de(Expr::Object{properties: vec![Property{
                                                    key: Expr::Ident(Identifier{
                                                    name: "ArrowRight".into()}),
                                                    computed: false,
                                                    value: Some(Expr::This),
                                                    kind: PropertyKind::Init,
                                                    shorthand: false}]},
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

    // Arrow function expression
    check_se_de(
        Stmt::Expr{expression: Expr::ArrowFunc(ArrowFuncExpr{
                id: None,
                params: vec![
                    FunctionParam::Ident(Identifier{name: "a".into()})
                ],
                body: ArrowFuncExprBody::Expr(Box::new(Expr::Update{
                    operator: UpdateOp::Inc,
                    argument: Box::new(Expr::Ident(Identifier{name: "a".into()})),
                    prefix: false,
                })),
                generator: false,
                async: false,
                expression: true,
            }),
            directive: None
        },
        json!({
            "type": "ExpressionStatement",
            "expression": {
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
            }
        }));

    // TODO
    // check_se(Expr::Class(ClassExpr{
    //         id: None,
    //         super_class: None,
    //         body: ClassBody {
    //             body: vec![
    //                 MethodDef{
    //                     key: Some(Expr::Ident(Id::new("test"))),
    //                     computed: false,
    //                     value: Some(FunctionExpr{
    //                         id: None,
    //                         params: vec![],
    //                         body: BlockStmt{
    //                             body: vec![]
    //                         },
    //                         generator: false,
    //                         expression: false,
    //                         async: false,
    //                     }),
    //                     kind: MethodDefKind::Method,
    //                     stat: false,
    //                 }
    //             ]
    //         }
    //     }),
    //     json!({
    //         "type": "ClassExpression",
    //         "id": null,
    //         "superClass": null,
    //         "body": {
    //             "type": "ClassBody",
    //             "body": [
    //                 {
    //                     "type": "MethodDefinition",
    //                     "key": {
    //                         "type": "Identifier",
    //                         "name": "test"
    //                     },
    //                     "computed": false,
    //                     "value": {
    //                         "type": "FunctionExpression",
    //                         "id": null,
    //                         "params": [],
    //                         "body": {
    //                             "type": "BlockStatement",
    //                             "body": []
    //                         },
    //                         "generator": false,
    //                         "expression": false,
    //                         "async": false
    //                     },
    //                     "kind": "method",
    //                     "static": false
    //                 }
    //             ]
    //         }
    //     }));

    check_se_de(Expr::Member{computed: false,
                                        object: Box::new(Expr::This),
                                        property: Box::new(Expr::Ident(Identifier{name: "snake".into()}))},
                json!({"type": "MemberExpression",
                            "computed": false,
                            "object": {
                                "type": "ThisExpression"
                            },
                            "property": {
                                "type": "Identifier",
                                "name": "snake"
                            }}));

    check_se_de(Expr::New{callee: Box::new(Expr::This), arguments: vec![]},
                json!({"type": "NewExpression",
                        "callee": {"type": "ThisExpression"},
                        "arguments": []}));

    check_se_de(Expr::Call{
            callee: CallExprCallee::Expr(Box::new(Expr::Ident(Id::new("a")))),
            arguments: vec![
                ArgumentListElement::Expr(Box::new(Expr::Ident(Id::new("x")))),
                ArgumentListElement::Spread(SpreadElement{
                    argument: Box::new(Expr::Ident(Id::new("_")))
                })
            ]
        },json!({
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

    check_se_de(Expr::Update{operator: UpdateOp::Inc,
                                        argument: Box::new(Expr::This),
                                        prefix: false},
                json!({"type": "UpdateExpression",
                        "operator": "++",
                        "argument": {"type": "ThisExpression"},
                        "prefix": false}));

    check_se_de(Expr::Unary{
            operator: UnaryOp::Delete,
            argument: Box::new(Expr::Ident(Id::new("x"))),
            prefix: true,
        },json!({
            "type": "UnaryExpression",
            "operator": "delete",
            "argument": {
                "type": "Identifier",
                "name": "x"
            },
            "prefix": true
        }));

    check_se_de(Expr::Binary{
            operator: BinaryOp::And,
            left: Box::new(Expr::Ident(Id::new("a"))),
            right: Box::new(Expr::Ident(Id::new("b"))),
        },json!({
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

    check_se_de(Expr::Logical{
            operator: LogicalOp::And,
            left: Box::new(Expr::Ident(Id::new("a"))),
            right: Box::new(Expr::Ident(Id::new("b"))),
        },json!({
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
    check_se_de(Expr::Conditional{
            test: Box::new(Expr::Ident(Id::new("a"))),
            consequent: Box::new(Expr::Literal(Lit::newf(0.0))),
            alternate: Box::new(Expr::Literal(Lit::newf(1.0)))
        },
        json!({
                "type": "ConditionalExpression",
                "test": {
                    "type": "Identifier",
                    "name": "a"
                },
                "consequent": {
                    "type": "Literal",
                    "value": 0.0, // TODO
                    "raw": "0"
                },
                "alternate": {
                    "type": "Literal",
                    "value": 1.0, // TODO
                    "raw": "1"
                }
        }));

    // Assignment expression
    check_se_de(Expr::Assignment{
            operator: AssignmentOp::Assign,
            left: Box::new(Expr::Ident(Identifier{name: "i".into()})),
            right: Box::new(Expr::Literal(Literal{
                value: LiteralKind::Num(0.0),
                raw: "0.0".into(),
                regex: None
            }))
        },
        json!({
                "type": "AssignmentExpression",
                "operator": "=",
                "left": {
                    "type": "Identifier",
                    "name": "i"
                },
                "right": {
                    "type": "Literal",
                    "value": 0.0, // TODO this hould be just 0 not 0.0
                    "raw": "0.0"
                }
            }));

    // Sequence expression
    check_se_de(Expr::Sequence{expressions: vec![
            Expr::Ident(Identifier{name: "a".into()}),
            Expr::Ident(Identifier{name: "b".into()}),
        ]},
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
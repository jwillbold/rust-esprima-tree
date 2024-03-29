use patterns::{IdentOrPattern};
use statements::*;
use expressions::*;
#[cfg(test)]
use helpers::{check_se_de};

// type Declaration = ClassDeclaration | FunctionDeclaration |  VariableDeclaration;
#[derive(Serialize, Deserialize, PartialEq, Debug)]
// #[serde(untagged)]
#[serde(tag="type")]
pub enum Decl {
    #[serde(rename="ClassDeclaration")]
    Class(ClassDecl),
    #[serde(rename="FunctionDeclaration")]
    Function(FunctionDecl),
    #[serde(rename="VariableDeclaration")]
    Variable(VariableDecl),
}

// interface ClassDeclaration {
//     type: 'ClassDeclaration';
//     id: Identifier | null;
//     superClass: Identifier | null;
//     body: ClassBody;
// }
#[derive(Serialize, Deserialize, PartialEq, Debug)]
#[serde(tag="type", rename="ClassDeclaration")]
pub struct ClassDecl {
    pub id: Option<Identifier>,
    #[serde(rename="superClass")]
    pub super_class: Option<Identifier>,
    pub body: ClassBody,
}

// interface ClassBody {
//     type: 'ClassBody';
//     body: MethodDefinition[];
// }
#[derive(Serialize, Deserialize, PartialEq, Debug)]
#[serde(tag="type")]
pub struct ClassBody {
    pub body: Vec<MethodDef>
}

// interface MethodDefinition {
//     type: 'MethodDefinition';
//     key: Expression | null;
//     computed: boolean;
//     value: FunctionExpression | null;
//     kind: 'method' | 'constructor';
//     static: boolean;
// }
#[derive(Serialize, Deserialize, PartialEq, Debug)]
#[serde(tag="type", rename="MethodDefinition")]
pub struct MethodDef {
    pub key: Option<Expr>,
    pub computed: bool,
    pub value: Option<FunctionExpr>,
    pub kind: MethodDefKind,
    #[serde(rename="static")]
    pub stat: bool
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
pub enum MethodDefKind {
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
#[serde(tag="type", rename="FunctionDeclaration")]
pub struct FunctionDecl {
    pub id: Option<Identifier>,
    pub params: Vec<FunctionParam>,
    pub body: BlockStmt,
    pub generator: bool,
    pub async: bool,
    pub expression: bool, // TODO This must be constant false
}

impl FunctionDecl {
    pub fn new(id: &str, params: Vec<FunctionParam>, body: Vec<StmtListItem>) -> Self {
        FunctionDecl {
            id: match id {
                "" => None,
                _ => Some(Id::new(id))
            },
            params,
            body: BlockStmt {
                body,
            },
            generator: false,
            async: false,
            expression: false
        }
    }
}

// interface VariableDeclaration {
//     type: 'VariableDeclaration';
//     declarations: VariableDeclarator[];
//     kind: 'var' | 'const' | 'let';
// }
#[derive(Serialize, Deserialize, PartialEq, Debug)]
#[serde(tag="type", rename="VariableDeclaration")]
pub struct VariableDecl {
    pub declarations: Vec<VariableDeclarator>,
    pub kind: VariableDeclKind,
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
pub enum VariableDeclKind {
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
#[serde(tag="type")]
pub struct VariableDeclarator {
    pub id: IdentOrPattern,
    pub init: Option<Expr>
}

#[test]
fn tetst_decl_se_de() {
    check_se_de(Decl::Class(ClassDecl{id: Some(Identifier{name: "TestClass".into()}),
         super_class: None,
         body: ClassBody{body: vec![
             MethodDef {
                key: Some(Expr::Ident(Identifier{name: "func1".into()})),
                computed: false,
                value: Some(FunctionExpr {
                    id: None,
                    params: vec![],
                    body: BlockStmt{body: vec![]},
                    generator: false,
                    expression: false,
                    async: false,
                }),
                kind: MethodDefKind::Method,
                stat: false
             }
         ]}}),
                json!({"type": "ClassDeclaration",
                        "id": {
                            "type": "Identifier",
                            "name": "TestClass"
                        },
                        "superClass": null,
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

    check_se_de(Decl::Function(FunctionDecl{
            id: Some(Identifier{name: "empty".into()}),
            params: vec![
                FunctionParam::Ident(Identifier{name: "a".into()})
            ],
            body: BlockStmt{body: vec![
                StmtListItem::Stmt(Stmt::Return(ReturnStmt{
                    argument: Some(Expr::Ident(Identifier{name: "a".into()}))
                }))
            ]},
            generator: false,
            expression: false,
            async: false
        }),
        json!({
                "type": "FunctionDeclaration",
                "id": {
                    "type": "Identifier",
                    "name": "empty"
                },
                "params": [
                    {
                        "type": "Identifier",
                        "name": "a"
                    }
                ],
                "body": {
                    "type": "BlockStatement",
                    "body": [
                        {
                            "type": "ReturnStatement",
                            "argument": {
                                "type": "Identifier",
                                "name": "a"
                            }
                        }
                    ]
                },
                "generator": false,
                "expression": false,
                "async": false
            }));

    check_se_de(Decl::Variable(VariableDecl{
            declarations: vec![
                VariableDeclarator{id: IdentOrPattern::Ident(Identifier{name: "row".into()}),
                                    init: Some(Expr::Literal(Literal{value:LiteralKind::Num(0.0),
                                                                    raw: "0.0".into(),
                                                                    regex: None}))}
                ],
            kind: VariableDeclKind::Let,
        }),
        json!({
            "type": "VariableDeclaration",
            "declarations": [
                {
                    "type": "VariableDeclarator",
                    "id": {
                        "type": "Identifier",
                        "name": "row"
                    },
                    "init": {
                        "type": "Literal",
                        "value": 0.0,
                        "raw": "0.0"
                    }
                }
            ],
            "kind": "let"
        }));
}

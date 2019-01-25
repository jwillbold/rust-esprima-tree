#![allow(unused_imports)]
#![allow(dead_code)]
use serde::ser::{Serialize, Serializer, SerializeStruct};

use declerations::*;
use patterns::*;
use expressions::*;
#[cfg(test)]
use helpers::{check_se_de};

// type Statement = BlockStatement | BreakStatement | ContinueStatement |
//     DebuggerStatement | DoWhileStatement | EmptyStatement |
//     ExpressionStatement | ForStatement | ForInStatement |
//     ForOfStatement | FunctionDeclaration | IfStatement |
//     LabeledStatement | ReturnStatement | SwitchStatement |
//     ThrowStatement | TryStatement | VariableDeclaration |
//     WhileStatement | WithStatement;
#[derive(Serialize, Deserialize, PartialEq, Debug)]
#[serde(tag="type")]
pub enum Stmt {
    #[serde(rename="BlockStatement")]
    Block(BlockStmt),

    #[serde(rename="BreakStatement")]
    Break(BreakStmt),

    #[serde(rename="ContinueStatement")]
    Continue(ContinueStmt),

    // TODO tests
    // interface DebuggerStatement {
    //     type: 'DebuggerStatement';
    // }
    #[serde(rename="DebuggerStatement")]
    Debugger,

    #[serde(rename="DoWhileStatement")]
    DoWhile(DoWhileStmt),

    // interface EmptyStatement {
    //     type: 'EmptyStatement';
    // }
    #[serde(rename="EmptyStatement")]
    Empty,

    #[serde(rename="ExpressionStatement")]
    Expr(ExprStmt),

    #[serde(rename="ForStatement")]
    For(ForStmt),

    #[serde(rename="ForInStatement")]
    ForIn(ForInStmt),

    #[serde(rename="ForOfStatement")]
    ForOf(ForOfStmt),

    #[serde(rename="FunctionDeclaration")]
    Function(FunctionDecl),

    #[serde(rename="IfStatement")]
    If(IfStmt),

    #[serde(rename="LabeledStatement")]
    Labled(LabeledStmt),

    #[serde(rename="ReturnStatement")]
    Return(ReturnStmt),

    #[serde(rename="SwitchStatement")]
    Switch(SwitchStmt),

    #[serde(rename="ThrowStatement")]
    Throw(ThrowStmt),

    #[serde(rename="TryStatement")]
    Try(TryStmt),

    #[serde(rename="VariableDeclaration")]
    VarDecl(VariableDecl),

    #[serde(rename="WhileStatement")]
    While(WhileStmt),

    #[serde(rename="WithStatement")]
    With(WithStmt),
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
#[serde(tag="type", rename="BlockStatement")]
pub struct BlockStmt {
    pub body: Vec<StmtListItem>
}

// type StatementListItem = Declaration | Statement;
#[derive(Serialize, Deserialize, PartialEq, Debug)]
#[serde(untagged)]
pub enum StmtListItem {
    Decl(Decl),
    Stmt(Stmt),
}

// interface BreakStatement {
//     type: 'BreakStatement';
//     label: Identifier | null;
// }
#[derive(Serialize, Deserialize, PartialEq, Debug)]
#[serde(tag="type", rename="BreakStatement")]
pub struct BreakStmt {
    pub label: Option<Identifier>
}

// interface ContinueStatement {
//     type: 'ContinueStatement';
//     label: Identifier | null;
// }
#[derive(Serialize, Deserialize, PartialEq, Debug)]
#[serde(tag="type", rename="ContinueStatement")]
pub struct ContinueStmt {
    pub label: Option<Identifier>
}

// interface DoWhileStatement {
//     type: 'DoWhileStatement';
//     body: Statement;
//     test: Expression;
// }
#[derive(Serialize, Deserialize, PartialEq, Debug)]
#[serde(tag="type", rename="DoWhileStatement")]
pub struct DoWhileStmt {
    pub body: Box<Stmt>,
    pub test: Expr
}

// interface ExpressionStatement {
//     type: 'ExpressionStatement';
//     expression: Expression;
//     directive?: string;
// }
#[derive(Serialize, Deserialize, PartialEq, Debug)]
#[serde(tag="type", rename="ExpressionStatement")]
pub struct ExprStmt {
    expression: Expr,
    #[serde(skip_serializing_if = "Option::is_none")]
    directive: Option<String>
}

// interface ForStatement {
//     type: 'ForStatement';
//     init: Expression | VariableDeclaration | null;
//     test: Expression | null;
//     update: Expression | null;
//     body: Statement;
// }
#[derive(Serialize, Deserialize, PartialEq, Debug)]
#[serde(tag="type", rename="ForStatement")]
pub struct ForStmt {
    pub init: Option<FotStmtInit>,
    pub test: Option<Expr>,
    pub update: Option<Expr>,
    pub body: Box<Stmt>
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
#[serde(untagged)]
pub enum FotStmtInit {
    Expr(Expr),
    VarDecl(VariableDecl),
}

// interface ForInStatement {
//     type: 'ForInStatement';
//     left: Expression;
//     right: Expression;
//     body: Statement;
//     each: false; // TODO this must be constant false
// }
#[derive(Serialize, Deserialize, PartialEq, Debug)]
#[serde(tag="type", rename="ForInStatement")]
pub struct ForInStmt {
    pub left: Expr,
    pub right: Expr,
    pub body: Box<Stmt>,
    pub each: bool
}

// interface ForOfStatement {
//     type: 'ForOfStatement';
//     left: Expression;
//     right: Expression;
//     body: Statement;
// }
#[derive(Serialize, Deserialize, PartialEq, Debug)]
#[serde(tag="type", rename="ForOfStatement")]
pub struct ForOfStmt {
    pub left: Expr,
    pub right: Expr,
    pub body: Box<Stmt>
}

// interface IfStatement {
//     type: 'IfStatement';
//     test: Expression;
//     consequent: Statement;
//     alternate?: Statement;
// }
#[derive(Serialize, Deserialize, PartialEq, Debug)]
#[serde(tag="type", rename="IfStatement")]
pub struct IfStmt {
    pub test: Expr,
    pub consequent: Box<Stmt>,
    pub alternate: Option<Box<Stmt>>
}

//  interface LabeledStatement {
//     type: 'LabeledStatement';
//     label: Identifier;
//     body: Statement;
// }
#[derive(Serialize, Deserialize, PartialEq, Debug)]
#[serde(tag="type", rename="LabeledStatement")]
pub struct LabeledStmt {
    pub label: Identifier,
    pub body: Box<Stmt>
}

// interface ReturnStatement {
//  type: 'ReturnStatement';
//  argument: Expression | null;
// }
#[derive(Serialize, Deserialize, PartialEq, Debug)]
#[serde(tag="type", rename="ReturnStatement")]
pub struct ReturnStmt {
    pub argument: Option<Expr>
}

// interface SwitchStatement {
//     type: 'SwitchStatement';
//     discriminant: Expression;
//     cases: SwitchCase[];
// }
#[derive(Serialize, Deserialize, PartialEq, Debug)]
#[serde(tag="type", rename="SwitchStatement")]
pub struct SwitchStmt {
    discriminant: Expr,
    cases: Vec<SwitchCase>
}

// interface SwitchCase {
//     type: 'SwitchCase';
//     test: Expression | null;
//     consequent: Statement[];
// }
#[derive(Serialize, Deserialize, PartialEq, Debug)]
#[serde(tag="type")]
pub struct SwitchCase {
    pub test: Option<Expr>,
    pub consequent: Vec<Stmt>
}

// interface CatchClause {
//     type: 'CatchClause';
//     param: Identifier | BindingPattern;
//     body: BlockStatement;
// }
#[derive(Serialize, Deserialize, PartialEq, Debug)]
#[serde(tag="type")]
pub struct CatchClause {
    pub param: IdentOrPattern,
    pub body: BlockStmt,
}

// interface ThrowStatement {
//     type: 'ThrowStatement';
//     argument: Expression;
// }
#[derive(Serialize, Deserialize, PartialEq, Debug)]
pub struct ThrowStmt {
    pub argument: Expr
}

// interface TryStatement {
//     type: 'TryStatement';
//     block: BlockStatement;
//     handler: CatchClause | null;
//     finalizer: BlockStatement | null;
// }
#[derive(Serialize, Deserialize, PartialEq, Debug)]
#[serde(tag="type", rename="TryStatement")]
pub struct TryStmt {
    pub block: BlockStmt,
    pub handler: Option<CatchClause>,
    pub finalizer: Option<BlockStmt>
}

// interface WhileStatement {
//     type: 'WhileStatement';
//     test: Expression;
//     body: Statement;
// }
#[derive(Serialize, Deserialize, PartialEq, Debug)]
#[serde(tag="type", rename="WhileStatement")]
pub struct WhileStmt {
    pub test: Expr,
    pub body: Box<Stmt>
}

// interface WithStatement {
//     type: 'WithStatement';
//     object: Expression;
//     body: Statement;
// }
#[derive(Serialize, Deserialize, PartialEq, Debug)]
#[serde(tag="type", rename="WithStatement")]
pub struct WithStmt {
    pub object: Expr,
    pub body: Box<Stmt>
}


#[test]
fn test_stmt_se_de() {
    // Empty block statment
    check_se_de(Stmt::Block(BlockStmt{body: vec![]}),
                json!({"type": "BlockStatement", "body": []}));

    // Single instruction block stmt
    check_se_de(Stmt::Block(BlockStmt{body: vec![
                    StmtListItem::Stmt(Stmt::Return(ReturnStmt{
                        argument: Some(Expr::Ident(Identifier{name: "a".into()}))
                    }))
                ]}),
                json!({
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
                    }));

    // Break stmt without label
    check_se_de(Stmt::Break(BreakStmt{label: None}),
                json!({"type": "BreakStatement", "label": serde_json::Value::Null}));

    // Break stmt with label
    check_se_de(Stmt::Break(BreakStmt{label: Some(Identifier{name: "lbl".into()})}),
                json!({
                        "type": "BreakStatement",
                        "label": {
                            "type": "Identifier",
                            "name": "lbl"
                        }
                    }));

    // Continue stmt without label
    check_se_de(Stmt::Continue(ContinueStmt{label: None}),
                json!({"type": "ContinueStatement", "label": serde_json::Value::Null}));

    // Continue stmt with label
    check_se_de(Stmt::Continue(ContinueStmt{label: Some(Identifier{name: "lbl".into()})}),
                json!({
                        "type": "ContinueStatement",
                        "label": {
                            "type": "Identifier",
                            "name": "lbl"
                        }
                    }));

    // DoWhile stmt and expression stmt
    check_se_de(Stmt::DoWhile(DoWhileStmt{body: Box::new(Stmt::Block(BlockStmt{body:vec![
                                    StmtListItem::Stmt(Stmt::Expr(ExprStmt{
                                            expression: Expr::Update(UpdateExpr{
                                                operator: UpdateOp::Dec,
                                                argument: Box::new(Expr::Ident(
                                                            Identifier{name: "a".into()})
                                                            ),
                                                prefix: false
                                            }),
                                            directive: None,
                                        }))
                                ]})),
                              test: Expr::Ident(Identifier::new("a"))
                }),
                json!({
                    "type": "DoWhileStatement",
                    "body": {
                        "type": "BlockStatement",
                        "body": [
                            {
                                "type": "ExpressionStatement",
                                "expression": {
                                    "type": "UpdateExpression",
                                    "operator": "--",
                                    "argument": {
                                        "type": "Identifier",
                                        "name": "a"
                                    },
                                    "prefix": false
                                }
                            }
                        ]
                    },
                    "test": {
                        "type": "Identifier",
                        "name": "a"
                    }
        }));

    check_se_de(Stmt::If(IfStmt{
                    test: Expr::Unary(UnaryExpr{
                        operator: UnaryOp::Not,
                        argument: Box::new(Expr::Ident(Id::new("l2"))),
                        prefix: true,
                    }),
                    consequent: Box::new(Stmt::Return(ReturnStmt{
                        argument: Some(Expr::Call(CallExpr{
                            callee: CallExprCallee::Expr(Box::new(Expr::Ident(Id::new("a")))),
                            arguments: vec![
                                ArgumentListElement::Expr(Box::new(Expr::Ident(Id::new("p"))))
                            ]
                        }))
                    })),
                    alternate: None
                }),
                json!({
                      "type": "IfStatement",
                      "test": {
                          "type": "UnaryExpression",
                          "operator": "!",
                          "argument": {
                              "type": "Identifier",
                              "name": "l2"
                          },
                          "prefix": true
                      },
                      "consequent": {
                          "type": "ReturnStatement",
                          "argument": {
                              "type": "CallExpression",
                              "callee": {
                                  "type": "Identifier",
                                  "name": "a"
                              },
                              "arguments": [
                                  {
                                      "type": "Identifier",
                                      "name": "p"
                                  },
                              ]
                          }
                      },
                      "alternate": null
                  }));
}

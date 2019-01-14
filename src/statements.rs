#![allow(unused_imports)]
#![allow(dead_code)]
use serde::ser::{Serialize, Serializer, SerializeStruct};
// use serde::de::{Deserialize, Deserializer};

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

    // interface BreakStatement {
    //     type: 'BreakStatement';
    //     label: Identifier | null;
    // }
    #[serde(rename="BreakStatement")]
    Break {
        #[serde(serialize_with="ident_as_opt_obj")]
        label: Option<Identifier>
    },

    // interface ContinueStatement {
    //     type: 'ContinueStatement';
    //     label: Identifier | null;
    // }
    #[serde(rename="ContinueStatement")]
    Continue {
        #[serde(serialize_with="ident_as_opt_obj")]
        label: Option<Identifier>
    },

    // TODO tests
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
        #[serde(skip_serializing_if = "Option::is_none")]
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
         #[serde(serialize_with="ident_as_obj")]
         label: Identifier,
         body: Box<Stmt>},

    // interface ReturnStatement {
    //  type: 'ReturnStatement';
    //  argument: Expression | null;
    // }
    #[serde(rename="ReturnStatement")]
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
        // #[serde(serialize_with="blockstmt_as_obj")] // TODO
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
pub enum FotStmtInit {
    Expr(Expr),
    // TODO as obj
    VarDecl(VariableDecl),
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
    #[serde(serialize_with="blockstmt_as_obj")]
    pub body: BlockStmt,
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
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

pub fn blockstmt_as_obj<S>(block: &BlockStmt, s: S) -> Result<S::Ok, S::Error>
    where S: Serializer {
    let mut state = s.serialize_struct("BlockStmt", 2)?;
    state.serialize_field("type", "BlockStatement")?;
    state.serialize_field("body", &block.body)?;
    state.end()
}


#[test]
fn test_stmt_se_de() {
    // Empty block statment
    check_se_de(Stmt::Block(BlockStmt{body: vec![]}),
                json!({"type": "BlockStatement", "body": []}));

    // Single instruction block stmt
    check_se_de(Stmt::Block(BlockStmt{body: vec![
                    StmtListItem::Stmt(Stmt::Return{
                        argument: Some(Expr::Ident(Identifier{name: "a".into()}))
                    })
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
    check_se_de(Stmt::Break{label: None},
                json!({"type": "BreakStatement", "label": serde_json::Value::Null}));

    // Break stmt with label
    check_se_de(Stmt::Break{label: Some(Identifier{name: "lbl".into()})},
                json!({
                        "type": "BreakStatement",
                        "label": {
                            "type": "Identifier",
                            "name": "lbl"
                        }
                    }));

    // Continue stmt without label
    check_se_de(Stmt::Continue{label: None},
                json!({"type": "ContinueStatement", "label": serde_json::Value::Null}));

    // Continue stmt with label
    check_se_de(Stmt::Continue{label: Some(Identifier{name: "lbl".into()})},
                json!({
                        "type": "ContinueStatement",
                        "label": {
                            "type": "Identifier",
                            "name": "lbl"
                        }
                    }));

    // DoWhile stmt and expression stmt
    check_se_de(Stmt::DoWhile{body: Box::new(Stmt::Block(BlockStmt{body:vec![
                                    StmtListItem::Stmt(Stmt::Expr{
                                            expression: Expr::Update{
                                                operator: UpdateOp::Dec,
                                                argument: Box::new(Expr::Ident(
                                                            Identifier{name: "a".into()})
                                                            ),
                                                prefix: false
                                            },
                                            directive: None,
                                        })
                                ]})),
                              test: Expr::Ident(Identifier{name: "a".into()})
                },
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

    check_se_de(Stmt::If{
                    test: Expr::Unary{
                        operator: UnaryOp::Not,
                        argument: Box::new(Expr::Ident(Id::new("l2"))),
                        prefix: true,
                    },
                    consequent: Box::new(Stmt::Return{
                        argument: Some(Expr::Call{
                            callee: CallExprCallee::Expr(Box::new(Expr::Ident(Id::new("a")))),
                            arguments: vec![
                                ArgumentListElement::Expr(Box::new(Expr::Ident(Id::new("p"))))
                            ]
                        })
                    }),
                    alternate: None
                },
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

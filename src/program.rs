use serde::ser::{Serializer, SerializeStruct};

use declerations::{Decl, ClassDecl, FunctionDecl};
use patterns::{BindingPattern};
use statements::{StmtListItem};
use expressions::{Expr, Literal, literal_as_obj, Identifier,
                    serialize_ident_as_obj, serialize_ident_as_opt_obj};
#[cfg(test)]
use helpers::{check_se_de};
#[cfg(test)]
use statements::{BlockStmt, *};
#[cfg(test)]
use expressions::{LiteralKind, *};
#[cfg(test)]
use declerations::*;
#[cfg(test)]
use patterns::*;

// interface Program {
//   type: 'Program';
//   sourceType: 'script';
//   body: StatementListItem[];
// }
//
// interface Program {
//   type: 'Program';
//   sourceType: 'module';
//   body: ModuleItem[];
// }
#[derive(Deserialize, PartialEq, Debug)]
pub struct Program {
    pub body: ProgramType
}

impl serde::Serialize for Program {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error> where S: Serializer {
        let mut state = serializer.serialize_struct("Program", 3)?;
        state.serialize_field("type", "Program")?;
        state.serialize_field("sourceType", match self.body {
            ProgramType::Script(_) => "script",
            ProgramType::Module(_) => "module",
        })?;
        state.serialize_field("body", &self.body)?;
        state.end()
    }
}


#[derive(Serialize, Deserialize, PartialEq, Debug)]
#[serde(untagged)]
pub enum ProgramType {
    Script(Vec<StmtListItem>),
    Module(Vec<ModuleItem>)
}

// type ModuleItem = ImportDeclaration | ExportDeclaration | StatementListItem;
#[derive(Serialize, Deserialize, PartialEq, Debug)]
#[serde(untagged)]
pub enum ModuleItem {
    Import(ImportDecl),
    Export(ExportDecl),
    Stmts(StmtListItem)
}

// type ImportDeclaration {
//     type: 'ImportDeclaration';
//     specifiers: ImportSpecifier[];
//     source: Literal;
// }
#[derive(Serialize, Deserialize, PartialEq, Debug)]
#[serde(tag="type", rename="ImportDeclaration")]
pub struct ImportDecl {
    pub specifiers: Vec<ImportSpecifier>,
    #[serde(serialize_with="literal_as_obj")]
    pub source: Literal
}

// interface ImportSpecifier {
//     type: 'ImportSpecifier' | 'ImportDefaultSpecifier' | 'ImportNamespaceSpecifier';
//     local: Identifier;
//     imported?: Identifier;
// }
#[derive(Serialize, Deserialize, PartialEq, Debug)]
pub struct ImportSpecifier {
    #[serde(rename="type")]
    pub ty: ImportSpecifierKind,
    #[serde(serialize_with="serialize_ident_as_obj")]
    pub local: Identifier,
    #[serde(skip_serializing_if="Option::is_none")]
    #[serde(serialize_with="serialize_ident_as_opt_obj")]
    pub imported: Option<Identifier>
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
pub enum ImportSpecifierKind {
    ImportSpecifier,
    ImportDefaultSpecifier,
    ImportNamespaceSpecifier
}

// type ExportDeclaration = ExportAllDeclaration | ExportDefaultDeclaration | ExportNamedDeclaration;
#[derive(Serialize, Deserialize, PartialEq, Debug)]
#[serde(tag="type")]
pub enum ExportDecl {
    // interface ExportAllDeclaration {
    //     type: 'ExportAllDeclaration';
    //     source: Literal;
    // }
    #[serde(rename="ExportAllDeclaration")]
    ExportAll{
        #[serde(serialize_with="literal_as_obj")]
        source: Literal
    },


    // interface ExportDefaultDeclaration {
    //     type: 'ExportDefaultDeclaration';
    //     declaration: Identifier | BindingPattern | ClassDeclaration | Expression | FunctionDeclaration;
    // }
    #[serde(rename="ExportDefaultDeclaration")]
    ExportDefault{
        declaration: ExportDefaultDeclKind
    },


    // interface ExportNamedDeclaration {
    //     type: 'ExportNamedDeclaration';
    //     declaration: ClassDeclaration | FunctionDeclaration | VariableDeclaration;
    //     specifiers: ExportSpecifier[];
    //     source: Literal;
    // }
    #[serde(rename="ExportNamedDeclaration")]
    ExportNamed{declaration: Decl,
                specifiers: Vec<ExportSpecifier>,
                // #[serde(serialize_with="literal_as_obj")]
                // TODO
                source: Option<Literal>
    }
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
#[serde(tag = "type")]
pub enum ExportDefaultDeclKind {
    Ident(Identifier),
    Binding(BindingPattern),
    Class(ClassDecl),
    // TODO this will produce as doubled type tag
    Expr(Expr),
    Function(FunctionDecl)
}

// interface ExportSpecifier {
//     type: 'ExportSpecifier';
//     exported: Identifier;
//     local: Identifier;
// };
#[derive(Serialize, Deserialize, PartialEq, Debug)]
#[serde(tag="type")]
pub struct ExportSpecifier {
    #[serde(serialize_with="serialize_ident_as_obj")]
    pub exported: Identifier,
    #[serde(serialize_with="serialize_ident_as_obj")]
    pub local: Identifier
}

#[test]
fn test_progman_se_de() {
    check_se_de(Program{body: ProgramType::Script(vec![])},
                json!({"type": "Program",
                        "sourceType": "script",
                        "body": []}));

    check_se_de(Program{body: ProgramType::Script(vec![
                    StmtListItem::Decl(Decl::Variable(VariableDecl{
                        declarations: vec![
                            VariableDeclarator{
                                id: IdentOrPattern::Ident(Id::new("canvas")),
                                init: None
                            }
                        ],
                        kind: VariableDeclKind::Var
                    }))
                ])},
                json!({"type": "Program",
                        "sourceType": "script",
                        "body": [
                        {
                            "type": "VariableDeclaration",
                            "declarations": [
                                {
                                    "type": "VariableDeclarator",
                                    "id": {
                                        "type": "Identifier",
                                        "name": "canvas"
                                    },
                                    "init": null
                                }
                            ],
                            "kind": "var"
                        }
                        ]}));

    // TODO
    // check_se_de(Program{body: ProgramType::Module(vec![])},
    //             json!({"type": "Program",
    //                     "sourceType": "module",
    //                     "body": []}));

    check_se_de(Program{body: ProgramType::Module(vec![
                    ModuleItem::Import(ImportDecl{specifiers: vec![],
                                       source: Literal{
                                            value: LiteralKind::Str("mypackage.js".into()),
                                            raw: "\'mypackage.js\'".into(),
                                            regex: None }
                        }),
                ])},
                json!({
                    "type": "Program",
                    "body": [
                        {
                            "type": "ImportDeclaration",
                            "specifiers": [],
                            "source": {
                                "type": "Literal",
                                "value": "mypackage.js",
                                "raw": "'mypackage.js'"
                            }
                        },
                    ],
                    "sourceType": "module"
                }));

    check_se_de(Program{body: ProgramType::Module(vec![
                    ModuleItem::Import(ImportDecl{specifiers: vec![
                                        ImportSpecifier{
                                            ty: ImportSpecifierKind::ImportDefaultSpecifier,
                                            local: Identifier{
                                                    name: "defaults".into()},
                                            imported: None}],
                                        source: Literal {
                                            value: LiteralKind::Str("mypackage.js".into()),
                                            raw: "\'mypackage.js\'".into(),
                                            regex: None }
                        }),
                ])},
                json!({
                    "type": "Program",
                    "body": [
                        {
                            "type": "ImportDeclaration",
                            "specifiers": [
                                {
                                    "type": "ImportDefaultSpecifier",
                                    "local": {
                                        "type": "Identifier",
                                        "name": "defaults"
                                    }
                                }
                            ],
                            "source": {
                                "type": "Literal",
                                "value": "mypackage.js",
                                "raw": "'mypackage.js'"
                            }
                        },
                    ],
                    "sourceType": "module"
                }));

    check_se_de(Program{body: ProgramType::Module(vec![
                    ModuleItem::Import(ImportDecl{specifiers: vec![
                                        ImportSpecifier{
                                            ty: ImportSpecifierKind::ImportSpecifier,
                                            local: Identifier{
                                                    name: "athing".into()
                                            },
                                            imported: Some(Identifier{
                                                name: "athing".into()
                                            })}],
                                        source: Literal{
                                            value: LiteralKind::Str("mypackage.js".into()),
                                            raw: "\'mypackage.js\'".into(),
                                            regex: None}
                        }),
                    ])},
                    json!({
                            "type": "Program",
                            "body": [
                                {
                                    "type": "ImportDeclaration",
                                    "specifiers": [
                                        {
                                            "type": "ImportSpecifier",
                                            "local": {
                                                "type": "Identifier",
                                                "name": "athing"
                                            },
                                            "imported": {
                                                "type": "Identifier",
                                                "name": "athing"
                                            }
                                        }
                                    ],
                                    "source": {
                                        "type": "Literal",
                                        "value": "mypackage.js",
                                        "raw": "'mypackage.js'"
                                    }
                                }
                            ],
                            "sourceType": "module"
                        }));

    check_se_de(Program{body: ProgramType::Module(vec![
                        ModuleItem::Import(ImportDecl{specifiers: vec![
                                            ImportSpecifier{
                                                ty: ImportSpecifierKind::ImportNamespaceSpecifier,
                                                local: Identifier{
                                                        name: "ns".into()},
                                                imported: None}],
                                            source: Literal{
                                                value: LiteralKind::Str("mypackage.js".into()),
                                                raw: "\'mypackage.js\'".into(),
                                                regex: None}
                            }),
                    ])},
                    json!({
                            "type": "Program",
                            "body": [
                                {
                                    "type": "ImportDeclaration",
                                    "specifiers": [
                                        {
                                            "type": "ImportNamespaceSpecifier",
                                            "local": {
                                                "type": "Identifier",
                                                "name": "ns"
                                            }
                                        }
                                    ],
                                    "source": {
                                        "type": "Literal",
                                        "value": "mypackage.js",
                                        "raw": "'mypackage.js'"
                                    }
                                },
                            ],
                            "sourceType": "module"
                    }));

    check_se_de(Program{body: ProgramType::Module(vec![
                    ModuleItem::Export(ExportDecl::ExportAll{
                        source: Literal {
                            value: LiteralKind::Str("mypackage.js".into()),
                            raw: "\'mypackage.js\'".into(),
                            regex: None
                        }
                    })
        ])},
        json!({
                "type": "Program",
                "body": [
                    {
                        "type": "ExportAllDeclaration",
                        "source": {
                            "type": "Literal",
                            "value": "mypackage.js",
                            "raw": "\'mypackage.js\'"
                        }
                    },
                ],
                "sourceType": "module"
            }));

    check_se_de(Program{body: ProgramType::Module(vec![
                    ModuleItem::Export(ExportDecl::ExportNamed{
                        declaration: Decl::Function(
                            FunctionDecl {
                                id: Some(Identifier{
                                    name: "FunctionName".into()
                                }),
                                params: vec![],
                                body: BlockStmt{body: vec![]},
                                generator: false,
                                expression: false,
                                async: false,
                            }
                        ),
                        specifiers: vec![],
                        source: None
                    })
        ])},
        json!({
                "type": "Program",
                "body": [
                    {
                        "type": "ExportNamedDeclaration",
                        "declaration": {
                            "type": "FunctionDeclaration",
                            "id": {
                                "type": "Identifier",
                                "name": "FunctionName"
                            },
                            "params": [],
                            "body": {
                                "type": "BlockStatement",
                                "body": []
                            },
                            "generator": false,
                            "expression": false,
                            "async": false
                        },
                        "specifiers": [],
                        "source": serde_json::Value::Null
                    }
                ],
                "sourceType": "module"
            }
        ));
}

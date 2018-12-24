#[macro_use]
extern crate serde_derive;
extern crate serde;
#[macro_use]
extern crate serde_json;
extern crate serde_test;

// Based on https://esprima.readthedocs.io/en/latest/syntax-tree-format.html
pub mod estree {

    #![allow(dead_code)]


    #[cfg(test)]
    fn check_se_de<T>(t: T, json: serde_json::Value) where for<'de> T: serde::Serialize +
                                                                       serde::Deserialize<'de> +
                                                                       std::fmt::Debug +
                                                                       std::cmp::PartialEq{
        assert_eq!(serde_json::to_value(&t).unwrap(), json);
        assert_eq!(t, serde_json::from_value::<T>(json).unwrap());
    }

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
    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    struct Program {
        #[serde(rename="type")]
        ty: String,
        #[serde(rename="sourceType")]
        source_type: ProgramType,
        body: ProgramBody,
    }

    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    // #[serde(untagged)]
    enum ProgramType {
        #[serde(rename="script")]
        Script,
        #[serde(rename="module")]
        Module
    }

    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    #[serde(untagged)]
    enum ProgramBody {
        StmtList(ProgramStmtListItm),
        Module(ProgramModuleItm)
    }

    // type StatementListItem = Declaration | Statement;
    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    #[serde(untagged)]
    enum ProgramStmtListItm {
        // Decl,
        Stmt,
    }

    // type ModuleItem = ImportDeclaration | ExportDeclaration | StatementListItem;
    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    #[serde(untagged)]
    enum ProgramModuleItm {
        Import,
        Export,
        StmtList
    }


    #[test]
    fn test_program_se_de() {
        check_se_de(Program{
            ty: "Program".to_string(),
            source_type: ProgramType::Script,
            body: ProgramBody::StmtList(ProgramStmtListItm::Stmt)
        }, json!({
            "type": "Program",
            "body": serde_json::Value::Null,
            "sourceType": "script"
        }));
    }

    //
    // ///////////////////////////////////////////////////////////////////////////////
    // ////
    // //// PATTERN
    // ////
    // ///////////////////////////////////////////////////////////////////////////////
    //
    // // type BindingPattern = ArrayPattern | ObjectPattern;
    // #[derive(Serialize, Deserialize, PartialEq, Debug)]
    // #[serde(tag="type")]
    // enum BindingPattern {
    //     #[serde(rename="ArrayPattern")]
    //     Array(ArrayPattern),
    //     #[serde(rename="ObjectPattern")]
    //     Object(ObjectPattern)
    // }
    //
    //
    // // interface ArrayPattern {
    // //     type: 'ArrayPattern';
    // //     elements: ArrayPatternElement[];
    // // }
    // #[derive(Serialize, Deserialize, PartialEq, Debug)]
    // struct ArrayPattern {
    //     elements: Vec<ArrayPatternElement>
    // }
    //
    // // type ArrayPatternElement = AssignmentPattern | Identifier | BindingPattern | RestElement | null;
    // #[derive(Serialize, Deserialize, PartialEq, Debug)]
    // #[serde(untagged)]
    // enum ArrayPatternElement {
    //     Assignment(AssignmentPattern),
    //     Ident(Identifier),
    //     Binding(BindingPattern),
    //     Rest(RestElement),
    //     Null
    // }
    //
    // // interface RestElement {
    // //     type: 'RestElement';
    // //     argument: Identifier | BindingPattern;
    // // }
    // #[derive(Serialize, Deserialize, PartialEq, Debug)]
    // struct RestElement {
    //     argument: IdentOrBindingPattern,
    // }
    //
    // // interface AssignmentPattern {
    // //     type: 'AssignmentPattern';
    // //     left: Identifier | BindingPattern;
    // //     right: Expression;
    // // }
    // #[derive(Serialize, Deserialize, PartialEq, Debug)]
    // struct AssignmentPattern {
    //     left: IdentOrBindingPattern,
    //     // right: Expr, //TODO: Expr
    // }
    //
    // #[derive(Serialize, Deserialize, PartialEq, Debug)]
    // #[serde(untagged)]
    // enum IdentOrBindingPattern {
    //     Ident(Identifier),
    //     Pattern(BindingPattern)
    // }
    //
    // // interface ObjectPattern {
    // //     type: 'ObjectPattern';
    // //     properties: Property[];
    // // }
    // #[derive(Serialize, Deserialize, PartialEq, Debug)]
    // struct ObjectPattern {
    //     // properties: Vec<Properties>
    // }
    //
    //
    // ///////////////////////////////////////////////////////////////////////////////
    // ////
    // //// Expression
    // ////
    // ///////////////////////////////////////////////////////////////////////////////
    //
    // #[derive(Serialize, Deserialize, PartialEq, Debug)]
    // #[serde(tag="type")]
    // enum Expr {
    //     #[serde(rename="identifier")]
    //     Ident(Identifier)
    // }
    //
    // #[derive(Serialize, Deserialize, PartialEq, Debug)]
    // struct Identifier {
    //     name: String,
    // }

}

use expressions::*;
#[cfg(test)]
use helpers::{check_se_de};


// type BindingPattern = ArrayPattern | ObjectPattern;
#[derive(Serialize, Deserialize, PartialEq, Debug)]
#[serde(untagged)]
pub enum BindingPattern {
    Array(ArrayPattern),
    Object(ObjectPattern)
}

// interface ArrayPattern {
//     type: 'ArrayPattern';
//     elements: ArrayPatternElement[];
// }
#[derive(Serialize, Deserialize, PartialEq, Debug)]
#[serde(tag="type")]
pub struct ArrayPattern {
    pub elements: Vec<ArrayPatternElement>
}

// interface ObjectPattern {
//     type: 'ObjectPattern';
//     properties: Property[];
// }
#[derive(Serialize, Deserialize, PartialEq, Debug)]
#[serde(tag="type")]
pub struct ObjectPattern {
    pub properties: Vec<Property>,
}

// type ArrayPatternElement = AssignmentPattern | Identifier | BindingPattern | RestElement | null;
#[derive(Serialize, Deserialize, PartialEq, Debug)]
#[serde(tag="type")]
pub enum ArrayPatternElement {
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
    RestElement{argument: IdentOrPattern},
    Null,
}

// Gets serialized as object
#[derive(Serialize, Deserialize, PartialEq, Debug)]
#[serde(tag="type")]
pub enum IdentOrPattern {
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
#[derive(Serialize, Deserialize, PartialEq, Debug)]
pub struct AssignmentPattern {
    pub left: IdentOrPattern,
    pub right: Expr
}

#[test]
fn test_patterns_se_de() {

    // Object pattern test
    // var {x, y} = {x: 0, y: 1};
    check_se_de(
        BindingPattern::Object(ObjectPattern{properties: vec![
            Property{
                key: Expr::Ident(Identifier{name: "x".into()}),
                computed: false,
                value: Some(Expr::Ident(Identifier{name: "x".into()})),
                kind: PropertyKind::Init,
                shorthand: true
            },
            Property{
                key: Expr::Ident(Identifier{name: "y".into()}),
                computed: false,
                value: Some(Expr::Ident(Identifier{name: "y".into()})),
                kind: PropertyKind::Init,
                shorthand: true
            },
        ]}),
        json!({
                "type": "ObjectPattern",
                "properties": [
                    {
                        "type": "Property",
                        "key": {
                            "type": "Identifier",
                            "name": "x"
                        },
                        "computed": false,
                        "value": {
                            "type": "Identifier",
                            "name": "x"
                        },
                        "kind": "init",
                        "method": false,
                        "shorthand": true
                    },
                    {
                        "type": "Property",
                        "key": {
                            "type": "Identifier",
                            "name": "y"
                        },
                        "computed": false,
                        "value": {
                            "type": "Identifier",
                            "name": "y"
                        },
                        "kind": "init",
                        "method": false,
                        "shorthand": true
                    }
                ]
            }));

    // array pattern
    // var [head, ...tail] = [1, 2, 3];
    check_se_de(
        BindingPattern::Array(ArrayPattern{elements: vec![
            ArrayPatternElement::Ident(Identifier{name: "head".into()}),
            ArrayPatternElement::RestElement{
                argument: IdentOrPattern::Ident(Identifier{name: "tail".into()})
            }
        ]}),
        json!({
                "type": "ArrayPattern",
                "elements": [
                    {
                        "type": "Identifier",
                        "name": "head"
                    },
                    {
                        "type": "RestElement",
                        "argument": {
                            "type": "Identifier",
                            "name": "tail"
                        }
                    }
                ]
        }));

    // TODO, the doc of esprima seems to be off
    // assignment pattern
    // var { x = 3 } = {};
    // check_se_de(
    //     Property{
    //         key: Expr::Ident(Identifier{name: "x".into()}),
    //         computed: false,
    //         value: Some(Expr::)
    //     },
    //     json!(
    //         {
    //             "type": "Property",
    //             "key": {
    //                 "type": "Identifier",
    //                 "name": "x"
    //             },
    //             "computed": false,
    //             "value": {
    //                 "type": "AssignmentPattern",
    //                 "left": {
    //                     "type": "Identifier",
    //                     "name": "x"
    //                 },
    //                 "right": {
    //                     "type": "Literal",
    //                     "value": 3,
    //                     "raw": "3"
    //                 }
    //             },
    //             "kind": "init",
    //             "method": false,
    //             "shorthand": true
    //         }
    //     )
    // );
}




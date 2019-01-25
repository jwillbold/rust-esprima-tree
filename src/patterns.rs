use expressions::*;
#[cfg(test)]
use helpers::{check_se_de};


#[derive(Serialize, Deserialize, PartialEq, Debug)]
#[serde(tag="type")]
pub enum Pattern {
    #[serde(rename="AssignmentPattern")]
    Assignment(AssignmentPattern),
    #[serde(rename="Identifier")]
    Ident(Identifier),
    #[serde(rename="BindingPattern")]
    Binding(BindingPattern),
    #[serde(rename="RestElement")]
    RestElement(RestElement),
}

// type BindingPattern = ArrayPattern | ObjectPattern;
#[derive(Serialize, Deserialize, PartialEq, Debug)]
// #[serde(untagged)]
#[serde(tag="type")]
pub enum BindingPattern {
    #[serde(rename="ArrayPattern")]
    Array(ArrayPattern),
    #[serde(rename="ObjectPattern")]
    Object(ObjectPattern)
}

// interface ArrayPattern {
//     type: 'ArrayPattern';
//     elements: ArrayPatternElement[];
// }
#[derive(Serialize, Deserialize, PartialEq, Debug)]
#[serde(tag="type")]
pub struct ArrayPattern {
    // type ArrayPatternElement = AssignmentPattern | Identifier | BindingPattern | RestElement | null;
    // => type ArrayPatternElement = Pattern | null
    pub elements: Vec<Option<Pattern>>
}

// The esprima doc of esprima seems to be off regarding this
// Source: https://github.com/estree/estree/blob/master/es2015.md
#[derive(Serialize, Deserialize, PartialEq, Debug)]
#[serde(tag="type", rename="Property")]
pub struct AssignmentProperty {
    pub key: Expr,
    pub computed: bool,
    pub value: Pattern,
    pub kind: PropertyKind, // TODO constant PropertyKind::Init
    pub method: bool, // TODO constant value: false
    pub shorthand: bool
}

// interface ObjectPattern {
//     type: 'ObjectPattern';
//     properties: Property[];
// }
#[derive(Serialize, Deserialize, PartialEq, Debug)]
#[serde(tag="type")]
pub struct ObjectPattern {
    pub properties: Vec<AssignmentProperty>,
}

// Gets serialized as object
#[derive(Serialize, Deserialize, PartialEq, Debug)]
// #[serde(tag="type")]
#[serde(untagged)]
pub enum IdentOrPattern {
    // #[serde(rename="Identifier")]
    Ident(Identifier),
    // #[serde(rename="BindingPattern")]
    Pattern(Box<BindingPattern>)
}

// interface AssignmentPattern {
//     type: 'AssignmentPattern';
//     left: Identifier | BindingPattern;
//     right: Expression;
// }
#[derive(Serialize, Deserialize, PartialEq, Debug)]
#[serde(tag="type")]
pub struct AssignmentPattern {
    pub left: IdentOrPattern,
    pub right: Expr
}

// interface RestElement {
//     type: 'RestElement';
//     argument: Identifier | BindingPattern;
// }
#[derive(Serialize, Deserialize, PartialEq, Debug)]
#[serde(tag="type")]
pub struct RestElement {
    argument: IdentOrPattern
}

#[test]
fn test_patterns_se_de() {

    // Object pattern test
    // var {x, y} = {x: 0, y: 1};
    check_se_de(
        BindingPattern::Object(ObjectPattern{properties: vec![
            AssignmentProperty{
                key: Expr::Ident(Identifier{name: "x".into()}),
                computed: false,
                value: Pattern::Ident(Identifier{name: "x".into()}),
                kind: PropertyKind::Init,
                method: false,
                shorthand: true
            },
            AssignmentProperty{
                key: Expr::Ident(Identifier{name: "x".into()}),
                computed: false,
                value: Pattern::Assignment(AssignmentPattern{
                    left: IdentOrPattern::Ident(Id::new("x")),
                    right: Expr::Literal(Lit::new_int(3))
                }),
                kind: PropertyKind::Init,
                method: false,
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
                        // Assignment Property
                        "type": "Property",
                        "key": {
                        "type": "Identifier",
                            "name": "x"
                        },
                        "computed": false,
                        "value": {
                            "type": "AssignmentPattern",
                            "left": {
                                "type": "Identifier",
                                "name": "x"
                            },
                            "right": {
                                "type": "Literal",
                                "value": 3,
                                "raw": "3"
                            }
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
            Some(Pattern::Ident(Identifier{name: "head".into()})),
            Some(Pattern::RestElement(RestElement{
                argument: IdentOrPattern::Ident(Identifier{name: "tail".into()})
            }))
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

    // assignment pattern
    // var { x = 3 } = {};
    check_se_de(
        AssignmentProperty {
            key: Expr::Ident(Id::new("x")),
            computed: false,
            value: Pattern::Assignment(AssignmentPattern{
                left: IdentOrPattern::Ident(Id::new("x")),
                right: Expr::Literal(Lit::new_int(3))
            }),
            kind: PropertyKind::Init,
            method: false,
            shorthand: true,
        },
        json!(
            {
                // Assignment Property
                "type": "Property",
                "key": {
                    "type": "Identifier",
                    "name": "x"
                },
                "computed": false,
                "value": {
                    "type": "AssignmentPattern",
                    "left": {
                        "type": "Identifier",
                        "name": "x"
                    },
                    "right": {
                        "type": "Literal",
                        "value": 3,
                        "raw": "3"
                    }
                },
                "kind": "init",
                "method": false,
                "shorthand": true
            }
        )
    );
}




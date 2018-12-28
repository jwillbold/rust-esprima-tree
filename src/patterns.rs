#![allow(dead_code)]
// use serde::ser::{Serializer, SerializeStruct};
// use serde::de::{Deserialize, Deserializer};

// use declerations::*;
// use statements::*;
use expressions::*;
#[cfg(test)]
use helpers::{check_se_de};


// type BindingPattern = ArrayPattern | ObjectPattern;
#[derive(Serialize, Deserialize, PartialEq, Debug)]
pub enum BindingPattern {
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
// TODO
fn test_patterns_se_de() {

}




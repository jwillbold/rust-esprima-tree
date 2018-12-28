#![recursion_limit="128"]

extern crate serde;
#[macro_use]
extern crate serde_json;

// extern crate estree;
// pub mod estree;

extern crate jsast;


#[cfg(test)]
fn check(json: serde_json::Value) {
    let program = serde_json::from_value::<jsast::Program>(json.clone()).unwrap();
    let program_json = serde_json::to_value(program).unwrap();

    assert_eq!(program_json, json);
}

#[test]
fn test_stuff() {
    check(json!(
        {
            "type": "Program",
            "body": [
                {
                    "type": "FunctionDeclaration",
                    "id": {
                        "type": "Identifier",
                        "name": "empty"
                    },
                    "params": [],
                    "body": {
                        "type": "BlockStatement",
                        "body": []
                    },
                    "generator": false,
                    "expression": false,
                    "async": false
                }
            ],
            "sourceType": "script"
        }
    ));


    // check(
    //     json!(
    //     {
    //         "type": "Program",
    //         "body": [
    //             {
    //                 "type": "ClassDeclaration",
    //                 "id": {
    //                     "type": "Identifier",
    //                     "name": "TestClass"
    //                 },
    //                 "superClass": null,
    //                 "body": {
    //                     "type": "ClassBody",
    //                     "body": [
    //                         {
    //                             "type": "MethodDefinition",
    //                             "key": {
    //                                 "type": "Identifier",
    //                                 "name": "func1"
    //                             },
    //                             "computed": false,
    //                             "value": {
    //                                 "type": "FunctionExpression",
    //                                 "id": null,
    //                                 "params": [],
    //                                 "body": {
    //                                     "type": "BlockStatement",
    //                                     "body": []
    //                                 },
    //                                 "generator": false,
    //                                 "expression": false,
    //                                 "async": false
    //                             },
    //                             "kind": "method",
    //                             "static": false
    //                         }
    //                     ]
    //                 }
    //             }
    //         ],
    //         "sourceType": "script"
    //     }
    // ));
}

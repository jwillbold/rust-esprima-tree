// check_se_de(Expr::Conditional(ConditionalExpr{test: Box::new(Expr::This),
//                                       alternate: Box::new(Expr::This),
//                                       consequent: Box::new(Expr::This)}),
//             json!({"type": "ConditionalExpression",
//                     "test": {"type": "ThisExpression"},
//                     "alternate": {"type": "ThisExpression"},
//                     "consequent": {"type": "ThisExpression"}}));
//
// check_se_de(Expr::Logical(ConditionalExpr{operator: LogicalOp::And,
//                                       left: Box::new(Expr::This),
//                                       right: Box::new(Expr::This)}),
//             json!({"type": "LogicalExpression",
//                     "operator": "&&",
//                     "left": {"type": "ThisExpression"},
//                     "right": {"type": "ThisExpression"}}));
//
// check_se_de(Expr::New(NewExpr{callee: Box::new(Expr::This),
//                               arguments: vec![]}),
//             json!({"type": "NewExpression",
//                     "callee": {"type": "ThisExpression"},
//                     "arguments": []}));
//                     check_se_de(Expr::New(NewExpr{callee: Box::new(Expr::This),
//                                                   arguments: vec![]}),
//                                 json!({"type": "NewExpression",
//                                         "callee": {"type": "ThisExpression"},
//                                         "arguments": []}));

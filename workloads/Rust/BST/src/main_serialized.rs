use std::path::Path;

use bst::{implementation::Tree, spec};

use std::process::ExitCode;

fn main() -> ExitCode {
    let args = std::env::args().collect::<Vec<_>>();
    if args.len() < 3 {
        eprintln!("Usage: {} <tests> <property>", args[0]);
        eprintln!("Tests should be an s-expression that is a list of test cases.");
        eprintln!(
            "For available properties, check https://github.com/alpaylan/etna-cli/blob/main/docs/workloads/bst.md"
        );
        return ExitCode::FAILURE;
    }
    let tests = args[1].as_str();
    let property = args[2].as_str();

    let tests = if Path::new(tests).exists() {
        std::fs::read_to_string(tests).expect("Failed to read tests file")
    } else {
        tests.to_string()
    };

    let mut discards = 0;
    let mut passed = 0;

    match property {
        "InsertValid" => {
            let tests: Vec<(Tree, i32, i32)> = serde_lexpr::from_str(&tests)
                .expect(r#"{{"property": "InsertValid", "status": "failed parsing"}}"#);

            for (i, (t, k, v)) in tests.into_iter().enumerate() {
                match spec::prop_insert_valid(t.clone(), k, v) {
                    None => discards += 1,
                    Some(true) => passed += 1,
                    Some(false) => {
                        eprintln!(
                            r#"{{ "property": "InsertValid", "test": {}, "args": ({} {} {})}}"#,
                            i,
                            serde_lexpr::to_string(&t)
                                .unwrap_or_else(|_| "failed to serialize tree".to_string()),
                            k,
                            v
                        );
                        return ExitCode::SUCCESS;
                    }
                }
            }
        }
        "DeleteValid" => {
            let tests: Vec<(Tree, i32)> = serde_lexpr::from_str(&tests)
                .expect(r#"{{"property": "DeleteValid", "status": "failed parsing"}}"#);

            for (i, (t, k)) in tests.into_iter().enumerate() {
                match spec::prop_delete_valid(t.clone(), k) {
                    None => discards += 1,
                    Some(true) => passed += 1,
                    Some(false) => {
                        eprintln!(
                            r#"{{ "property": "DeleteValid", "status": "foundbug", "passed": {}, "discards": {}, "test": {}, "args": "({}, {})"}}"#,
                            passed,
                            discards,
                            i,
                            serde_lexpr::to_string(&t)
                                .unwrap_or_else(|_| "failed to serialize tree".to_string()),
                            k
                        );
                        return ExitCode::SUCCESS;
                    }
                }
            }
        }
        "UnionValid" => {
            let tests: Vec<(Tree, Tree)> = serde_lexpr::from_str(&tests)
                .expect(r#"{{"property": "UnionValid", "status": "failed parsing"}}"#);

            for (i, (t1, t2)) in tests.into_iter().enumerate() {
                match spec::prop_union_valid(t1.clone(), t2.clone()) {
                    None => discards += 1,
                    Some(true) => passed += 1,
                    Some(false) => {
                        eprintln!(
                            r#"{{ "property": "UnionValid", "status": "foundbug", "passed": {}, "discards": {}, "test": {}, "args": "({} {})"}}"#,
                            passed,
                            discards,
                            i,
                            serde_lexpr::to_string(&t1)
                                .unwrap_or_else(|_| "failed to serialize tree".to_string()),
                            serde_lexpr::to_string(&t2)
                                .unwrap_or_else(|_| "failed to serialize tree".to_string())
                        );
                        return ExitCode::SUCCESS;
                    }
                }
            }
        }
        "InsertPost" => {
            let tests: Vec<(Tree, i32, i32, i32)> = serde_lexpr::from_str(&tests)
                .expect(r#"{{"property": "InsertPost", "status": "failed parsing"}}"#);

            for (i, (t, k, v, query_k)) in tests.into_iter().enumerate() {
                match spec::prop_insert_post(t.clone(), k, v, query_k) {
                    None => discards += 1,
                    Some(true) => passed += 1,
                    Some(false) => {
                        eprintln!(
                            r#"{{"property": "InsertPost", "status": "foundbug", "passed": {}, "discards": {}, "test": {}, "args": "({} {} {} {})"}}"#,
                            passed,
                            discards,
                            i,
                            serde_lexpr::to_string(&t)
                                .unwrap_or_else(|_| "failed to serialize tree".to_string()),
                            k,
                            v,
                            query_k
                        );
                        return ExitCode::SUCCESS;
                    }
                }
            }
        }
        "DeletePost" => {
            let tests: Vec<(Tree, i32, i32)> = serde_lexpr::from_str(&tests)
                .expect(r#"{{"property": "DeletePost", "status": "failed parsing"}}"#);

            for (i, (t, k, query_k)) in tests.into_iter().enumerate() {
                match spec::prop_delete_post(t.clone(), k, query_k) {
                    None => discards += 1,
                    Some(true) => passed += 1,
                    Some(false) => {
                        eprintln!(
                            r#"{{"property": "DeletePost", "status": "foundbug", "passed": {}, "discards": {}, "test": {}, "args": "({} {} {})"}}"#,
                            passed,
                            discards,
                            i,
                            serde_lexpr::to_string(&t)
                                .unwrap_or_else(|_| "failed to serialize tree".to_string()),
                            k,
                            query_k
                        );
                        return ExitCode::SUCCESS;
                    }
                }
            }
        }
        "UnionPost" => {
            let tests: Vec<(Tree, Tree, i32)> = serde_lexpr::from_str(&tests)
                .expect(r#"{{"property": "UnionPost", "status": "failed parsing"}}"#);

            for (i, (t1, t2, k)) in tests.into_iter().enumerate() {
                match spec::prop_union_post(t1.clone(), t2.clone(), k) {
                    None => discards += 1,
                    Some(true) => passed += 1,
                    Some(false) => {
                        eprintln!(
                            r#"{{"property": "UnionPost", "status": "foundbug", "passed": {}, "discards": {}, "test": {}, "args": "({} {} {})"}}"#,
                            passed,
                            discards,
                            i,
                            serde_lexpr::to_string(&t1)
                                .unwrap_or_else(|_| "failed to serialize tree".to_string()),
                            serde_lexpr::to_string(&t2)
                                .unwrap_or_else(|_| "failed to serialize tree".to_string()),
                            k
                        );
                        return ExitCode::SUCCESS;
                    }
                }
            }
        }
        "InsertModel" => {
            let tests: Vec<(Tree, i32, i32)> = serde_lexpr::from_str(&tests)
                .expect(r#"{{"property": "InsertModel", "status": "failed parsing"}}"#);

            for (i, (t, k, v)) in tests.into_iter().enumerate() {
                match spec::prop_insert_model(t.clone(), k, v) {
                    None => discards += 1,
                    Some(true) => passed += 1,
                    Some(false) => {
                        eprintln!(
                            r#"{{"property": "InsertModel", "status": "foundbug", "passed": {}, "discards": {}, "test": {}, "args": "({} {} {})"}}"#,
                            passed,
                            discards,
                            i,
                            serde_lexpr::to_string(&t)
                                .unwrap_or_else(|_| "failed to serialize tree".to_string()),
                            k,
                            v
                        );
                        return ExitCode::SUCCESS;
                    }
                }
            }
        }
        "DeleteModel" => {
            let tests: Vec<(Tree, i32)> = serde_lexpr::from_str(&tests)
                .expect(r#"{{"property": "DeleteModel", "status": "failed parsing"}}"#);

            for (i, (t, k)) in tests.into_iter().enumerate() {
                match spec::prop_delete_model(t.clone(), k) {
                    None => discards += 1,
                    Some(true) => passed += 1,
                    Some(false) => {
                        eprintln!(
                            r#"{{"property": "DeleteModel", "status": "foundbug", "passed": {}, "discards": {}, "test": {}, "args": "({} {})"}}"#,
                            passed,
                            discards,
                            i,
                            serde_lexpr::to_string(&t)
                                .unwrap_or_else(|_| "failed to serialize tree".to_string()),
                            k
                        );
                        return ExitCode::SUCCESS;
                    }
                }
            }
        }
        "UnionModel" => {
            let tests: Vec<(Tree, Tree)> = serde_lexpr::from_str(&tests)
                .expect(r#"{{"property": "UnionModel", "status": "failed parsing"}}"#);

            for (i, (t1, t2)) in tests.into_iter().enumerate() {
                match spec::prop_union_model(t1.clone(), t2.clone()) {
                    None => discards += 1,
                    Some(true) => passed += 1,
                    Some(false) => {
                        eprintln!(
                            r#"{{"property": "UnionModel", "status": "foundbug", "passed": {}, "discards": {}, "test": {}, "args": "({} {})"}}"#,
                            passed,
                            discards,
                            i,
                            serde_lexpr::to_string(&t1)
                                .unwrap_or_else(|_| "failed to serialize tree".to_string()),
                            serde_lexpr::to_string(&t2)
                                .unwrap_or_else(|_| "failed to serialize tree".to_string())
                        );
                        return ExitCode::SUCCESS;
                    }
                }
            }
        }
        "InsertInsert" => {
            let tests: Vec<(Tree, i32, i32, i32, i32)> = serde_lexpr::from_str(&tests)
                .expect(r#"{{"property": "InsertInsert", "status": "failed parsing"}}"#);

            for (i, (t, k, kp, v, vp)) in tests.into_iter().enumerate() {
                match spec::prop_insert_insert(t.clone(), k, kp, v, vp) {
                    None => discards += 1,
                    Some(true) => passed += 1,
                    Some(false) => {
                        eprintln!(
                            r#"{{"property": "InsertInsert", "status": "foundbug", "passed": {}, "discards": {}, "test": {}, "args": "({} {} {} {} {})"}}"#,
                            passed,
                            discards,
                            i,
                            serde_lexpr::to_string(&t)
                                .unwrap_or_else(|_| "failed to serialize tree".to_string()),
                            k,
                            kp,
                            v,
                            vp
                        );
                        return ExitCode::SUCCESS;
                    }
                }
            }
        }
        "InsertDelete" => {
            let tests: Vec<(Tree, i32, i32, i32)> = serde_lexpr::from_str(&tests)
                .expect(r#"{{"property": "InsertDelete", "status": "failed parsing"}}"#);

            for (i, (t, k, kp, v)) in tests.into_iter().enumerate() {
                match spec::prop_insert_delete(t.clone(), k, kp, v) {
                    None => discards += 1,
                    Some(true) => passed += 1,
                    Some(false) => {
                        eprintln!(
                            r#"{{"property": "InsertDelete", "status": "foundbug", "passed": {}, "discards": {}, "test": {}, "args": "({} {} {} {})"}}"#,
                            passed,
                            discards,
                            i,
                            serde_lexpr::to_string(&t)
                                .unwrap_or_else(|_| "failed to serialize tree".to_string()),
                            k,
                            kp,
                            v
                        );
                        return ExitCode::SUCCESS;
                    }
                }
            }
        }
        "InsertUnion" => {
            let tests: Vec<(Tree, Tree, i32, i32)> = serde_lexpr::from_str(&tests)
                .expect(r#"{{"property": "InsertUnion", "status": "failed parsing"}}"#);

            for (i, (t1, t2, k, v)) in tests.into_iter().enumerate() {
                match spec::prop_insert_union(t1.clone(), t2.clone(), k, v) {
                    None => discards += 1,
                    Some(true) => passed += 1,
                    Some(false) => {
                        eprintln!(
                            r#"{{"property": "InsertUnion", "status": "foundbug", "passed": {}, "discards": {}, "test": {}, "args": "({} {} {} {})"}}"#,
                            passed,
                            discards,
                            i,
                            serde_lexpr::to_string(&t1)
                                .unwrap_or_else(|_| "failed to serialize tree".to_string()),
                            serde_lexpr::to_string(&t2)
                                .unwrap_or_else(|_| "failed to serialize tree".to_string()),
                            k,
                            v
                        );
                        return ExitCode::SUCCESS;
                    }
                }
            }
        }
        "DeleteInsert" => {
            let tests: Vec<(Tree, i32, i32, i32)> = serde_lexpr::from_str(&tests)
                .expect(r#"{{"property": "DeleteInsert", "status": "failed parsing"}}"#);

            for (i, (t, k, kp, v)) in tests.into_iter().enumerate() {
                match spec::prop_delete_insert(t.clone(), k, kp, v) {
                    None => discards += 1,
                    Some(true) => passed += 1,
                    Some(false) => {
                        eprintln!(
                            r#"{{"property": "DeleteInsert", "status": "foundbug", "passed": {}, "discards": {}, "test": {}, "args": "({} {} {} {})"}}"#,
                            passed,
                            discards,
                            i,
                            serde_lexpr::to_string(&t)
                                .unwrap_or_else(|_| "failed to serialize tree".to_string()),
                            k,
                            kp,
                            v
                        );
                        return ExitCode::SUCCESS;
                    }
                }
            }
        }
        "DeleteDelete" => {
            let tests: Vec<(Tree, i32, i32)> = serde_lexpr::from_str(&tests)
                .expect(r#"{{"property": "DeleteDelete", "status": "failed parsing"}}"#);

            for (i, (t, k, kp)) in tests.into_iter().enumerate() {
                match spec::prop_delete_delete(t.clone(), k, kp) {
                    None => discards += 1,
                    Some(true) => passed += 1,
                    Some(false) => {
                        eprintln!(
                            r#"{{"property": "DeleteDelete", "status": "foundbug", "passed": {}, "discards": {}, "test": {}, "args": "({} {} {})"}}"#,
                            passed,
                            discards,
                            i,
                            serde_lexpr::to_string(&t)
                                .unwrap_or_else(|_| "failed to serialize tree".to_string()),
                            k,
                            kp
                        );
                        return ExitCode::SUCCESS;
                    }
                }
            }
        }
        "DeleteUnion" => {
            let tests: Vec<(Tree, Tree, i32)> = serde_lexpr::from_str(&tests)
                .expect(r#"{{"property": "DeleteUnion", "status": "failed parsing"}}"#);

            for (i, (t1, t2, k)) in tests.into_iter().enumerate() {
                match spec::prop_delete_union(t1.clone(), t2.clone(), k) {
                    None => discards += 1,
                    Some(true) => passed += 1,
                    Some(false) => {
                        eprintln!(
                            r#"{{"property": "DeleteUnion", "status": "foundbug", "passed": {}, "discards": {}, "test": {}, "args": "({} {} {})"}}"#,
                            passed,
                            discards,
                            i,
                            serde_lexpr::to_string(&t1)
                                .unwrap_or_else(|_| "failed to serialize tree".to_string()),
                            serde_lexpr::to_string(&t2)
                                .unwrap_or_else(|_| "failed to serialize tree".to_string()),
                            k
                        );
                        return ExitCode::SUCCESS;
                    }
                }
            }
        }
        "UnionDeleteInsert" => {
            let tests: Vec<(Tree, Tree, i32, i32)> = serde_lexpr::from_str(&tests)
                .expect(r#"{{"property": "UnionDeleteInsert", "status": "failed parsing"}}"#);

            for (i, (t1, t2, k, v)) in tests.into_iter().enumerate() {
                match spec::prop_union_delete_insert(t1.clone(), t2.clone(), k, v) {
                    None => discards += 1,
                    Some(true) => passed += 1,
                    Some(false) => {
                        eprintln!(
                            r#"{{"property": "UnionDeleteInsert", "status": "foundbug", "passed": {}, "discards": {}, "test": {}, "args": "({} {} {} {})"}}"#,
                            passed,
                            discards,
                            i,
                            serde_lexpr::to_string(&t1)
                                .unwrap_or_else(|_| "failed to serialize tree".to_string()),
                            serde_lexpr::to_string(&t2)
                                .unwrap_or_else(|_| "failed to serialize tree".to_string()),
                            k,
                            v
                        );
                        return ExitCode::SUCCESS;
                    }
                }
            }
        }
        "UnionUnionIdempotent" => {
            let tests: Vec<Tree> = serde_lexpr::from_str(&tests)
                .expect(r#"{{"property": "UnionDeleteInsert", "status": "failed parsing"}}"#);

            for (i, t1) in tests.into_iter().enumerate() {
                match spec::prop_union_union_idempotent(t1.clone()) {
                    None => discards += 1,
                    Some(true) => passed += 1,
                    Some(false) => {
                        eprintln!(
                            r#"{{"property": "UnionDeleteInsert", "status": "foundbug", "passed": {}, "discards": {}, "test": {}, "args": "({})"}}"#,
                            passed,
                            discards,
                            i,
                            serde_lexpr::to_string(&t1)
                                .unwrap_or_else(|_| "failed to serialize tree".to_string()),
                        );
                        return ExitCode::SUCCESS;
                    }
                }
            }
        }
        "UnionUnionAssoc" => {
            let tests: Vec<(Tree, Tree, Tree)> = serde_lexpr::from_str(&tests)
                .expect(r#"{{"property": "UnionUnionAssoc", "status": "failed parsing"}}"#);

            for (i, (t1, t2, t3)) in tests.into_iter().enumerate() {
                match spec::prop_union_union_assoc(t1.clone(), t2.clone(), t3.clone()) {
                    None => discards += 1,
                    Some(true) => passed += 1,
                    Some(false) => {
                        eprintln!(
                            r#"{{"property": "UnionUnionAssoc", "status": "foundbug", "passed": {}, "discards": {}, "test": {}, "args": "({} {} {})"}}"#,
                            passed,
                            discards,
                            i,
                            serde_lexpr::to_string(&t1)
                                .unwrap_or_else(|_| "failed to serialize tree".to_string()),
                            serde_lexpr::to_string(&t2)
                                .unwrap_or_else(|_| "failed to serialize tree".to_string()),
                            serde_lexpr::to_string(&t3)
                                .unwrap_or_else(|_| "failed to serialize tree".to_string())
                        );
                        return ExitCode::SUCCESS;
                    }
                }
            }
        }
        _ => {
            eprintln!("Unknown property: {}", property);
        }
    }

    eprintln!(
        r#"{{ "property": "{}", "passed": {}, "discards": {} }}"#,
        property, passed, discards
    );
    ExitCode::FAILURE
}

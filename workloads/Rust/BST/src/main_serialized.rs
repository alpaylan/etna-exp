use std::path::Path;

use bst::{implementation::Tree, spec};
use etna_rs_utils::sampling::*;

use std::process::ExitCode;

fn sample(property: &str, tests: &str) -> SamplingResult {
    let mut discarded = 0;
    let mut passed = 0;

    match property {
        "InsertValid" => {
            let Ok(tests) = serde_lexpr::from_str::<Vec<(Tree, i32, i32)>>(&tests) else {
                return SamplingResult {
                    property: "InsertValid".to_string(),
                    tests: 0,
                    status: Status::Aborted("failed to parse tests".to_string()),
                    passed,
                    discarded,
                };
            };

            for (t, k, v) in tests.into_iter() {
                match spec::prop_insert_valid(t.clone(), k, v) {
                    None => discarded += 1,
                    Some(true) => passed += 1,
                    Some(false) => {
                        return SamplingResult {
                            property: "InsertValid".to_string(),
                            tests: passed + discarded + 1,
                            status: Status::FoundBug(format!("({} {} {})", t, k, v,)),
                            passed,
                            discarded,
                        };
                    }
                }
            }
        }
        "DeleteValid" => {
            let Ok(tests) = serde_lexpr::from_str::<Vec<(Tree, i32)>>(&tests) else {
                return SamplingResult {
                    property: "DeleteValid".to_string(),
                    tests: 0,
                    status: Status::Aborted("failed to parse tests".to_string()),
                    passed,
                    discarded,
                };
            };

            for (t, k) in tests.into_iter() {
                match spec::prop_delete_valid(t.clone(), k) {
                    None => discarded += 1,
                    Some(true) => passed += 1,
                    Some(false) => {
                        return SamplingResult {
                            property: "DeleteValid".to_string(),
                            tests: passed + discarded + 1,
                            status: Status::FoundBug(format!("({} {})", t, k,)),
                            passed,
                            discarded,
                        };
                    }
                }
            }
        }
        "UnionValid" => {
            let Ok(tests) = serde_lexpr::from_str::<Vec<(Tree, Tree)>>(&tests) else {
                return SamplingResult {
                    property: "UnionValid".to_string(),
                    tests: 0,
                    status: Status::Aborted("failed to parse tests".to_string()),
                    passed,
                    discarded,
                };
            };

            for (t1, t2) in tests.into_iter() {
                match spec::prop_union_valid(t1.clone(), t2.clone()) {
                    None => discarded += 1,
                    Some(true) => passed += 1,
                    Some(false) => {
                        return SamplingResult {
                            property: "UnionValid".to_string(),
                            tests: passed + discarded + 1,
                            status: Status::FoundBug(format!("({} {})", t1, t2)),
                            passed,
                            discarded,
                        };
                    }
                }
            }
        }
        "InsertPost" => {
            let Ok(tests) = serde_lexpr::from_str::<Vec<(Tree, i32, i32, i32)>>(&tests) else {
                return SamplingResult {
                    property: "InsertPost".to_string(),
                    tests: 0,
                    status: Status::Aborted("failed to parse tests".to_string()),
                    passed,
                    discarded,
                };
            };

            for (t, k, v, query_k) in tests.into_iter() {
                match spec::prop_insert_post(t.clone(), k, v, query_k) {
                    None => discarded += 1,
                    Some(true) => passed += 1,
                    Some(false) => {
                        return SamplingResult {
                            property: "InsertPost".to_string(),
                            tests: passed + discarded + 1,
                            status: Status::FoundBug(format!("({} {} {} {})", t, k, v, query_k,)),
                            passed,
                            discarded,
                        };
                    }
                }
            }
        }
        "DeletePost" => {
            let Ok(tests) = serde_lexpr::from_str::<Vec<(Tree, i32, i32)>>(&tests) else {
                return SamplingResult {
                    property: "DeletePost".to_string(),
                    tests: 0,
                    status: Status::Aborted("failed to parse tests".to_string()),
                    passed,
                    discarded,
                };
            };

            for (t, k, query_k) in tests.into_iter() {
                match spec::prop_delete_post(t.clone(), k, query_k) {
                    None => discarded += 1,
                    Some(true) => passed += 1,
                    Some(false) => {
                        return SamplingResult {
                            property: "DeletePost".to_string(),
                            tests: passed + discarded + 1,
                            status: Status::FoundBug(format!("({} {} {})", t, k, query_k,)),
                            passed,
                            discarded,
                        };
                    }
                }
            }
        }
        "UnionPost" => {
            let Ok(tests) = serde_lexpr::from_str::<Vec<(Tree, Tree, i32)>>(&tests) else {
                return SamplingResult {
                    property: "UnionPost".to_string(),
                    tests: 0,
                    status: Status::Aborted("failed to parse tests".to_string()),
                    passed,
                    discarded,
                };
            };

            for (t1, t2, k) in tests.into_iter() {
                match spec::prop_union_post(t1.clone(), t2.clone(), k) {
                    None => discarded += 1,
                    Some(true) => passed += 1,
                    Some(false) => {
                        return SamplingResult {
                            property: "UnionPost".to_string(),
                            tests: passed + discarded + 1,
                            status: Status::FoundBug(format!(
                                "({} {} {})",
                                t1, t2
                                k
                            )),
                            passed,
                            discarded,
                        };
                    }
                }
            }
        }
        "InsertModel" => {
            let Ok(tests) = serde_lexpr::from_str::<Vec<(Tree, i32, i32)>>(&tests) else {
                return SamplingResult {
                    property: "InsertModel".to_string(),
                    tests: 0,
                    status: Status::Aborted("failed to parse tests".to_string()),
                    passed,
                    discarded,
                };
            };

            for (t, k, v) in tests.into_iter() {
                match spec::prop_insert_model(t.clone(), k, v) {
                    None => discarded += 1,
                    Some(true) => passed += 1,
                    Some(false) => {
                        return SamplingResult {
                            property: "InsertModel".to_string(),
                            tests: passed + discarded + 1,
                            status: Status::FoundBug(format!("({} {} {})", t, k, v,)),
                            passed,
                            discarded,
                        };
                    }
                }
            }
        }
        "DeleteModel" => {
            let Ok(tests) = serde_lexpr::from_str::<Vec<(Tree, i32)>>(&tests) else {
                return SamplingResult {
                    property: "DeleteModel".to_string(),
                    tests: 0,
                    status: Status::Aborted("failed to parse tests".to_string()),
                    passed,
                    discarded,
                };
            };

            for (t, k) in tests.into_iter() {
                match spec::prop_delete_model(t.clone(), k) {
                    None => discarded += 1,
                    Some(true) => passed += 1,
                    Some(false) => {
                        return SamplingResult {
                            property: "DeleteModel".to_string(),
                            tests: passed + discarded + 1,
                            status: Status::FoundBug(format!("({} {})", t, k,)),
                            passed,
                            discarded,
                        };
                    }
                }
            }
        }
        "UnionModel" => {
            let Ok(tests) = serde_lexpr::from_str::<Vec<(Tree, Tree)>>(&tests) else {
                return SamplingResult {
                    property: "UnionModel".to_string(),
                    tests: 0,
                    status: Status::Aborted("failed to parse tests".to_string()),
                    passed,
                    discarded,
                };
            };

            for (t1, t2) in tests.into_iter() {
                match spec::prop_union_model(t1.clone(), t2.clone()) {
                    None => discarded += 1,
                    Some(true) => passed += 1,
                    Some(false) => {
                        return SamplingResult {
                            property: "UnionModel".to_string(),
                            tests: passed + discarded + 1,
                            status: Status::FoundBug(format!("({} {})", t1, t2)),
                            passed,
                            discarded,
                        };
                    }
                }
            }
        }
        "InsertInsert" => {
            let Ok(tests) = serde_lexpr::from_str::<Vec<(Tree, i32, i32, i32, i32)>>(&tests) else {
                return SamplingResult {
                    property: "InsertInsert".to_string(),
                    tests: 0,
                    status: Status::Aborted("failed to parse tests".to_string()),
                    passed,
                    discarded,
                };
            };

            for (t, k, kp, v, vp) in tests.into_iter() {
                match spec::prop_insert_insert(t.clone(), k, kp, v, vp) {
                    None => discarded += 1,
                    Some(true) => passed += 1,
                    Some(false) => {
                        return SamplingResult {
                            property: "InsertInsert".to_string(),
                            tests: passed + discarded + 1,
                            status: Status::FoundBug(format!("({} {} {} {} {})", t, k, kp, v, vp,)),
                            passed,
                            discarded,
                        };
                    }
                }
            }
        }
        "InsertDelete" => {
            let Ok(tests) = serde_lexpr::from_str::<Vec<(Tree, i32, i32, i32)>>(&tests) else {
                return SamplingResult {
                    property: "InsertDelete".to_string(),
                    tests: 0,
                    status: Status::Aborted("failed to parse tests".to_string()),
                    passed,
                    discarded,
                };
            };

            for (t, k, kp, v) in tests.into_iter() {
                match spec::prop_insert_delete(t.clone(), k, kp, v) {
                    None => discarded += 1,
                    Some(true) => passed += 1,
                    Some(false) => {
                        return SamplingResult {
                            property: "InsertDelete".to_string(),
                            tests: passed + discarded + 1,
                            status: Status::FoundBug(format!("({} {} {} {})", t, k, kp, v,)),
                            passed,
                            discarded,
                        };
                    }
                }
            }
        }
        "InsertUnion" => {
            let Ok(tests) = serde_lexpr::from_str::<Vec<(Tree, Tree, i32, i32)>>(&tests) else {
                return SamplingResult {
                    property: "InsertUnion".to_string(),
                    tests: 0,
                    status: Status::Aborted("failed to parse tests".to_string()),
                    passed,
                    discarded,
                };
            };

            for (t1, t2, k, v) in tests.into_iter() {
                match spec::prop_insert_union(t1.clone(), t2.clone(), k, v) {
                    None => discarded += 1,
                    Some(true) => passed += 1,
                    Some(false) => {
                        return SamplingResult {
                            property: "InsertUnion".to_string(),
                            tests: passed + discarded + 1,
                            status: Status::FoundBug(format!("({} {} {} {})", t1, t2, k, v,)),
                            passed,
                            discarded,
                        };
                    }
                }
            }
        }
        "DeleteInsert" => {
            let Ok(tests) = serde_lexpr::from_str::<Vec<(Tree, i32, i32, i32)>>(&tests) else {
                return SamplingResult {
                    property: "DeleteInsert".to_string(),
                    tests: 0,
                    status: Status::Aborted("failed to parse tests".to_string()),
                    passed,
                    discarded,
                };
            };

            for (t, k, kp, v) in tests.into_iter() {
                match spec::prop_delete_insert(t.clone(), k, kp, v) {
                    None => discarded += 1,
                    Some(true) => passed += 1,
                    Some(false) => {
                        return SamplingResult {
                            property: "DeleteInsert".to_string(),
                            tests: passed + discarded + 1,
                            status: Status::FoundBug(format!("({} {} {} {})", t, k, kp, v,)),
                            passed,
                            discarded,
                        };
                    }
                }
            }
        }
        "DeleteDelete" => {
            let Ok(tests) = serde_lexpr::from_str::<Vec<(Tree, i32, i32)>>(&tests) else {
                return SamplingResult {
                    property: "DeleteDelete".to_string(),
                    tests: 0,
                    status: Status::Aborted("failed to parse tests".to_string()),
                    passed,
                    discarded,
                };
            };

            for (t, k, kp) in tests.into_iter() {
                match spec::prop_delete_delete(t.clone(), k, kp) {
                    None => discarded += 1,
                    Some(true) => passed += 1,
                    Some(false) => {
                        return SamplingResult {
                            property: "DeleteDelete".to_string(),
                            tests: passed + discarded + 1,
                            status: Status::FoundBug(format!("({} {} {})", t, k, kp,)),
                            passed,
                            discarded,
                        };
                    }
                }
            }
        }
        "DeleteUnion" => {
            let Ok(tests) = serde_lexpr::from_str::<Vec<(Tree, Tree, i32)>>(&tests) else {
                return SamplingResult {
                    property: "DeleteUnion".to_string(),
                    tests: 0,
                    status: Status::Aborted("failed to parse tests".to_string()),
                    passed,
                    discarded,
                };
            };

            for (t1, t2, k) in tests.into_iter() {
                match spec::prop_delete_union(t1.clone(), t2.clone(), k) {
                    None => discarded += 1,
                    Some(true) => passed += 1,
                    Some(false) => {
                        return SamplingResult {
                            property: "DeleteUnion".to_string(),
                            tests: passed + discarded + 1,
                            status: Status::FoundBug(format!("({} {} {})", t1, t2, k)),
                            passed,
                            discarded,
                        };
                    }
                }
            }
        }
        "UnionDeleteInsert" => {
            let Ok(tests) = serde_lexpr::from_str::<Vec<(Tree, Tree, i32, i32)>>(&tests) else {
                return SamplingResult {
                    property: "UnionDeleteInsert".to_string(),
                    tests: 0,
                    status: Status::Aborted("failed to parse tests".to_string()),
                    passed,
                    discarded,
                };
            };

            for (t1, t2, k, v) in tests.into_iter() {
                match spec::prop_union_delete_insert(t1.clone(), t2.clone(), k, v) {
                    None => discarded += 1,
                    Some(true) => passed += 1,
                    Some(false) => {
                        return SamplingResult {
                            property: "UnionDeleteInsert".to_string(),
                            tests: passed + discarded + 1,
                            status: Status::FoundBug(format!("({} {} {} {})", t1, t2, k, v,)),
                            passed,
                            discarded,
                        };
                    }
                }
            }
        }
        "UnionUnionIdempotent" => {
            let Ok(tests) = serde_lexpr::from_str::<Vec<Tree>>(&tests) else {
                return SamplingResult {
                    property: "UnionUnionIdempotent".to_string(),
                    tests: 0,
                    status: Status::Aborted("failed to parse tests".to_string()),
                    passed,
                    discarded,
                };
            };

            for t1 in tests.into_iter() {
                match spec::prop_union_union_idempotent(t1.clone()) {
                    None => discarded += 1,
                    Some(true) => passed += 1,
                    Some(false) => {frm
                        return SamplingResult {
                            property: "UnionUnionIdempotent".to_string(),
                            tests: passed + discarded + 1,
                            status: Status::FoundBug(format!("({})", t1,)),
                            passed,
                            discarded,
                        };
                    }
                }
            }
        }
        "UnionUnionAssoc" => {
            let Ok(tests) = serde_lexpr::from_str::<Vec<(Tree, Tree, Tree)>>(&tests) else {
                return SamplingResult {
                    property: "UnionUnionAssoc".to_string(),
                    tests: 0,
                    status: Status::Aborted("failed to parse tests".to_string()),
                    passed,
                    discarded,
                };
            };

            for (t1, t2, t3) in tests.into_iter() {
                match spec::prop_union_union_assoc(t1.clone(), t2.clone(), t3.clone()) {
                    None => discarded += 1,
                    Some(true) => passed += 1,
                    Some(false) => {
                        return SamplingResult {
                            property: "UnionUnionAssoc".to_string(),
                            tests: passed + discarded + 1,
                            status: Status::FoundBug(format!(
                                "({} {} {})",
                                serde_lexpr::to_string(&t1)
                                    .unwrap_or_else(|_| "failed to serialize tree".to_string()),
                                serde_lexpr::to_string(&t2)
                                    .unwrap_or_else(|_| "failed to serialize tree".to_string()),
                                serde_lexpr::to_string(&t3)
                                    .unwrap_or_else(|_| "failed to serialize tree".to_string())
                            )),
                            passed,
                            discarded,
                        };
                    }
                }
            }
        }
        _ => {
            return SamplingResult {
                property: property.to_string(),
                tests: 0,
                status: Status::Aborted(format!("Unknown property: {}", property)),
                passed: 0,
                discarded: 0,
            };
        }
    };

    SamplingResult {
        property: property.to_string(),
        tests: passed + discarded,
        status: Status::Finished,
        passed,
        discarded,
    }
}

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
    let result = sample(property, &tests);

    println!("{}", result);

    match result.status {
        Status::Finished => ExitCode::SUCCESS,
        Status::FoundBug(_) | Status::Aborted(_) => ExitCode::FAILURE,
    }
}

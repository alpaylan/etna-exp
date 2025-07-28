use etna_rs_utils::{SamplingResult, Status};
use rbt::{implementation::Tree, spec};
use std::path::Path;
use std::process::ExitCode;

fn sample(property: String, tests: &str) -> SamplingResult {
    let mut discarded = 0;
    let mut passed = 0;

    match property.as_str() {
        "InsertValid" => {
            let Ok(tests) = serde_lexpr::from_str::<Vec<(Tree, i32, i32)>>(&tests) else {
                return SamplingResult {
                    property,
                    status: Status::Aborted("Failed to parse tests".to_string()),
                    tests: 0,
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
                            property,
                            status: Status::FoundBug(format!("({} {} {})", t, k, v)),
                            tests: passed + discarded + 1,
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
                    property,
                    status: Status::Aborted("failed to parse tests".to_string()),
                    tests: 0,
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
                            property,
                            status: Status::FoundBug(format!("({} {})", t, k)),
                            tests: passed + discarded + 1,
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
                    property,
                    status: Status::Aborted("failed to parse tests".to_string()),
                    tests: 0,
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
                            property,
                            status: Status::FoundBug(format!("({} {} {} {})", t, k, v, query_k)),
                            tests: passed + discarded + 1,
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
                    property,
                    status: Status::Aborted("failed to parse tests".to_string()),
                    tests: 0,
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
                            property,
                            status: Status::FoundBug(format!("({} {} {})", t, k, query_k)),
                            tests: passed + discarded + 1,
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
                    property,
                    status: Status::Aborted("failed to parse tests".to_string()),
                    tests: 0,
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
                            property,
                            status: Status::FoundBug(format!("({} {} {})", t, k, v)),
                            tests: passed + discarded + 1,
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
                    property,
                    status: Status::Aborted("failed to parse tests".to_string()),
                    tests: 0,
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
                            property,
                            status: Status::FoundBug(format!("({} {})", t, k)),
                            tests: passed + discarded + 1,
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
                    property,
                    status: Status::Aborted("failed to parse tests".to_string()),
                    tests: 0,
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
                            property,
                            status: Status::FoundBug(format!("({} {} {} {} {})", t, k, kp, v, vp)),
                            tests: passed + discarded + 1,
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
                    property,
                    status: Status::Aborted("failed to parse tests".to_string()),
                    tests: 0,
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
                            property,
                            status: Status::FoundBug(format!("({} {} {} {})", t, k, kp, v)),
                            tests: passed + discarded + 1,
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
                    property,
                    status: Status::Aborted("failed to parse tests".to_string()),
                    tests: 0,
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
                            property,
                            status: Status::FoundBug(format!("({} {} {} {})", t, k, kp, v)),
                            tests: passed + discarded + 1,
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
                    property,
                    status: Status::Aborted("failed to parse tests".to_string()),
                    tests: 0,
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
                            property,
                            status: Status::FoundBug(format!("({} {} {})", t, k, kp)),
                            tests: passed + discarded + 1,
                            passed,
                            discarded,
                        };
                    }
                }
            }
        }

        _ => {
            return SamplingResult {
                status: Status::Aborted(format!("Unknown property: {}", property)),
                property,
                tests: 0,
                passed,
                discarded,
            };
        }
    }

    return SamplingResult {
        property,
        status: Status::Finished,
        tests: passed + discarded,
        passed,
        discarded,
    };
}

fn main() -> ExitCode {
    let args = std::env::args().collect::<Vec<_>>();
    if args.len() < 3 {
        eprintln!("Usage: {} <tests> <property>", args[0]);
        eprintln!("Tests should be an s-expression that is a list of test cases.");
        eprintln!(
            "For available properties, check https://github.com/alpaylan/etna-cli/blob/main/docs/workloads/rbt.md"
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

    let result = sample(property.to_string(), &tests);

    println!("{}", result);

    match result.status {
        Status::Finished => ExitCode::SUCCESS,
        Status::FoundBug(_) | Status::Aborted(_) => ExitCode::FAILURE,
    }
}

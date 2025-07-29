use std::path::Path;

use stlc::{implementation::Expr, parser, spec, strategies::bespoke::ExprOpt};
use trace::init_depth_var;

fn main() {
    let args = std::env::args().collect::<Vec<_>>();
    if args.len() < 3 {
        eprintln!("Usage: {} <tests> <property>", args[0]);
        eprintln!("Tests should be an s-expression that is a list of test cases.");
        eprintln!(
            "For available properties, check https://github.com/alpaylan/etna-cli/blob/main/docs/workloads/stlc.md"
        );
        return;
    }
    let tests = args[1].as_str();
    let property = args[2].as_str();

    let tests = if Path::new(tests).exists() {
        std::fs::read_to_string(tests).expect("Failed to read tests file")
    } else {
        tests.to_string()
    };

    match property {
        "SinglePreserve" => {
            let tests: Vec<Expr> = parser::parse(&tests).unwrap_or_else(|e| {
                eprintln!("Failed to parse tests: '{}'", e);
                return vec![];
            });
            println!("Running SinglePreserve property tests...");

            for (i, e) in tests.into_iter().enumerate() {
                match spec::prop_single_preserve(ExprOpt(Some(e.clone()))) {
                    None => {
                        eprintln!("Test {} discarded for SinglePreserve: ({})", i, e);
                    }
                    Some(true) => {
                        println!("Test {} passed for SinglePreserve: ({})", i, e);
                    }
                    Some(false) => {
                        eprintln!("Test {} failed for SinglePreserve: ({})", i, e);
                        break;
                    }
                }
            }
        }
        "MultiPreserve" => {
            let tests: Vec<Expr> = parser::parse(&tests).unwrap_or_else(|_| {
                eprintln!("Failed to parse tests: '{}'", tests);
                return vec![];
            });

            for (i, e) in tests.into_iter().enumerate() {
                if !spec::prop_multi_preserve(ExprOpt(Some(e.clone()))).unwrap_or(true) {
                    eprintln!("Test {} failed for MultiPreserve: ({})", i, e);
                }
            }
        }

        _ => {
            eprintln!("Unknown property: {}", property);
        }
    }
}

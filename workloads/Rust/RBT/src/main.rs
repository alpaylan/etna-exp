use rbt::implementation::Tree;
use rbt::spec;

use std::time::Duration;

fn main() {
    let args = std::env::args().collect::<Vec<_>>();
    if args.len() < 3 {
        eprintln!("Usage: {} <tool> <property>", args[0]);
        eprintln!("Available tools: quickcheck");
        eprintln!(
            "For available properties, check https://github.com/alpaylan/etna-cli/blob/main/docs/workloads/rbt.md"
        );
        return;
    }
    let tool = args[1].as_str();
    let property = args[2].as_str();

    let num_tests = 200_000_000;
    let mut qc = quickcheck::QuickCheck::new()
        .tests(num_tests)
        .max_tests(num_tests * 2)
        .max_time(Duration::from_secs(60 * 60));

    let result = match (tool, property) {
        ("quickcheck", "InsertValid") => {
            qc.quicktest(spec::prop_insert_valid as fn(Tree, i32, i32) -> Option<bool>)
        }
        ("quickcheck", "DeleteValid") => {
            qc.quicktest(spec::prop_delete_valid as fn(Tree, i32) -> Option<bool>)
        }
        ("quickcheck", "InsertPost") => {
            qc.quicktest(spec::prop_insert_post as fn(Tree, i32, i32, i32) -> Option<bool>)
        }
        ("quickcheck", "DeletePost") => {
            qc.quicktest(spec::prop_delete_post as fn(Tree, i32, i32) -> Option<bool>)
        }
        ("quickcheck", "InsertModel") => {
            qc.quicktest(spec::prop_insert_model as fn(Tree, i32, i32) -> Option<bool>)
        }
        ("quickcheck", "DeleteModel") => {
            qc.quicktest(spec::prop_delete_model as fn(Tree, i32) -> Option<bool>)
        }
        ("quickcheck", "InsertInsert") => {
            qc.quicktest(spec::prop_insert_insert as fn(Tree, i32, i32, i32, i32) -> Option<bool>)
        }
        ("quickcheck", "InsertDelete") => {
            qc.quicktest(spec::prop_insert_delete as fn(Tree, i32, i32, i32) -> Option<bool>)
        }
        ("quickcheck", "DeleteInsert") => {
            qc.quicktest(spec::prop_delete_insert as fn(Tree, i32, i32, i32) -> Option<bool>)
        }
        ("quickcheck", "DeleteDelete") => {
            qc.quicktest(spec::prop_delete_delete as fn(Tree, i32, i32) -> Option<bool>)
        }
        _ => {
            panic!("Unknown tool or property: {} {}", tool, property)
        }
    };

    result.print_status();
}

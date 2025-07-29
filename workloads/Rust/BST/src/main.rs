use bst::{implementation::Tree, spec};
use std::time::Duration;

fn main() {
    let args = std::env::args().collect::<Vec<_>>();
    if args.len() < 3 {
        eprintln!("Usage: {} <tool> <property>", args[0]);
        eprintln!("Available tools: quickcheck");
        eprintln!(
            "For available properties, check https://github.com/alpaylan/etna-cli/blob/main/docs/workloads/bst.md"
        );
        return;
    }
    let tool = args[1].as_str();
    let property = args[2].as_str();

    let num_tests = 200_00;
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
        ("quickcheck", "UnionValid") => {
            qc.quicktest(spec::prop_union_valid as fn(Tree, Tree) -> Option<bool>)
        }
        ("quickcheck", "InsertPost") => {
            qc.quicktest(spec::prop_insert_post as fn(Tree, i32, i32, i32) -> Option<bool>)
        }
        ("quickcheck", "DeletePost") => {
            qc.quicktest(spec::prop_delete_post as fn(Tree, i32, i32) -> Option<bool>)
        }
        ("quickcheck", "UnionPost") => {
            qc.quicktest(spec::prop_union_post as fn(Tree, Tree, i32) -> Option<bool>)
        }
        ("quickcheck", "InsertModel") => {
            qc.quicktest(spec::prop_insert_model as fn(Tree, i32, i32) -> Option<bool>)
        }
        ("quickcheck", "DeleteModel") => {
            qc.quicktest(spec::prop_delete_model as fn(Tree, i32) -> Option<bool>)
        }
        ("quickcheck", "UnionModel") => {
            qc.quicktest(spec::prop_union_model as fn(Tree, Tree) -> Option<bool>)
        }
        ("quickcheck", "InsertInsert") => {
            qc.quicktest(spec::prop_insert_insert as fn(Tree, i32, i32, i32, i32) -> Option<bool>)
        }
        ("quickcheck", "InsertDelete") => {
            qc.quicktest(spec::prop_insert_delete as fn(Tree, i32, i32, i32) -> Option<bool>)
        }
        ("quickcheck", "InsertUnion") => {
            qc.quicktest(spec::prop_insert_union as fn(Tree, Tree, i32, i32) -> Option<bool>)
        }
        ("quickcheck", "DeleteInsert") => {
            qc.quicktest(spec::prop_delete_insert as fn(Tree, i32, i32, i32) -> Option<bool>)
        }
        ("quickcheck", "DeleteDelete") => {
            qc.quicktest(spec::prop_delete_delete as fn(Tree, i32, i32) -> Option<bool>)
        }
        ("quickcheck", "DeleteUnion") => {
            qc.quicktest(spec::prop_delete_union as fn(Tree, Tree, i32) -> Option<bool>)
        }
        ("quickcheck", "UnionDeleteInsert") => {
            qc.quicktest(spec::prop_union_delete_insert as fn(Tree, Tree, i32, i32) -> Option<bool>)
        }
        ("quickcheck", "UnionUnionIdempotent") => {
            qc.quicktest(spec::prop_union_union_idempotent as fn(Tree) -> Option<bool>)
        }
        ("quickcheck", "UnionUnionAssoc") => {
            qc.quicktest(spec::prop_union_union_assoc as fn(Tree, Tree, Tree) -> Option<bool>)
        }
        _ => {
            panic!("Unknown tool or property: {} {}", tool, property)
        }
    };


    result.print_status();
}

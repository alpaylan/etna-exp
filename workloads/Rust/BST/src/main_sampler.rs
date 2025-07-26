use bst::{implementation::Tree, spec};

fn main() {
    let args = std::env::args().collect::<Vec<_>>();
    if args.len() < 4 {
        eprintln!("Usage: {} <tool> <property> <tests>", args[0]);
        eprintln!("Available tools: quickcheck");
        eprintln!(
            "For available properties, check https://github.com/alpaylan/etna-cli/blob/main/docs/workloads/bst.md"
        );
        return;
    }
    let tool = args[1].as_str();
    let property = args[2].as_str();
    let tests = args[3].as_str();

    let num_tests = tests
        .parse::<u64>()
        .expect(format!("Failed to parse number of tests: '{}'", tests).as_str());
    let mut qc = quickcheck::QuickCheck::new()
        .tests(num_tests)
        .max_tests(num_tests * 2)
        .max_time(std::time::Duration::from_secs(1));

    let result = match (tool, property) {
        ("quickcheck", "InsertValid") => {
            qc.quicksample(spec::prop_insert_valid as fn(Tree, i32, i32) -> Option<bool>)
        }
        ("quickcheck", "DeleteValid") => {
            qc.quicksample(spec::prop_delete_valid as fn(Tree, i32) -> Option<bool>)
        }
        ("quickcheck", "UnionValid") => {
            qc.quicksample(spec::prop_union_valid as fn(Tree, Tree) -> Option<bool>)
        }
        ("quickcheck", "InsertPost") => {
            qc.quicksample(spec::prop_insert_post as fn(Tree, i32, i32, i32) -> Option<bool>)
        }
        ("quickcheck", "DeletePost") => {
            qc.quicksample(spec::prop_delete_post as fn(Tree, i32, i32) -> Option<bool>)
        }
        ("quickcheck", "UnionPost") => {
            qc.quicksample(spec::prop_union_post as fn(Tree, Tree, i32) -> Option<bool>)
        }
        ("quickcheck", "InsertModel") => {
            qc.quicksample(spec::prop_insert_model as fn(Tree, i32, i32) -> Option<bool>)
        }
        ("quickcheck", "DeleteModel") => {
            qc.quicksample(spec::prop_delete_model as fn(Tree, i32) -> Option<bool>)
        }
        ("quickcheck", "UnionModel") => {
            qc.quicksample(spec::prop_union_model as fn(Tree, Tree) -> Option<bool>)
        }
        ("quickcheck", "InsertInsert") => {
            qc.quicksample(spec::prop_insert_insert as fn(Tree, i32, i32, i32, i32) -> Option<bool>)
        }
        ("quickcheck", "InsertUnion") => {
            qc.quicksample(spec::prop_insert_union as fn(Tree, Tree, i32, i32) -> Option<bool>)
        }
        ("quickcheck", "InsertDelete") => {
            qc.quicksample(spec::prop_insert_delete as fn(Tree, i32, i32, i32) -> Option<bool>)
        }
        ("quickcheck", "DeleteInsert") => {
            qc.quicksample(spec::prop_delete_insert as fn(Tree, i32, i32, i32) -> Option<bool>)
        }
        ("quickcheck", "DeleteDelete") => {
            qc.quicksample(spec::prop_delete_delete as fn(Tree, i32, i32) -> Option<bool>)
        }
        ("quickcheck", "DeleteUnion") => {
            qc.quicksample(spec::prop_delete_union as fn(Tree, Tree, i32) -> Option<bool>)
        }
        ("quickcheck", "UnionDeleteInsert") => {
            qc.quicksample(spec::prop_union_delete_insert as fn(Tree, Tree, i32, i32) -> Option<bool>)
        }
        ("quickcheck", "UnionUnionIdempotent") => {
            qc.quicksample(spec::prop_union_union_idempotent as fn(Tree) -> Option<bool>)
        }
        ("quickcheck", "UnionUnionAssoc") => {
            qc.quicksample(spec::prop_union_union_assoc as fn(Tree, Tree, Tree) -> Option<bool>)
        }
        _ => {
            panic!("Unknown tool or property: {} {}", tool, property)
        }
    };

    let mut results = Vec::<serde_json::Value>::new();

    for (duration, element) in result {
        let mut object = serde_json::Map::new();
        object.insert(
            "time".to_string(),
            serde_json::Value::String(format!("{}ns", duration.as_nanos())),
        );
        object.insert(
            "value".to_string(),
            serde_json::Value::String(element.to_string()),
        );
        results.push(serde_json::Value::Object(object));
    }

    let results = serde_json::Value::Array(results);

    let output = serde_json::to_string(&results).expect("Failed to serialize results to JSON");

    println!("{}", output);
}

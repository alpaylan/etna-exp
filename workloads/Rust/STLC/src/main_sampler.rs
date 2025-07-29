use stlc::{spec, strategies::bespoke::ExprOpt};

fn main() {
    let args = std::env::args().collect::<Vec<_>>();
    if args.len() < 4 {
        eprintln!("Usage: {} <tool> <property> <tests>", args[0]);
        eprintln!("Available tools: quickcheck");
        eprintln!(
            "For available properties, check https://github.com/alpaylan/etna-cli/blob/main/docs/workloads/stlc.md"
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
        ("quickcheck", "SinglePreserve") => {
            qc.quicksample(spec::prop_single_preserve as fn(ExprOpt) -> Option<bool>)
        }
        ("quickcheck", "MultiPreserve") => {
            qc.quicksample(spec::prop_multi_preserve as fn(ExprOpt) -> Option<bool>)
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

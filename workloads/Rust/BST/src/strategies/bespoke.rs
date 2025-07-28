use quickcheck::Arbitrary;

use crate::implementation::Tree;


fn gen_tree(g: &mut quickcheck::Gen, size: usize, lo: i32, hi: i32) -> Tree {
    // println!("Generating tree with size: {}, range: ({}, {})", size, lo, hi);
    if size == 0 || lo + 1 >= hi - 1 {
        return Tree::E;
    }

    let k = g.random_range((lo + 1)..(hi - 1));
    let left = gen_tree(g, size / 2, lo, k);
    let right = gen_tree(g, size / 2, k, hi);
    Tree::T(Box::new(left), k, k, Box::new(right))
}

impl Arbitrary for Tree {
    fn arbitrary(g: &mut quickcheck::Gen) -> Self {
        let size = g.size();
        let lo = 0;
        let hi = 100;
        gen_tree(g, size, lo, hi)
    }
}

use quickcheck::Arbitrary;

use crate::implementation::Tree;

fn gen_tree(g: &mut quickcheck::Gen, size: usize, lo: i32, hi: i32) -> Tree {
    if size == 0 || lo + 1 >= hi - 1 {
        return Tree::E;
    }

    let k = *g.choose(&((lo + 1)..(hi - 1)).into_iter().collect::<Vec<_>>()).unwrap();
    let left = gen_tree(g, size - 1, lo, k);
    let right = gen_tree(g, size - 1, k, hi);
    Tree::T(Box::new(left), k, k, Box::new(right))
}

impl Arbitrary for Tree {
    fn arbitrary(g: &mut quickcheck::Gen) -> Self {
        let size = ((g.size() as f64) * 100.0) as usize;
        let lo = 0;
        let hi = 100;
        gen_tree(g, size, lo, hi)
    }
}
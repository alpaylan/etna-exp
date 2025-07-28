use quickcheck::{Arbitrary, Gen};

use crate::implementation::{
    Color::{self, *},
    Tree::{self, *},
    blacken, elems,
};

fn choose(min: usize, max: usize, g: &mut Gen) -> usize {
    usize::arbitrary(g) % (max - min + 1) + min
}

pub(crate) fn gen_kvs(size: usize, g: &mut Gen) -> Vec<(i32, i32)> {
    let mut kvs = Vec::with_capacity(size);
    for _ in 0..size {
        let k = Arbitrary::arbitrary(g);
        let v = Arbitrary::arbitrary(g);
        kvs.push((k, v));
    }
    kvs
}

fn balance(col: Color, tl: Tree, key: i32, val: i32, tr: Tree) -> Tree {
    match (col, tl, key, val, tr) {
        (B, T(R, box T(R, a, x, vx, b), y, vy, c), z, vz, d) => T(
            R,
            Box::new(T(B, a, x, vx, b)),
            y,
            vy,
            Box::new(T(B, c, z, vz, Box::new(d))),
        ),

        (B, T(R, a, x, vx, box T(R, b, y, vy, c)), z, vz, d) => T(
            R,
            Box::new(T(B, a, x, vx, b)),
            y,
            vy,
            Box::new(T(B, c, z, vz, Box::new(d))),
        ),
        (B, a, x, vx, T(R, box T(R, b, y, vy, c), z, vz, d)) => T(
            R,
            Box::new(T(B, Box::new(a), x, vx, b)),
            y,
            vy,
            Box::new(T(B, c, z, vz, d)),
        ),
        (B, a, x, vx, T(R, b, y, vy, box T(R, c, z, vz, d))) => T(
            R,
            Box::new(T(B, Box::new(a), x, vx, b)),
            y,
            vy,
            Box::new(T(B, c, z, vz, d)),
        ),
        (rb, a, x, vx, b) => T(rb, Box::new(a), x, vx, Box::new(b)),
    }
}

pub(crate) fn insert(key: i32, val: i32, t: Tree) -> Tree {
    fn ins(x: i32, vx: i32, s: Tree) -> Tree {
        match (x, vx, s) {
            (x, vx, E) => T(R, Box::new(E), x, vx, Box::new(E)),
            (x, vx, T(rb, box a, y, vy, box b)) => {
                if x < y {
                    balance(rb, ins(x, vx, a), y, vy, b)
                } else if y < x {
                    balance(rb, a, y, vy, ins(x, vx, b))
                } else {
                    T(rb, Box::new(a), y, vx, Box::new(b))
                }
            }
        }
    }
    blacken(ins(key, val, t))
}

pub(crate) fn bespoke(g: &mut Gen) -> Tree {
    let s= g.size() + 1;
    let sz = choose(1, s, g);
    let kvs = gen_kvs(sz, g);
    kvs.iter().fold(E, |t, (k, v)| insert(*k, *v, t))
}

impl Arbitrary for Tree {
    fn arbitrary(g: &mut Gen) -> Self {
        bespoke(g)
    }

    fn shrink(&self) -> Box<dyn Iterator<Item = Self>> {
        let elems = elems(&self);
        if elems.len() <= 2 {
            return Box::new(std::iter::empty());
        }

        let t1 = elems[1..].iter().fold(E, |t, (k, v)| insert(*k, *v, t));
        let t2 = elems[..elems.len() - 1]
            .iter()
            .fold(E, |t, (k, v)| insert(*k, *v, t));

        Box::new(std::iter::once(t1).chain(std::iter::once(t2)))
    }
}

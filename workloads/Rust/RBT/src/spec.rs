use crate::implementation::{Color, Tree, delete, find, insert};

use etna_rs_utils::Implies as _;

pub(crate) fn is_bst(t: &Tree) -> bool {
    fn every(p: &dyn Fn(i32) -> bool, t: &Tree) -> bool {
        match t {
            Tree::E => true,
            Tree::T(_, box a, x, _, box b) => every(&p, a) && p(*x) && every(&p, b),
        }
    }
    match t {
        Tree::E => true,
        Tree::T(_, a, x, _, b) => {
            // -- Difference from SC: donpt allow repeated keys.
            every(&|y| y < *x, a) && every(&|y| y > *x, b) && is_bst(a) && is_bst(b)
        }
    }
}

pub(crate) fn no_red_red(t: &Tree) -> bool {
    fn black_root(t: &Tree) -> bool {
        match t {
            Tree::T(Color::R, _, _, _, _) => false,
            _ => true,
        }
    }
    match t {
        Tree::E => true,
        Tree::T(Color::B, a, _, _, b) => no_red_red(a) && no_red_red(b),
        Tree::T(Color::R, a, _, _, b) => {
            black_root(a) && black_root(b) && no_red_red(a) && no_red_red(b)
        }
    }
}

pub(crate) fn consistent_black_height(t: &Tree) -> bool {
    fn go(t: &Tree) -> (bool, i32) {
        match t {
            Tree::E => (true, 1),
            Tree::T(rb, a, _, _, b) => {
                let (a_bool, a_height) = go(a);
                let (b_bool, b_height) = go(b);
                let is_black = |rb| match rb {
                    Color::B => 1,
                    Color::R => 0,
                };
                (
                    a_bool && b_bool && a_height == b_height,
                    a_height + is_black(*rb),
                )
            }
        }
    }

    let (a_bool, _) = go(&t);
    a_bool
}

pub(crate) fn is_rbt(t: &Tree) -> Option<bool> {
    Some(is_bst(t) && consistent_black_height(t) && no_red_red(t))
}

pub(crate) fn to_list(t: &Tree) -> Vec<(i32, i32)> {
    match t {
        Tree::E => Vec::new(),
        Tree::T(_, l, k, v, r) => [to_list(l), vec![(*k, *v)], to_list(r)].concat(),
    }
}

pub fn prop_insert_valid(t: Tree, k: i32, v: i32) -> Option<bool> {
    is_rbt(&t).implies(|| is_rbt(&insert(k, v, t.clone())))
}

pub fn prop_delete_valid(t: Tree, k: i32) -> Option<bool> {
    is_rbt(&t).implies(|| is_rbt(&delete(k, t.clone())?))
}

pub fn prop_insert_post(t: Tree, k: i32, k2: i32, v: i32) -> Option<bool> {
    is_rbt(&t).implies(|| {
        find(k2, insert(k, v, t.clone()))
            == if k == k2 {
                Some(v)
            } else {
                find(k2, t.clone())
            }
    })
}

pub fn prop_delete_post(t: Tree, k: i32, k2: i32) -> Option<bool> {
    is_rbt(&t).implies(|| {
        Some(find(k2, delete(k, t.clone())?) == if k == k2 { None } else { find(k2, t.clone()) })
    })
}

pub(crate) fn delete_key(k: i32, l: &[(i32, i32)]) -> Vec<(i32, i32)> {
    l.iter().filter(|&&(x, _)| x != k).cloned().collect()
}

pub(crate) fn l_insert(kv: (i32, i32), l: &[(i32, i32)]) -> Vec<(i32, i32)> {
    if l.is_empty() {
        vec![kv]
    } else {
        let (k, v) = l[0];
        if kv.0 == k {
            let mut xs = l[1..].to_vec();
            xs.insert(0, kv);
            xs
        } else if kv.0 < k {
            let mut l = l.to_vec();
            l.insert(0, kv);
            l
        } else {
            let mut l_new = l_insert(kv, &l[1..]);
            l_new.insert(0, (k, v));
            l_new
        }
    }
}

pub fn prop_insert_model(t: Tree, k: i32, v: i32) -> Option<bool> {
    is_rbt(&t).implies(|| {
        to_list(&insert(k, v, t.clone())) == l_insert((k, v), &delete_key(k, &to_list(&t)))
    })
}

pub fn prop_delete_model(t: Tree, k: i32) -> Option<bool> {
    is_rbt(&t).implies(|| Some(to_list(&delete(k, t.clone())?) == delete_key(k, &to_list(&t))))
}

pub fn prop_insert_insert(t: Tree, k: i32, kp: i32, v: i32, vp: i32) -> Option<bool> {
    is_rbt(&t).implies(|| {
        let t1 = insert(k, v, t.clone());
        let t2 = insert(kp, vp, insert(k, v, t.clone()));

        to_list(&insert(k, v, insert(kp, vp, t.clone())))
            == to_list(if k == kp { &t1 } else { &t2 })
    })
}

pub fn prop_insert_delete(t: Tree, k: i32, kp: i32, v: i32) -> Option<bool> {
    is_rbt(&t).implies(|| {
        let t1 = insert(k, v, t.clone());
        let t2 = delete(kp, t.clone())?;

        Some(
            to_list(&delete(kp, insert(k, v, t.clone()))?)
                == to_list(if k == kp { &t1 } else { &t2 }),
        )
    })
}

pub fn prop_delete_insert(t: Tree, k: i32, kp: i32, v: i32) -> Option<bool> {
    is_rbt(&t).implies(|| match delete(k, insert(kp, v, t.clone())) {
        None => false,
        Some(tp) => match delete(k, t.clone()) {
            None => false,
            Some(tpp) => {
                let tppp = insert(kp, v, tpp.clone());
                to_list(&tp) == to_list(if k == kp { &tpp } else { &tppp })
            }
        },
    })
}

pub fn prop_delete_delete(t: Tree, k: i32, kp: i32) -> Option<bool> {
    is_rbt(&t).implies(|| match delete(kp, t.clone()) {
        None => false,
        Some(tp) => match delete(k, tp) {
            None => false,
            Some(tpp) => match delete(k, t.clone()) {
                None => false,
                Some(t1p) => match delete(kp, t1p) {
                    None => false,
                    Some(t1pp) => to_list(&tpp) == to_list(&t1pp),
                },
            },
        },
    })
}

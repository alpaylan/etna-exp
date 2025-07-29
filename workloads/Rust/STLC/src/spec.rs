use crate::{
    implementation::{Expr, Typ, get_typ, multistep, pstep, type_check},
    strategies::bespoke::ExprOpt,
};

pub fn mt(e: &Expr) -> Option<Typ> {
    get_typ(&vec![], e)
}

pub fn m_type_check(e: &Expr, t: &Typ) -> bool {
    type_check(&vec![], e, t)
}

pub fn prop_single_preserve(e: ExprOpt) -> Option<bool> {
    let ExprOpt(Some(e)) = e else { return None };
    let tp = mt(&e)?;
    Some(pstep(&e).map(|e| m_type_check(&e, &tp)).unwrap_or(true))
}

pub fn prop_multi_preserve(e: ExprOpt) -> Option<bool> {
    let ExprOpt(Some(e)) = e else { return None };

    let tp = mt(&e)?;
    Some(
        multistep(40, pstep, &e)
            .map(|e| m_type_check(&e, &tp))
            .unwrap_or(true),
    )
}

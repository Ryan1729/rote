use crate::*;
use super::arb;
use proptest::proptest;

#[allow(unused)]
fn then_applying_is_the_same_as_applying_in_sequence_on(
    rope: Rope,
    (e1, e2): (Edit, Edit),
) {
    let e3 = e1.clone() + e2.clone();
    let unedited = CursoredRope::from(rope);

    let added = {
        let mut c_r = unedited.clone();

        c_r.apply(&e3);

        c_r
    };

    let separate = {
        let mut c_r = unedited.clone();

        c_r.apply(&e1);
        c_r.apply(&e2);

        c_r
    };

    assert_eq!(added, separate);
    panic!();
}

proptest!{
    #[test]
    fn then_applying_is_the_same_as_applying_in_sequence(
        (r, (e1, e2)) in arb::rope_and_edit_pair()
    ) {
        then_applying_is_the_same_as_applying_in_sequence_on(r, (e1, e2));
    }
}
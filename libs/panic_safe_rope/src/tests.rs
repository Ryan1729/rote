use super::*;

use proptest::proptest;

proptest!{
    #[test]
    fn empty_slice_works(s in ".*") {
        let r: Rope = s.into();

        let slice = r.empty_slice();
        assert_eq!(slice.len_chars().0, 0);
    }

    #[test]
    fn full_slice_works(s in ".*") {
        let r: Rope = s.into();

        let slice = r.full_slice();
        assert_eq!(slice.len_chars().0, r.len_chars().0);
    }
}

mod included_files;
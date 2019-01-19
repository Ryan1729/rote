use std::path::{Path, PathBuf};

// We expect to have this function take a recursive directory traversal iterator later
pub fn find_in<'path, I: Iterator<Item = &'path Path>>(
    paths: I,
    needle: &str,
) -> Vec<PathBuf> {
    if needle.len() == 0 {
        return Vec::new();
    }
    let len = {
        if let (_, Some(l)) = paths.size_hint() {
            l
        } else {
            128
        }
    };
    let mut output: Vec<PathBuf> = Vec::with_capacity(len);

    for path in paths {
        let mut contains_needle = false;

        // This macro attempts to abstract over `Path::to_str` and `Path::to_string_lossy`
        // with a minimum of duplication
        macro_rules! cmp_match_indices {
            ($p1: expr, $p2: expr, $path_cmp: expr) => {{
                use std::cmp::Ordering::*;
                let mut p1_iter = $p1.match_indices(needle);
                let mut p2_iter = $p2.match_indices(needle);
                let mut backup_ordering = Equal;

                // TODO do we care that `match_indices` doesn't return overlapping matches?
                let mut p1_needle_count = 0;
                let mut p2_needle_count = 0;
                loop {
                    match (p1_iter.next(), p2_iter.next()) {
                        (Some((p1_i, _)), Some((p2_i, _))) => {
                            contains_needle = true;
                            backup_ordering = p1_i.cmp(&p2_i);

                            p1_needle_count += 1;
                            p2_needle_count += 1;
                        }
                        (Some(_), None) => {
                            contains_needle = true;
                            p1_needle_count += 1;
                            break;
                        }
                        (None, Some(_)) => {
                            p2_needle_count += 1;
                            break;
                        }
                        (None, None) => {
                            break;
                        }
                    }
                }

                p1_needle_count
                    .cmp(&p2_needle_count)
                    .then_with(|| backup_ordering)
                    .then_with(|| $path_cmp)
            }};
        }

        let i = if output.len() == 0 {
            // base case for when we don't havea second path to compare to.
            contains_needle = match path.to_str() {
                // Fast(er) path for valid unicde paths
                Some(s) => s.match_indices(needle).count() > 0,
                None => {
                    let s = path.to_string_lossy();
                    s.match_indices(needle).count() > 0
                }
            };
            0
        } else {
            output
                .binary_search_by(|p| {
                    match (path.to_str(), p.to_str()) {
                        // Fast(er) path for valid unicode paths
                        (Some(s1), Some(s2)) => cmp_match_indices!(s1, s2, p.as_path().cmp(path)),
                        (Some(s1), None) => {
                            let s2 = p.to_string_lossy();
                            cmp_match_indices!(s1, s2, p.as_path().cmp(path))
                        }
                        (None, Some(s2)) => {
                            let s1 = path.to_string_lossy();
                            cmp_match_indices!(s1, s2, p.as_path().cmp(path))
                        }
                        (None, None) => {
                            let s1 = path.to_string_lossy();
                            let s2 = p.to_string_lossy();
                            cmp_match_indices!(s1, s2, p.as_path().cmp(path))
                        }
                    }
                })
                .unwrap_or_else(|i| i)
        };

        if contains_needle {
            output.insert(i, path.to_path_buf());
        }
    }
    output
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn find_in_paths_works_on_this_small_example() {
        let needle = "b";

        let searched_paths = vec![
            PathBuf::from("C:\\Users\\ryan1729\\Documents\\bartog.txt"),
            PathBuf::from("C:\\Users\\ryan1729\\Documents\\beans.txt"),
            PathBuf::from("C:\\Users\\ryan1729\\Documents\\unrelated.txt"),
        ];

        let expected = vec![
            PathBuf::from("C:\\Users\\ryan1729\\Documents\\bartog.txt"),
            PathBuf::from("C:\\Users\\ryan1729\\Documents\\beans.txt"),
        ];

        assert_eq!(
            find_in(searched_paths.iter().map(|p| p.as_path()), needle),
            expected
        );
    }

    #[test]
    fn find_in_paths_sorts_things_with_multiple_needles_higher() {
        let needle = "b";

        let searched_paths = vec![
            PathBuf::from("C:\\Users\\ryan1729\\Documents\\basketball.txt"),
            PathBuf::from("C:\\Users\\ryan1729\\Documents\\beans.txt"),
            PathBuf::from("C:\\Users\\ryan1729\\Documents\\beebasketball.txt"),
        ];

        let expected = vec![
            PathBuf::from("C:\\Users\\ryan1729\\Documents\\beebasketball.txt"),
            PathBuf::from("C:\\Users\\ryan1729\\Documents\\basketball.txt"),
            PathBuf::from("C:\\Users\\ryan1729\\Documents\\beans.txt"),
        ];

        assert_eq!(
            find_in(searched_paths.iter().map(|p| p.as_path()), needle),
            expected
        );
    }
}

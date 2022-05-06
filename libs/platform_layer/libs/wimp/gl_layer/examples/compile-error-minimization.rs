fn main() {
    let non_copy = String::new();

    type F = dyn Fn();
    let f: &F = &|| { &non_copy; };

    fn takes_dyn(_: &dyn Fn()) {}

    takes_dyn(f);

    drop(non_copy);
}

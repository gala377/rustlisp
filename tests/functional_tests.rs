mod setup;

macro_rules! functional_tests {
    ($($name:ident),*$(,)?) => {
        $(
        #[test]
            fn $name() {
                setup::run_rustlisp_file(stringify!($name).to_owned() + ".rlp")
            }
        )*
    };
}

functional_tests! {
    lambda_captures_local_vars,
}
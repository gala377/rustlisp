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
    empty_file_test,
    lambda_captures_local_vars,
    equal_function_test,
    assert_implementations_test,
    quasiquote_form_test,
    macro_tests,
    new_local_env_tests,
    line_comments_test,
    control_flow_test,
    set_form_tests,
    native_types_test,
    load_function_test,
}

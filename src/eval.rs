use std::convert::TryInto;

#[cfg(feature = "hashbrown")]
use hashbrown::HashMap;
#[cfg(not(feature = "hashbrown"))]
use std::collections::HashMap;

#[cfg(debug)]
use crate::gc::HeapMarked;

use crate::{
    env::{BuiltinSymbols, Environment, SymbolId, SymbolTable},
    gc::{self, Heap, MarkSweep, ScopedMutPtr, ScopedPtr, ScopedRef},
    runtime::{FunctionArgs, Lambda, RootedVal, WeakVal},
};

type Env = Environment;

#[derive(Clone)]
pub struct FuncFrame {
    pub globals: Env,
    pub locals: Option<Env>,
}

pub enum ModuleState {
    Evaluated(Env),
    Evaluating,
}

pub struct Interpreter {
    pub heap: Heap,
    pub gc: MarkSweep,
    pub call_stack: Vec<FuncFrame>,
    pub symbols: SymbolTable,
    pub modules: HashMap<String, ModuleState>,
}

impl Interpreter {
    pub fn new(
        heap: Heap,
        gc: MarkSweep,
        globals: Env,
        locals: Option<Env>,
        symbols: SymbolTable,
        main_module_name: &str,
    ) -> Self {
        let mut modules = HashMap::new();
        modules.insert(main_module_name.into(), ModuleState::Evaluating);
        Self {
            heap,
            gc,
            symbols,
            modules,
            call_stack: vec![FuncFrame { globals, locals }],
        }
    }

    pub fn eval(&mut self, expr: &RootedVal) -> RootedVal {
        self.eval_weak(&expr.as_weak())
    }

    fn eval_weak(&mut self, expr: &WeakVal) -> RootedVal {
        let res = match expr {
            WeakVal::NumberVal(_) | WeakVal::StringVal(_) => expr.as_root(),
            WeakVal::Symbol(val) => self.eval_symbol(*val),
            WeakVal::List(val) => self.eval_list(val),
            _ => panic!("Cannot evaluate this node"),
        };
        match &res {
            RootedVal::Boxed { inner, auto_deref } if *auto_deref => self.get_ref(inner).as_root(),
            _ => res,
        }
    }

    fn _eval_weak_do_not_auto_deref_box(&mut self, expr: &WeakVal) -> RootedVal {
        match expr {
            WeakVal::NumberVal(_) | WeakVal::StringVal(_) => expr.as_root(),
            WeakVal::Symbol(val) => self.eval_symbol(*val),
            WeakVal::List(val) => self.eval_list(val),
            _ => panic!("Cannot evaluate this node"),
        }
    }

    fn eval_symbol(&mut self, expr: SymbolId) -> RootedVal {
        let mut curr_env = self.get_locals();
        while let Some(env) = curr_env {
            if let Some(val) = env.get(expr) {
                return val.as_root();
            }
            curr_env = env.into_parent();
        }
        self.get_globals()
            .get(expr)
            .expect(&format!("symbol {} not defined", self.symbols[expr]))
            .upgrade()
    }

    fn eval_list(&mut self, vals: &gc::Weak<Vec<WeakVal>>) -> RootedVal {
        if self.get_ref(vals).is_empty() {
            return RootedVal::nil(&mut self.heap);
        }
        if let Ok(val) = self.try_eval_special_form(vals) {
            return val;
        }
        let func = self.get_ref(vals)[0].clone();
        let func_evaled = self.eval_weak(&func);
        if let Ok(val) = self.try_eval_macro(func_evaled.clone(), vals) {
            return val;
        }
        let evaled = map_scoped_vec_range(self, vals, (1, 0), |vm, val| vm.eval_weak(val));
        self.call(&func_evaled, evaled)
    }

    pub fn call(&mut self, func: &RootedVal, args: Vec<RootedVal>) -> RootedVal {
        let res = match func {
            RootedVal::Lambda(lambda_ref) => {
                let (func_body, func_args, func_env, func_globals) = {
                    let func = self.get_ref(lambda_ref);
                    (
                        func.body.clone(),
                        func.args.clone(),
                        func.env.clone(),
                        func.globals.clone(),
                    )
                };
                let func_env =
                    func_call_env_with_parent(&func_args, args, func_env, &mut self.heap);
                self.with_frame(
                    FuncFrame {
                        globals: func_globals,
                        locals: Some(func_env),
                    },
                    |vm| vm.eval_weak(&func_body),
                )
            }
            RootedVal::NativeFunc(func) => self.with_frame(
                FuncFrame {
                    globals: self.get_globals(),
                    locals: None,
                },
                |vm| func(vm, args),
            ),
            _ => panic!("first symbol of a list should refer to a function"),
        };
        // If we've allocated more that 3/4th of the tracking entries
        if self.heap.taken_entries.get() > 3 * self.heap.vacant_entries.get() {
            self.run_gc();
        }
        res
    }

    fn try_eval_macro(
        &mut self,
        func_evaled: RootedVal,
        vals: &gc::Weak<Vec<WeakVal>>,
    ) -> Result<RootedVal, ()> {
        if let RootedVal::Macro(macro_body) = &func_evaled {
            let mut vals = vals.clone();
            let macro_args = map_scoped_vec_range(self, &vals, (1, 0), |_, val| val.as_root());
            let expanded = self.expand_macro(macro_body, macro_args);
            let res = self.eval(&expanded);
            use RootedVal::*;
            match &expanded {
                List(inner) => {
                    let replace_with = self.get_ref(inner).clone();
                    let mut code_ref = self.get_mut(&mut vals);
                    code_ref.clone_from(&replace_with);
                }
                Symbol(_) | NumberVal(_) | StringVal(_) => {
                    let wrapped = self.heap.allocate(vec![
                        WeakVal::Symbol(BuiltinSymbols::Identity as SymbolId),
                        expanded.as_weak(),
                    ]);
                    let replace_with = self.get_ref(&wrapped).clone();
                    let mut code_ref = self.get_mut(&mut vals);
                    code_ref.clone_from(&replace_with);
                }
                Macro(_) | UserType(_) | NativeFunc(_) | Lambda(_) | Boxed { .. } => {
                    panic!("Macro invocation returned value that cannot be evaluated")
                }
            }
            Ok(res)
        } else {
            Err(())
        }
    }

    fn expand_macro(&mut self, macro_body: &gc::Root<Lambda>, args: Vec<RootedVal>) -> RootedVal {
        let (func_body, func_args, func_globals) = {
            let func = self.get_ref(macro_body);
            (func.body.clone(), func.args.clone(), func.globals.clone())
        };
        let func_env = func_call_env(&func_args, args, &mut self.heap);
        self.with_frame(
            FuncFrame {
                globals: func_globals,
                locals: Some(func_env),
            },
            |vm| vm.eval_weak(&func_body),
        )
    }

    fn with_frame<R>(&mut self, frame: FuncFrame, func: impl FnOnce(&mut Self) -> R) -> R {
        self.call_stack.push(frame);
        let res = func(self);
        self.call_stack.pop();
        res
    }

    fn try_eval_special_form(&mut self, vals: &gc::Weak<Vec<WeakVal>>) -> Result<RootedVal, ()> {
        let symbol = match &self.get_ref(vals)[0] {
            WeakVal::Symbol(val) => *val,
            _ => return Err(()),
        };
        match symbol.try_into() {
            Ok(BuiltinSymbols::Define) => Ok(self.eval_define(vals)),
            Ok(BuiltinSymbols::Macro) => Ok(self.eval_macro(vals)),
            Ok(BuiltinSymbols::Begin) => Ok(self.eval_begin(vals)),
            Ok(BuiltinSymbols::Quote) => Ok(self.eval_quote(vals)),
            Ok(BuiltinSymbols::Quasiquote) => Ok(self.eval_quasiquote(vals)),
            Ok(BuiltinSymbols::Unquote) => panic!("Unquote in not quasiquoted context"),
            Ok(BuiltinSymbols::Lambda) => Ok(self.eval_lambda(vals)),
            Ok(BuiltinSymbols::If) => Ok(self.eval_if(vals)),
            Ok(BuiltinSymbols::While) => Ok(self.eval_while(vals)),
            Ok(BuiltinSymbols::Set) => Ok(self.eval_set(vals)),
            Ok(BuiltinSymbols::Let) => Ok(self.eval_let(vals)),
            Ok(BuiltinSymbols::Export) => Ok(self.eval_export(vals)),
            Ok(BuiltinSymbols::Splice) => panic!("unquote-splicing is not quasiquote context"),
            Ok(BuiltinSymbols::BoxRef) => Ok(self.eval_box_ref(vals)),
            _ => Err(()),
        }
    }

    fn eval_define(&mut self, vals: &gc::Weak<Vec<WeakVal>>) -> RootedVal {
        enum Pattern {
            Value {
                name: SymbolId,
                val: WeakVal,
            },
            Function {
                name: SymbolId,
                args: FunctionArgs,
                body: Vec<WeakVal>,
            },
        }

        let (mut create_env, box_result) = match self.get_locals() {
            None => (self.get_globals(), true),
            Some(val) => (val.clone(), false),
        };
        let constr = {
            let vals_ref = self.get_ref(vals);
            match &vals_ref[..] {
                // variable define form
                [_, WeakVal::Symbol(name), val] => Pattern::Value {
                    name: *name,
                    val: val.clone(),
                },
                // function define form
                [_, WeakVal::List(inner), body @ ..] => {
                    if let [WeakVal::Symbol(name), args @ ..] = &self.get_ref(inner)[..] {
                        Pattern::Function {
                            name: *name,
                            args: collect_function_definition_arguments(args),
                            body: body.iter().cloned().collect(),
                        }
                    } else {
                        panic!("wrong function define form")
                    }
                }
                _ => panic!("not a define form"),
            }
        };
        match constr {
            Pattern::Value { name, ref val } => {
                let mut evaled = self.eval_weak(val);
                if box_result {
                    evaled = RootedVal::boxed_unfrozen(evaled.as_weak(), &mut self.heap);
                }
                create_env.insert_binding(name, evaled.downgrade());
            }
            Pattern::Function { name, args, body } => {
                let body = prepare_function_body(body, &mut self.heap);
                let globals = self.get_globals();
                let mut function =
                    RootedVal::function(name, args, body.downgrade(), globals, &mut self.heap);
                if box_result {
                    function = RootedVal::boxed_unfrozen(function.as_weak(), &mut self.heap);
                }
                create_env.insert_binding(name, function.downgrade());
            }
        }
        RootedVal::none()
    }

    fn eval_macro(&mut self, vals: &gc::Weak<Vec<WeakVal>>) -> RootedVal {
        struct MacroConstructor {
            pub name: SymbolId,
            pub args: FunctionArgs,
            pub body: Vec<WeakVal>,
        }
        let vals_ref = self.get_ref(vals);
        let MacroConstructor { name, args, body } = match &vals_ref[..] {
            [_, WeakVal::List(inner), body @ ..] => {
                if let [WeakVal::Symbol(name), args @ ..] = &self.get_ref(inner)[..] {
                    MacroConstructor {
                        name: *name,
                        args: collect_function_definition_arguments(args),
                        body: body.iter().cloned().collect(),
                    }
                } else {
                    panic!("wrong macro define form")
                }
            }
            _ => panic!("not a macro definition form"),
        };
        let body = prepare_function_body(body, &mut self.heap);
        let globals = self.get_globals();
        let macro_val = RootedVal::macro_val(name, args, body.downgrade(), globals, &mut self.heap);
        self.get_globals()
            .insert_binding(name, macro_val.downgrade());
        RootedVal::none()
    }

    fn eval_lambda(&mut self, vals: &gc::Weak<Vec<WeakVal>>) -> RootedVal {
        let locals = self
            .get_locals()
            .map(Environment::split)
            .unwrap_or_else(Environment::new);
        let globals = self.get_globals();
        let (args, body) = match &self.get_ref(vals)[..] {
            [_, WeakVal::List(args), body @ ..] => {
                let args = collect_function_definition_arguments(&self.get_ref(args));
                (args, body.iter().cloned().collect())
            }
            _ => panic!("not a lambda form"),
        };
        let body = prepare_function_body(body, &mut self.heap);
        RootedVal::lambda(locals, args, body.downgrade(), globals, &mut self.heap)
    }

    fn eval_begin(&mut self, vals: &gc::Weak<Vec<WeakVal>>) -> RootedVal {
        map_scoped_vec_range(self, vals, (1, 0), |vm, val| vm.eval_weak(val))
            .pop()
            .unwrap()
    }

    fn eval_quote(&mut self, vals: &gc::Weak<Vec<WeakVal>>) -> RootedVal {
        assert_eq!(
            self.get_ref(vals).len(),
            2,
            "you can only quote single expression"
        );
        // Quote is just passing unevaluated code
        // the only thing we need to do is root it.
        self.get_ref(vals)[1].as_root()
    }

    fn eval_quasiquote(&mut self, vals: &gc::Weak<Vec<WeakVal>>) -> RootedVal {
        assert_eq!(
            self.get_ref(vals).len(),
            2,
            "you can only quasiquote single expression"
        );
        let expr = self.get_ref(vals)[1].clone();
        self.quasiquote_expr(&expr)
    }

    fn quasiquote_expr(&mut self, expr: &WeakVal) -> RootedVal {
        enum Action {
            Upgrade,
            Unquote,
            Recurse,
        }
        let action = match expr {
            WeakVal::Symbol(_) | WeakVal::NumberVal(_) | WeakVal::StringVal(_) => Action::Upgrade,
            WeakVal::List(inner) if self.get_ref(inner).len() == 2 => {
                let head = &self.get_ref(inner)[0];
                if head.is_builtin_symbol(BuiltinSymbols::Unquote) {
                    Action::Unquote
                } else {
                    Action::Recurse
                }
            }
            WeakVal::List(_) => Action::Recurse,
            _ => panic!("couldn't possibly quasiquote this"),
        };
        match action {
            Action::Upgrade => expr.as_root(),
            Action::Unquote => {
                let inner = expr.as_root().as_list().unwrap();
                let raw_val = self.get_ref(&inner)[1].clone();
                self.eval_weak(&raw_val)
            }
            Action::Recurse => {
                let ptr = expr.as_root().as_list().unwrap();
                let mut res = Vec::new();
                map_scoped_vec(self, &ptr, |vm, val| match val {
                    WeakVal::List(inner) if vm.get_ref(inner).len() == 2 => {
                        match vm.handle_possible_unquote_splicing(inner) {
                            None => res.push(vm.quasiquote_expr(&val)),
                            Some(vals) => res.extend(vals),
                        }
                    }
                    _ => res.push(vm.quasiquote_expr(&val)),
                });
                RootedVal::list_from_rooted(res, &mut self.heap)
            }
        }
    }

    fn handle_possible_unquote_splicing(
        &mut self,
        expr: &gc::Weak<Vec<WeakVal>>,
    ) -> Option<Vec<RootedVal>> {
        let head = &self.get_ref(expr)[0];
        if !head.is_builtin_symbol(BuiltinSymbols::Splice) {
            return None;
        }
        let rest = self.get_ref(expr)[1].clone();
        let rest_evaled = self
            .eval_weak(&rest)
            .as_list()
            .expect("Unquote-splicing argument has to evaluate to a list");
        Some(
            self.get_ref(&rest_evaled)
                .iter()
                .map(WeakVal::as_root)
                .collect(),
        )
    }

    fn eval_if(&mut self, expr: &gc::Weak<Vec<WeakVal>>) -> RootedVal {
        assert_eq!(
            self.get_ref(expr).len(),
            4,
            "If has to have condition and two clauses"
        );
        let (predicate, if_true, if_false) = {
            let predicate = self.get_ref(expr)[1].clone();
            let if_true = self.get_ref(expr)[2].clone();
            let if_false = self.get_ref(expr)[3].clone();
            (predicate, if_true, if_false)
        };
        let res = self.eval_weak(&predicate);
        let eval_next = match res {
            RootedVal::Symbol(val) if val == BuiltinSymbols::True as SymbolId => if_true,
            RootedVal::Symbol(val) if val == BuiltinSymbols::False as SymbolId => if_false,
            _ => panic!("If predicate needs to eval to #t or #f"),
        };
        self.eval_weak(&eval_next)
    }

    fn eval_while(&mut self, expr: &gc::Weak<Vec<WeakVal>>) -> RootedVal {
        assert!(
            self.get_ref(expr).len() > 2,
            "while expr needs at least 2 clauses"
        );
        let cond = self.get_ref(expr)[1].clone();
        while self
            .eval_weak(&cond)
            .is_builtin_symbol(BuiltinSymbols::True)
        {
            map_scoped_vec_range(self, expr, (2, 0), |vm, val| vm.eval_weak(val));
        }
        RootedVal::none()
    }

    fn eval_set(&mut self, expr: &gc::Weak<Vec<WeakVal>>) -> RootedVal {
        assert_eq!(self.get_ref(expr).len(), 3, "set form takes 2 arguments");
        let location = self.get_ref(expr)[1].clone();
        let val = self.get_ref(expr)[2].clone();
        let val = self.eval_weak(&val);
        match &location {
            WeakVal::Symbol(inner) => {
                let mut containing_env = self.get_globals();
                let mut curr_env = self.get_locals();
                while let Some(env) = curr_env {
                    if env.borrow().values.contains_key(inner) {
                        containing_env = env;
                        break;
                    }
                    curr_env = env.into_parent();
                }
                let mut env_borrow = containing_env.borrow_mut();
                match env_borrow.values.get_mut(inner) {
                    Some(WeakVal::Boxed { inner, auto_deref }) if *auto_deref => {
                        *self.get_mut(inner) = val.as_weak()
                    }
                    Some(entry @ _) => *entry = val.as_weak(),
                    None => panic!("Trying to set! not defined value"),
                }
            }
            WeakVal::List(inner) => {
                let (list_expr, index_expr) = match &self.get_ref(inner)[..] {
                    [list_expr, index_expr] => (list_expr.clone(), index_expr.clone()),
                    _ => panic!("Invalid list set!"),
                };
                if list_expr.is_builtin_symbol(BuiltinSymbols::BoxRef) {
                    let mut boxed = self.eval_weak(&index_expr);
                    boxed.set_box(val.as_weak(), &mut self.heap);
                    return RootedVal::none();
                }
                let (mut list, index) = (
                    self.eval_weak(&list_expr)
                        .as_list()
                        .expect("set! form list expected as first argument"),
                    self.eval_weak(&index_expr)
                        .as_number()
                        .expect("set! form number expected as second argument")
                        as usize,
                );
                let len = self.get_ref(&list).len();
                if index >= len {
                    panic!("Trying to set list location past its size")
                }
                self.get_mut(&mut list)[index] = val.as_weak();
            }
            _ => panic!("Invalid set!"),
        }
        RootedVal::none()
    }

    fn eval_let(&mut self, expr: &gc::Weak<Vec<WeakVal>>) -> RootedVal {
        let size = self.get_ref(expr).len();
        assert!(size > 2, "let form needs at least 2 arguments");
        let bindings = self.get_ref(expr)[1]
            .as_root()
            .as_list()
            .expect("Let binding expected to be a list");
        let mut let_env: Environment = map_scoped_vec(self, &bindings, |vm, binding| {
            let (name, body) = vm
                .extract_pair(binding)
                .expect("Expected binding to be a pair");
            (
                name.as_root()
                    .as_symbol()
                    .expect("First binding element should be a symbol"),
                vm.eval_weak(&body),
            )
        })
        .into_iter()
        .into();
        let curr_frame = self.get_frame_mut();
        match &curr_frame.locals {
            None => curr_frame.locals = Some(let_env.clone()),
            Some(env) => {
                let_env.reparent(env.clone());
                curr_frame.locals = Some(let_env.clone())
            }
        }
        // eval body
        let mut results = map_scoped_vec_range(self, expr, (2, 0), |vm, val| vm.eval_weak(val));
        let res = results
            .pop()
            .expect("Let evaluation should not yield empty results");
        // pop local env
        self.get_frame_mut().locals = let_env.into_parent();
        res
    }

    fn eval_export(&mut self, expr: &gc::Weak<Vec<WeakVal>>) -> RootedVal {
        assert!(
            self.get_locals().is_none(),
            "You can only export in top level"
        );
        let exports: Vec<_> = self.get_ref(expr).iter().skip(1).cloned().collect();
        for export in exports {
            match export {
                WeakVal::Symbol(s) => self.get_globals().export(s),
                _ => panic!("Expected identifier in export form"),
            }
        }
        RootedVal::none()
    }

    fn eval_box_ref(&mut self, expr: &gc::Weak<Vec<WeakVal>>) -> RootedVal {
        assert!(self.get_ref(expr).len() == 2, "box-ref expects 1 argument");
        let boxed_val = self.get_ref(expr).get(1).unwrap().clone();
        let mut res = self._eval_weak_do_not_auto_deref_box(&boxed_val);
        if let RootedVal::Boxed { inner, .. } = &mut res {
            RootedVal::Boxed {
                inner: inner.clone(),
                auto_deref: false,
            }
        } else {
            panic!("box-ref argument needs to be a box")
        }
    }

    #[inline]
    fn extract_pair(&self, pair: &WeakVal) -> Option<(WeakVal, WeakVal)> {
        let pair = pair.as_root().as_list()?;
        match &self.get_ref(&pair)[..] {
            [a, b] => Some((a.clone(), b.clone())),
            _ => None,
        }
    }

    #[inline]
    pub fn get_ref<'a, T>(&'a self, ptr: &'a impl ScopedRef<T>) -> ScopedPtr<T> {
        self.heap.deref_ptr(ptr)
    }

    #[inline]
    pub fn get_mut<'a, T>(&'a mut self, ptr: &'a mut impl ScopedRef<T>) -> ScopedMutPtr<T> {
        self.heap.deref_ptr_mut(ptr)
    }

    #[inline]
    pub fn get_frame(&self) -> &FuncFrame {
        self.call_stack.last().unwrap()
    }

    #[inline]
    pub fn get_frame_mut(&mut self) -> &mut FuncFrame {
        self.call_stack.last_mut().unwrap()
    }

    #[inline]
    pub fn get_globals(&self) -> Env {
        self.get_frame().globals.clone()
    }

    #[inline]
    pub fn get_locals(&self) -> Option<Env> {
        self.get_frame().locals.clone()
    }

    pub fn run_gc(&mut self) {
        self.gc
            .step(&mut self.heap, &mut self.call_stack, &mut self.modules);
    }

    pub fn get_value(&mut self, name: &str) -> Option<RootedVal> {
        let symbol = self.symbols.get(name)?;
        Some(self.eval_symbol(symbol))
    }
}

#[inline]
fn func_call_env(args: &FunctionArgs, values: Vec<RootedVal>, heap: &mut Heap) -> Environment {
    let func_env = Environment::new();
    populate_env(func_env, args, values, heap)
}

#[inline]
fn func_call_env_with_parent(
    args: &FunctionArgs,
    values: Vec<RootedVal>,
    parent: Environment,
    heap: &mut Heap,
) -> Environment {
    let func_env = Environment::with_parent(parent);
    populate_env(func_env, args, values, heap)
}

fn populate_env(
    mut env: Environment,
    args: &FunctionArgs,
    values: Vec<RootedVal>,
    heap: &mut Heap,
) -> Environment {
    assert!(
        values.len() >= args.positional.len(),
        "Not enough call function args"
    );
    {
        for i in 0..args.positional.len() {
            env.insert_binding(args.positional[i].clone(), values[i].as_weak());
        }
        let rest_start_index = args.positional.len();
        let has_values_for_rest = values.len() > rest_start_index;
        match &args.rest {
            Some(name) if has_values_for_rest => {
                let rest_arg_value = values[rest_start_index..].iter().cloned().collect();
                env.insert_binding(
                    *name,
                    RootedVal::list_from_rooted(rest_arg_value, heap).as_weak(),
                );
            }
            Some(name) => {
                env.insert_binding(*name, RootedVal::nil(heap).as_weak());
            }
            None if has_values_for_rest => panic!("Call supplied more arguments than expected"),
            None => (),
        }
    }
    env
}

fn collect_function_definition_arguments(args: &[WeakVal]) -> FunctionArgs {
    let mut function_args = FunctionArgs {
        positional: Vec::with_capacity(2),
        rest: None,
    };
    let mut rest_arg = false;
    for arg in args {
        if let WeakVal::Symbol(name) = arg {
            if *name == BuiltinSymbols::Dot as usize {
                rest_arg = true;
                break;
            }
            function_args.positional.push(*name);
        }
    }
    if rest_arg {
        match args.last() {
            Some(WeakVal::Symbol(name)) if *name == BuiltinSymbols::Dot as usize => {
                panic!("Rest function arg name missing")
            }
            Some(WeakVal::Symbol(name)) => function_args.rest = Some(*name),
            Some(_) => panic!("Function rest arg name has to be a symbol"),
            None => unreachable!("To get there there has to be at least one symbol"),
        }
    }
    function_args
}

fn prepare_function_body(mut body: Vec<WeakVal>, heap: &mut Heap) -> RootedVal {
    assert!(!body.is_empty(), "function body cannot be empty");
    if body.len() == 1 {
        body.pop().unwrap().upgrade()
    } else {
        let mut inner = vec![WeakVal::Symbol(BuiltinSymbols::Begin as SymbolId)];
        let iter = body.into_iter().map(|x| x.clone());
        inner.extend(iter);
        RootedVal::list(inner, heap)
    }
}

fn map_scoped_vec_range<Ptr, Func, Res>(
    vm: &mut Interpreter,
    ptr: &Ptr,
    range: (usize, usize),
    mut func: Func,
) -> Vec<Res>
where
    Ptr: ScopedRef<Vec<WeakVal>>,
    Func: FnMut(&mut Interpreter, &WeakVal) -> Res,
{
    let len = vm.get_ref(ptr).len();
    let mut res = Vec::with_capacity(len - range.0 - range.1);
    let size = vm.get_ref(ptr).len();
    for i in range.0..(size - range.1) {
        let raw_val = {
            let val = &vm.get_ref(ptr)[i];
            val.clone()
        };
        res.push(func(vm, &raw_val));
    }
    res
}

#[inline]
fn map_scoped_vec<Ptr, Func, Res>(vm: &mut Interpreter, ptr: &Ptr, func: Func) -> Vec<Res>
where
    Ptr: ScopedRef<Vec<WeakVal>>,
    Func: FnMut(&mut Interpreter, &WeakVal) -> Res,
{
    map_scoped_vec_range(vm, ptr, (0, 0), func)
}

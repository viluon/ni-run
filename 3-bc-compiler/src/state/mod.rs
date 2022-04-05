// Rust State monad taken from
// https://reddit.com/r/rust/comments/2pn9sk/comment/cmy98t8/

struct State<'a, S, A> {
    // `Fn(S) -> (A, S)` is a trait for environment-immutable closures.
    // others include `FnMut` (environment-mutable) and `FnOnce` (can only be called once);
    // this is very similar (and in fact, equivalent) to `&self`, `&mut self` and `self` methods respectively.
    // `Box<...>` is required for making it a concrete (sized) type, allowing it to be stored to the struct.
    // `+ 'a` is required since the trait can contain references (similar to `|...|: 'a -> ...` in the boxed closure).
    run_state: Box<dyn Fn(S) -> (A, S) + 'a>
}

impl<'a, S, A> State<'a, S, A> {
    fn flat_map<'b, B, F>(&'b self, f: F) -> State<'b, S, B>
    where F: Fn(A) -> State<'b, S, B> + 'b {
        State {
            // `move |...| { ... }` means that the closure moves its environments into itself,
            // this is required since we lose `f` after the return.
            // the borrowing counterpart is called `ref |...| { ... }`, and a bare `|...| { ... }` will be inferred to one of both.
            run_state: Box::new(move |first_state| {
                // currently there is a caveat for calling new closures in a box:
                // you cannot directly use the call syntax. you need to explicitly write the method name out.
                // also note the "weird" tuple construction, this makes one-element tuple.
                let (result, next_state) = self.run_state.call((first_state,));

                f(result).run_state.call((next_state,))
            })
        }
    }
}

shqld/llvm
==========

A rusty safe wrapper for [llvm-sys](https://gitlab.com/taricorp/llvm-sys.rs) with a focus on achieving higher safety and usability than [inkwell](https://github.com/TheDan64/inkwell).


Install
-------

```Cargo.toml
[dependencies]
llvm = { git = "https://github.com/shqld/llvm.git" }
```

Usage
-----

See [examples](https://github.com/shqld/llvm/tree/main/examples) for more details.

```rust
use llvm::{Context, ExecutionEngine};

let context = Context::new();
let module = context    
    .create_module("example")
    .with_source_file_name("example.ts");
let function = module
    .create_function("main", context.i32_ty(), &[], false)
    .build(|builder| {
        builder.build_ret(&context.const_i32(42));
        Ok(())
    })
    .unwrap();

assert_eq!(
    module.to_string().trim(),
    r#"
; ModuleID = 'example'
source_filename = "example.ts"

define i32 @main() {
  ret i32 42
}
    "#.trim()
);

let engine = ExecutionEngine::new_mc_jit_compiler(module).unwrap();
let result = unsafe { engine.run(&[], &[]).unwrap() };

assert_eq!(result, 42);

```
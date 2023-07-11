use llvm::{Context, ExecutionEngine};

fn main() -> Result<(), String> {
	let context = Context::new();
	let module = context.create_module("example");

	let engine = ExecutionEngine::new_mc_jit_compiler(module).unwrap();
	let module = engine.get_module("example").unwrap();

	{
		let function = module.create_function(
			"sum",
			context.i32_ty(),
			&[context.i32_ty(), context.i32_ty()],
			false,
		);
		let entry = function.entry_basic_block();
		let builder = entry.new_builder();
		let x = function.get_param(0).unwrap();
		let y = function.get_param(1).unwrap();

		builder.build_ret(&builder.build_add(&x, &y));

		assert!(function.is_valid());

		let f = unsafe {
			engine
				.get_compiled_function::<fn(i32, i32) -> i32>("sum")
				.unwrap()
		};

		assert_eq!(
			//
			f(40, 2),
			42
		);
	}

	let module = context.create_module("example2");

	let engine = ExecutionEngine::new_mc_jit_compiler(module).unwrap();
	let module = engine.get_module("example2").unwrap();

	{
		let function = module
			.create_function(
				"main",
				context.i32_ty(),
				&[
					context.i32_ty(),
					context.ptr_ty(context.ptr_ty(context.i8_ty())),
				],
				false,
			)
			.build(|builder| {
				builder.build_ret(&context.const_i32(42));
				Ok(())
			})
			.unwrap();

		assert!(function.is_valid());

		assert_eq!(unsafe { engine.run(&[], &[]).unwrap() }, 42);
	}

	Ok(())
}

#[test]
fn test() {
	main().unwrap();
}

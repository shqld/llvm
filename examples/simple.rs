use llvm::{named, Context};

fn main() -> Result<(), String> {
	let context = Context::new();
	let module = context
		.create_module("example")
		.with_source_file_name("example.ts");

	let function = module.create_function(
		"calc",
		context.i32_ty(),
		&[context.i32_ty(), context.i32_ty()],
		false,
	);

	let entry = function.entry_basic_block();
	let builder = entry.new_builder();

	let di = module.new_debug_info_builder();

	let file = di.build_file("example.ts", "dir");
	let loc = di.build_location(
		1,
		1,
		di.build_subprogram(
			&file, "func", "func", &file, 1, &file, false, true, 1, 1, false,
		),
		None,
	);

	let ret_ptr = builder
		.build_alloca(context.i32_ty())?
		.with_name("ret")
		.with_debug_loc(&loc);

	let mut x = function.get_param(0).unwrap();
	x.set_name("x");
	let mut y = function.get_param(1).unwrap();
	y.set_name("y");

	// == `let x_plus_y = builder.build_add(&x, &y).with_name("x_plus_y");`
	named!(let x_plus_y = builder.build_add(&x, &y));

	// == `let mut x_plus_y_plus_42 = builder.build_add(&x_plus_y, &context.const_i32(42)).with_name("x_plus_y_plus_42");`
	named!(let mut x_plus_y_plus_42 = builder.build_add(&x_plus_y, &context.const_i32(42)));
	x_plus_y_plus_42.set_debug_loc(&loc);

	builder
		.build_store(&x_plus_y_plus_42, &ret_ptr)?
		.with_debug_loc(&loc);

	let ret = builder.build_load(&ret_ptr)?;
	builder.build_ret(&ret);

	module.validate()?;

	Ok(())
}

#[test]
fn test() {
	main().unwrap();
}

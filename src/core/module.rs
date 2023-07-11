use std::{cell::Cell, fmt::Display, marker::PhantomData};

use llvm_sys::{
	analysis::{LLVMVerifierFailureAction, LLVMVerifyModule},
	bit_writer::LLVMWriteBitcodeToFile,
	core::*,
	LLVMModule,
};

use crate::{
	ffi::{from_cstr, nullstr, to_cstring},
	types::FunctionType,
	Context, DebugInfoBuilder, Function, Global, LLVMRef, Type,
};

#[derive(Debug, PartialEq, Eq)]
pub struct Module<'ctx> {
	raw: *mut LLVMModule,
	_marker: PhantomData<Cell<&'ctx Context>>,
}

unsafe impl Sync for Module<'_> {}
unsafe impl Send for Module<'_> {}

impl Display for Module<'_> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(
			f,
			"{}",
			from_cstr(unsafe { LLVMPrintModuleToString(self.as_mut_ptr()) })
		)
	}
}

impl Drop for Module<'_> {
	fn drop(&mut self) {
		unsafe { LLVMDisposeModule(self.as_mut_ptr()) }
	}
}

impl LLVMRef<LLVMModule> for Module<'_> {
	unsafe fn as_mut_ptr(&self) -> *mut LLVMModule {
		self.raw
	}
}

impl<'ctx> Module<'ctx> {
	pub fn new(context: &'ctx Context) -> Module<'ctx> {
		let raw = unsafe { LLVMModuleCreateWithNameInContext(nullstr, context.as_mut_ptr()) };

		Module {
			raw,
			_marker: PhantomData,
		}
	}

	pub fn module_id(&self) -> &str {
		let mut len: usize = 0;
		from_cstr(unsafe { LLVMGetModuleIdentifier(self.as_mut_ptr(), &mut len) })
	}

	pub fn set_module_id(&mut self, module_id: &str) {
		unsafe {
			LLVMSetModuleIdentifier(
				self.as_mut_ptr(),
				module_id.as_ptr() as *const _,
				module_id.len(),
			)
		}
	}

	pub fn source_file_name(&self) -> &str {
		let mut len: usize = 0;
		from_cstr(unsafe { LLVMGetSourceFileName(self.as_mut_ptr(), &mut len) })
	}

	pub fn set_source_file_name(&mut self, source_file_name: &str) {
		unsafe {
			LLVMSetSourceFileName(
				self.as_mut_ptr(),
				source_file_name.as_ptr() as *const _,
				source_file_name.len(),
			)
		}
	}

	pub fn with_module_id(mut self, module_id: &str) -> Self {
		self.set_module_id(module_id);
		self
	}

	pub fn with_source_file_name(mut self, source_file_name: &str) -> Self {
		self.set_source_file_name(source_file_name);
		self
	}

	/// # Examples
	///
	/// ```
	/// use llvm::{Context};
	///
	/// let context = Context::new();
	/// let mut module = context.create_module("");
	///
	/// assert_eq!(module.data_layout(), "");
	///
	/// module.set_data_layout("e-m:o-i64:64-i128:128-n32:64-S128");
	/// assert_eq!(module.data_layout(), "e-m:o-i64:64-i128:128-n32:64-S128");
	/// ````
	pub fn data_layout(&self) -> &str {
		from_cstr(unsafe { LLVMGetDataLayoutStr(self.as_mut_ptr()) })
	}

	/// # Examples
	///
	/// ```
	/// use llvm::{Context};
	///
	/// let context = Context::new();
	/// let mut module = context.create_module("");
	///
	/// assert_eq!(module.data_layout(), "");
	///
	/// module.set_data_layout("e-m:o-i64:64-i128:128-n32:64-S128");
	/// assert_eq!(module.data_layout(), "e-m:o-i64:64-i128:128-n32:64-S128");
	/// ````
	pub fn set_data_layout(&mut self, data_layout: &str) {
		unsafe {
			LLVMSetDataLayout(self.as_mut_ptr(), to_cstring(data_layout).as_ptr());
		}
	}

	/// # Examples
	///
	/// ```
	/// use llvm::{Context};
	///
	/// let context = Context::new();
	/// let module = context.create_module("").with_data_layout("e-m:o-i64:64-i128:128-n32:64-S128");
	///
	/// assert_eq!(module.data_layout(), "e-m:o-i64:64-i128:128-n32:64-S128");
	/// ```
	pub fn with_data_layout(mut self, data_layout: &str) -> Self {
		self.set_data_layout(data_layout);
		self
	}

	/// # Examples
	///
	/// ```
	/// use llvm::{Context};
	///
	/// let context = Context::new();
	/// let mut module = context.create_module("");
	///
	/// assert_eq!(module.target_triple(), "");
	///
	/// module.set_target_triple("x86_64-unknown-linux-gnu");
	/// assert_eq!(module.target_triple(), "x86_64-unknown-linux-gnu");
	/// ```
	pub fn target_triple(&self) -> &str {
		from_cstr(unsafe { LLVMGetTarget(self.as_mut_ptr()) })
	}

	/// # Examples
	///
	/// ```
	/// use llvm::{Context};
	///
	/// let context = Context::new();
	/// let mut module = context.create_module("");
	///
	/// assert_eq!(module.target_triple(), "");
	///
	/// module.set_target_triple("x86_64-unknown-linux-gnu");
	/// assert_eq!(module.target_triple(), "x86_64-unknown-linux-gnu");
	/// ```
	pub fn set_target_triple(&mut self, target_triple: &str) {
		unsafe {
			LLVMSetTarget(self.as_mut_ptr(), to_cstring(target_triple).as_ptr());
		}
	}

	/// # Examples
	///
	/// ```
	/// use llvm::{Context};
	///
	/// let context = Context::new();
	/// let module = context.create_module("").with_target_triple("x86_64-unknown-linux-gnu");
	///
	/// assert_eq!(module.target_triple(), "x86_64-unknown-linux-gnu");
	/// ```
	pub fn with_target_triple(mut self, target_triple: &str) -> Self {
		self.set_target_triple(target_triple);
		self
	}

	pub fn to_file(&self, path: &str) -> Result<(), &str> {
		let result =
			unsafe { LLVMWriteBitcodeToFile(self.as_mut_ptr(), to_cstring(path).as_ptr()) };

		if result == 0 {
			Ok(())
		} else {
			Err("failed")
		}
	}

	/// # Examples
	///
	/// ```
	/// use llvm::{Context, Module};
	///
	/// let context = Context::new();
	/// let module = context.create_module("");
	///
	/// module.add_global(context.i32_ty(), "test");
	/// assert_eq!(module.to_string(), "\n@test = external global i32\n")
	/// ```
	pub fn add_global<'mdl>(&'mdl self, ty: Type<'ctx>, name: &str) -> Global<'ctx, 'mdl> {
		Global::new(unsafe {
			LLVMAddGlobal(
				self.as_mut_ptr(),
				ty.as_mut_ptr(),
				to_cstring(name).as_ptr(),
			)
		})
	}

	pub fn create_function(
		&self,
		name: &str,
		ret_ty: Type<'ctx>,
		param_tys: &[Type<'ctx>],
		is_var_args: bool,
	) -> Function<'ctx, '_> {
		let mut param_tys = param_tys
			.iter()
			.map(|t| unsafe { t.as_mut_ptr() })
			.collect::<Vec<_>>();

		let ty = FunctionType::new(unsafe {
			LLVMFunctionType(
				ret_ty.as_mut_ptr(),
				param_tys.as_mut_ptr(),
				param_tys.len() as u32,
				is_var_args as i32,
			)
		});

		Function::new(self, &ty).with_name(name)
	}

	pub fn new_debug_info_builder(&self) -> DebugInfoBuilder<'ctx, '_> {
		DebugInfoBuilder::new(self)
	}

	pub fn validate(&self) -> Result<(), &str> {
		let mut out_message = std::mem::MaybeUninit::uninit();

		let code = unsafe {
			LLVMVerifyModule(
				self.as_mut_ptr(),
				LLVMVerifierFailureAction::LLVMReturnStatusAction,
				out_message.as_mut_ptr(),
			)
		};

		if code == 1 {
			let out_message = unsafe { out_message.assume_init() };
			if out_message.is_null() {
				panic!("Module is not valid");
			} else {
				Err(from_cstr(out_message))
			}
		} else {
			Ok(())
		}
	}
}

#[cfg(test)]
mod tests {
	use llvm_sys::core::*;

	use crate::{Context, LLVMRef, Module};

	#[test]
	fn new() {
		let context = Context::new();
		let module = Module::new(&context);

		assert!(unsafe { LLVMGetModuleContext(module.raw) == context.as_mut_ptr() });

		assert!(module.module_id() == "");
		assert!(module.source_file_name() == "");
	}

	#[test]
	fn create_modules_with_shared_context() {
		let context = Context::new();

		{
			let module1 = Module::new(&context);
			assert!(unsafe { LLVMGetModuleContext(module1.raw) == context.as_mut_ptr() });
		}

		{
			let module2 = Module::new(&context);
			assert!(unsafe { LLVMGetModuleContext(module2.raw) == context.as_mut_ptr() });
		}
	}

	#[test]
	fn module_id() {
		let context = Context::new();
		let module = Module::new(&context).with_module_id("test");

		assert_eq!(module.module_id(), "test");

		let mut module = Module::new(&context);
		module.set_module_id("test");
		assert_eq!(module.module_id(), "test");
	}

	#[test]
	fn source_file_name() {
		let context = Context::new();

		let module = Module::new(&context).with_source_file_name("test.rs");
		assert_eq!(module.source_file_name(), "test.rs");

		let mut module = Module::new(&context);
		module.set_source_file_name("test.rs");
		assert_eq!(module.source_file_name(), "test.rs");
	}

	#[test]
	fn print_module() {
		let context = Context::new();
		let mut module = Module::new(&context);
		assert_eq!(module.to_string(), "");

		module.set_module_id("test");
		assert_eq!(module.to_string(), "; ModuleID = 'test'\n");

		module.set_source_file_name("test.rs");
		assert_eq!(
			module.to_string(),
			"; ModuleID = 'test'\nsource_filename = \"test.rs\"\n"
		);
	}
}

use std::{ffi::c_char, ops::Deref};

use llvm_sys::{
	analysis::{LLVMVerifierFailureAction, LLVMVerifyFunction},
	core::*,
	LLVMValue,
};

use crate::{
	ffi::{from_cstr, nullstr},
	types::FunctionType,
	BasicBlock, Builder, Global, LLVMRef, Module, Param,
};

#[derive(Debug, PartialEq, Eq)]
pub struct Function<'ctx, 'mdl> {
	inner: Global<'ctx, 'mdl>,
}

impl<'ctx, 'mdl> Deref for Function<'ctx, 'mdl> {
	type Target = Global<'ctx, 'mdl>;

	fn deref(&self) -> &Self::Target {
		&self.inner
	}
}

impl<'ctx, 'mdl> Function<'ctx, 'mdl> {
	pub(crate) unsafe fn from_raw(raw: *mut LLVMValue) -> Self {
		Self {
			inner: Global::new(raw),
		}
	}

	pub fn new(module: &'mdl Module<'ctx>, ty: &FunctionType<'ctx>) -> Self {
		let raw = unsafe { LLVMAddFunction(module.as_mut_ptr(), nullstr, ty.as_mut_ptr()) };
		unsafe {
			LLVMAppendBasicBlockInContext(LLVMGetModuleContext(module.as_mut_ptr()), raw, nullstr)
		};

		Self {
			inner: Global::new(raw),
		}
	}

	pub fn name(&self) -> &str {
		let mut len = 0;
		from_cstr(unsafe { LLVMGetValueName2(self.as_mut_ptr(), &mut len) })
	}

	pub fn set_name(&mut self, name: &str) {
		unsafe {
			LLVMSetValueName2(
				self.as_mut_ptr(),
				name.as_ptr() as *const c_char,
				name.len(),
			)
		};
	}

	pub fn with_name(mut self, name: &str) -> Self {
		self.set_name(name);
		self
	}

	pub fn builder(&self) -> Builder<'ctx, 'mdl, '_, '_> {
		unsafe {
			let ctx = LLVMGetModuleContext(LLVMGetGlobalParent(self.as_mut_ptr()));
			let raw = LLVMCreateBuilderInContext(ctx);
			let bb = LLVMGetLastBasicBlock(self.as_mut_ptr());
			LLVMPositionBuilderAtEnd(raw, bb);
			Builder::from_raw(raw)
		}
	}

	pub fn build<F>(self, f: F) -> Result<Self, String>
	where
		F: FnOnce(Builder<'ctx, 'mdl, '_, '_>) -> Result<(), String>,
	{
		let builder = self.builder();
		f(builder)?;
		Ok(self)
	}

	/// # Example
	/// ```compile_fail
	/// use llvm::{Context, Module};
	/// let context = Context::new();
	/// let module = context.new_module();
	/// let func = module.new_function(context.i32(), &[], false);
	///
	/// func.get_param(0).unwrap().with_name("num"); // no method named `with_name` found for struct `Value`
	/// "get_param() returns Value (not reference) but actually it is reference to the C++ object";
	///
	/// ```
	pub fn get_param<'fnc>(&'fnc self, index: u32) -> Option<Param<'ctx, 'mdl, 'fnc>> {
		let raw = unsafe { LLVMGetParam(self.as_mut_ptr(), index) };

		if raw.is_null() {
			None
		} else {
			Some(Param::new(raw))
		}
	}

	pub fn entry_basic_block<'fnc>(&'fnc self) -> BasicBlock<'ctx, 'mdl, 'fnc> {
		BasicBlock::from_raw(unsafe { LLVMGetEntryBasicBlock(self.as_mut_ptr()) })
	}

	pub fn is_valid(&self) -> bool {
		unsafe {
			LLVMVerifyFunction(
				self.as_mut_ptr(),
				LLVMVerifierFailureAction::LLVMReturnStatusAction,
			) == 0
		}
	}

	pub fn eprintln_validation_error(&self) {
		let is_eprinted = unsafe {
			LLVMVerifyFunction(
				self.as_mut_ptr(),
				LLVMVerifierFailureAction::LLVMPrintMessageAction,
			) == 1
		};
		if is_eprinted {
			eprintln!();
		}
	}
}

#[cfg(test)]
mod tests {}

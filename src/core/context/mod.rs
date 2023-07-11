mod constants;
mod types;

use llvm_sys::{core::*, LLVMContext};
use types::CommonTypes;

use crate::{LLVMRef, Module, Type};

#[derive(Debug, PartialEq, Eq)]
pub struct Context {
	raw: *mut LLVMContext,
	common_types: CommonTypes,
}

unsafe impl Sync for Context {}
unsafe impl Send for Context {}

impl Default for Context {
	fn default() -> Self {
		Self::new()
	}
}

impl Drop for Context {
	fn drop(&mut self) {
		unsafe { LLVMContextDispose(self.raw) }
	}
}

impl LLVMRef<LLVMContext> for Context {
	unsafe fn as_mut_ptr(&self) -> *mut LLVMContext {
		debug_assert!(!self.raw.is_null());
		self.raw
	}
}

impl Context {
	pub(crate) fn common_types(&self) -> &CommonTypes {
		&self.common_types
	}
}

impl Context {
	pub fn new() -> Self {
		let raw = unsafe { LLVMContextCreate() };

		Self {
			raw,
			common_types: CommonTypes::new(raw),
		}
	}

	pub fn create_module(&self, module_id: &str) -> Module {
		Module::new(self).with_module_id(module_id)
	}

	pub fn get_type_by_name(&self, name: &str) -> Option<Type> {
		let name = std::ffi::CString::new(name).unwrap();
		let ty = unsafe { LLVMGetTypeByName2(self.raw, name.as_ptr()) };
		if ty.is_null() {
			None
		} else {
			Some(Type::from(ty))
		}
	}
}

#[cfg(test)]
mod tests {
	use llvm_sys::core::*;

	use crate::{Context, LLVMRef};

	#[test]
	fn new() {
		let context = Context::new();
		assert!(unsafe { context.as_mut_ptr() != LLVMGetGlobalContext() });
	}
}

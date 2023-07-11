use std::{cell::Cell, marker::PhantomData};

use llvm_sys::{core::*, LLVMBasicBlock};

use crate::{
	ffi::{from_cstr, to_cstring},
	Builder, Function, LLVMRef,
};

#[derive(Debug, PartialEq, Eq)]
pub struct BasicBlock<'ctx, 'mdl, 'fnc> {
	raw: *mut LLVMBasicBlock,
	_marker: PhantomData<Cell<&'fnc Function<'ctx, 'mdl>>>,
}

impl LLVMRef<LLVMBasicBlock> for BasicBlock<'_, '_, '_> {
	unsafe fn as_mut_ptr(&self) -> *mut LLVMBasicBlock {
		self.raw
	}
}

impl<'ctx, 'mdl, 'fnc> BasicBlock<'ctx, 'mdl, 'fnc> {
	pub(crate) fn from_raw(raw: *mut LLVMBasicBlock) -> Self {
		assert!(!raw.is_null());

		Self {
			raw,
			_marker: PhantomData,
		}
	}

	pub fn name(&self) -> &str {
		from_cstr(unsafe { LLVMGetBasicBlockName(self.raw) })
	}

	pub fn set_name(&mut self, name: &str) {
		let name = to_cstring(name);

		unsafe {
			let raw_value = LLVMBasicBlockAsValue(self.raw);
			LLVMSetValueName2(raw_value, name.as_ptr(), name.as_bytes().len())
		};
	}

	pub fn new_builder<'blk>(&'blk self) -> Builder<'ctx, 'mdl, 'fnc, 'blk> {
		Builder::new(self)
	}
}

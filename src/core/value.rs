use std::{cell::Cell, fmt::Display, marker::PhantomData};

use llvm_sys::{core::*, LLVMValue};

use crate::{ffi::from_cstr, BasicBlock, Context, DebugInfo, Function, LLVMRef, Module, Type};

// A wrapper of LLVMValueRef that is always used through Deref,
// so this does not need to have lifetime specifiers.
#[derive(Debug, PartialEq, Eq)]
pub struct Value {
	raw: *mut LLVMValue,
}

impl Display for Value {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(
			f,
			"{}",
			from_cstr(unsafe { LLVMPrintValueToString(self.as_mut_ptr()) })
		)
	}
}

impl LLVMRef<LLVMValue> for Value {
	unsafe fn as_mut_ptr(&self) -> *mut LLVMValue {
		self.raw
	}
}

impl Value {
	pub fn ty(&self) -> Type {
		unsafe { Type::from(LLVMTypeOf(self.as_mut_ptr())) }
	}
}

macro_rules! impl_value_variant {
	($Value:ty, $Variant:path) => {
		impl Display for $Value {
			fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
				write!(
					f,
					"{}",
					from_cstr(unsafe { LLVMPrintValueToString(self.as_mut_ptr()) })
				)
			}
		}

		impl std::ops::Deref for $Value {
			type Target = Value;

			fn deref(&self) -> &Self::Target {
				&self.inner
			}
		}

		impl LLVMRef<LLVMValue> for $Value {
			unsafe fn as_mut_ptr(&self) -> *mut LLVMValue {
				self.raw
			}
		}

		impl $Value {
			pub fn new(raw: *mut LLVMValue) -> Self {
				Self {
					inner: Value { raw },
					_marker: PhantomData,
				}
			}

			pub fn ty(&self) -> Type {
				unsafe { Type::from(LLVMTypeOf(self.as_mut_ptr())) }
			}
		}
	};
}

#[derive(Debug, PartialEq, Eq)]
pub struct Constant<'ctx> {
	inner: Value,
	_marker: PhantomData<Cell<&'ctx Context>>,
}

impl_value_variant!(Constant<'_>, Value::Constant);

#[derive(Debug, PartialEq, Eq)]
pub struct Global<'ctx, 'mdl> {
	inner: Value,
	_marker: PhantomData<Cell<&'mdl Module<'ctx>>>,
}

impl_value_variant!(Global<'_, '_>, Value::Global);

impl Global<'_, '_> {
	pub fn name(&self) -> &str {
		let mut len = 0;
		from_cstr(unsafe { LLVMGetValueName2(self.as_mut_ptr(), &mut len) })
	}

	pub fn set_name(&mut self, name: &str) {
		unsafe { LLVMSetValueName2(self.as_mut_ptr(), name.as_ptr() as *const _, name.len()) };
	}

	pub fn with_name(mut self, name: &str) -> Self {
		self.set_name(name);
		self
	}
}

#[derive(Debug, PartialEq, Eq)]
pub struct Param<'ctx, 'mdl, 'fnc> {
	inner: Value,
	_marker: PhantomData<Cell<&'fnc Function<'ctx, 'mdl>>>,
}

impl_value_variant!(Param<'_, '_, '_>, Value::Param);

impl Param<'_, '_, '_> {
	pub fn name(&self) -> &str {
		let mut len = 0;
		from_cstr(unsafe { LLVMGetValueName2(self.as_mut_ptr(), &mut len) })
	}

	pub fn set_name(&mut self, name: &str) {
		unsafe { LLVMSetValueName2(self.as_mut_ptr(), name.as_ptr() as *const _, name.len()) };
	}
}

#[derive(Debug, PartialEq, Eq)]
pub struct Instruction<'ctx, 'mdl, 'fnc, 'blk> {
	inner: Value,
	_marker: PhantomData<Cell<&'blk BasicBlock<'ctx, 'mdl, 'fnc>>>,
}

impl_value_variant!(Instruction<'_, '_, '_, '_>, Value::Instruction);

impl Instruction<'_, '_, '_, '_> {
	pub fn name(&self) -> &str {
		let mut len = 0;
		from_cstr(unsafe { LLVMGetValueName2(self.as_mut_ptr(), &mut len) })
	}

	pub fn set_name(&mut self, name: &str) {
		unsafe { LLVMSetValueName2(self.as_mut_ptr(), name.as_ptr() as *const _, name.len()) };
	}

	pub fn with_name(mut self, name: &str) -> Self {
		self.set_name(name);
		self
	}

	pub fn debug_loc(&self) -> Option<DebugInfo> {
		let raw = unsafe { llvm_sys::debuginfo::LLVMInstructionGetDebugLoc(self.as_mut_ptr()) };

		if raw.is_null() {
			None
		} else {
			Some(DebugInfo::new(raw))
		}
	}

	pub fn set_debug_loc(&mut self, loc: &DebugInfo) {
		unsafe {
			llvm_sys::debuginfo::LLVMInstructionSetDebugLoc(self.as_mut_ptr(), loc.as_mut_ptr())
		};
	}

	pub fn with_debug_loc(mut self, loc: &DebugInfo) -> Self {
		self.set_debug_loc(loc);
		self
	}
}

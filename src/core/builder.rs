use std::{cell::Cell, marker::PhantomData};

use llvm_sys::{core::*, LLVMBuilder, LLVMOpcode};

use crate::{ffi::nullstr, BasicBlock, Instruction, LLVMRef, Metadata, Type, Value};

#[derive(Debug, PartialEq, Eq)]
pub struct Builder<'ctx, 'mdl, 'fnc, 'blk> {
	raw: *mut LLVMBuilder,
	_marker: PhantomData<Cell<&'blk BasicBlock<'ctx, 'mdl, 'fnc>>>,
}

impl LLVMRef<LLVMBuilder> for Builder<'_, '_, '_, '_> {
	unsafe fn as_mut_ptr(&self) -> *mut LLVMBuilder {
		self.raw
	}
}

unsafe impl Sync for Builder<'_, '_, '_, '_> {}
unsafe impl Send for Builder<'_, '_, '_, '_> {}

impl<'ctx, 'mdl, 'fnc, 'blk> Builder<'ctx, 'mdl, 'fnc, 'blk> {
	pub(crate) fn new(parent: &'blk BasicBlock<'ctx, 'mdl, 'fnc>) -> Self {
		let raw = unsafe {
			LLVMCreateBuilderInContext(LLVMGetModuleContext(LLVMGetGlobalParent(
				LLVMGetBasicBlockParent(parent.as_mut_ptr()),
			)))
		};

		unsafe { LLVMPositionBuilderAtEnd(raw, parent.as_mut_ptr()) };

		Self {
			raw,
			_marker: PhantomData,
		}
	}

	pub(crate) unsafe fn from_raw(raw: *mut LLVMBuilder) -> Self {
		Self {
			raw,
			_marker: PhantomData,
		}
	}

	#[allow(clippy::not_unsafe_ptr_arg_deref)]
	pub fn set_debug_location(&self, info: Metadata) {
		unsafe { LLVMSetCurrentDebugLocation2(self.raw, info.as_mut_ptr()) }
	}
}

impl Drop for Builder<'_, '_, '_, '_> {
	fn drop(&mut self) {
		unsafe { LLVMDisposeBuilder(self.raw) }
	}
}

impl<'ctx> Builder<'ctx, '_, '_, '_> {
	pub fn build_ret_void(&self) -> Instruction {
		Instruction::new(unsafe { LLVMBuildRetVoid(self.as_mut_ptr()) })
	}

	pub fn build_ret(&self, val: &Value) -> Instruction {
		Instruction::new(unsafe { LLVMBuildRet(self.as_mut_ptr(), val.as_mut_ptr()) })
	}

	pub fn build_ret_aggregate(&self, vals: &[Value]) -> Instruction {
		let mut vals = vals
			.iter()
			.map(|v| unsafe { v.as_mut_ptr() })
			.collect::<Vec<_>>();

		Instruction::new(unsafe {
			LLVMBuildAggregateRet(self.as_mut_ptr(), vals.as_mut_ptr(), vals.len() as u32)
		})
	}

	pub fn build_br(&self, bb: &BasicBlock) -> Instruction {
		Instruction::new(unsafe { LLVMBuildBr(self.as_mut_ptr(), bb.as_mut_ptr()) })
	}

	pub fn build_cond_br(&self, cond: &Value, then: &BasicBlock, els: &BasicBlock) -> Instruction {
		Instruction::new(unsafe {
			LLVMBuildCondBr(
				self.as_mut_ptr(),
				cond.as_mut_ptr(),
				then.as_mut_ptr(),
				els.as_mut_ptr(),
			)
		})
	}

	pub fn build_switch(
		&self,
		val: &Value,
		els: &BasicBlock,
		cases: &[(Value, &BasicBlock)],
	) -> Instruction {
		let raw = unsafe {
			LLVMBuildSwitch(
				self.as_mut_ptr(),
				val.as_mut_ptr(),
				els.as_mut_ptr(),
				cases.len() as u32,
			)
		};

		for case in cases {
			unsafe { LLVMAddCase(raw, case.0.as_mut_ptr(), case.1.as_mut_ptr()) };
		}

		Instruction::new(raw)
	}

	pub fn build_indirect_br(&self, addr: &Value, dests: &[&BasicBlock]) -> Instruction {
		let raw = unsafe {
			LLVMBuildIndirectBr(self.as_mut_ptr(), addr.as_mut_ptr(), dests.len() as u32)
		};

		for dest in dests {
			unsafe { LLVMAddDestination(raw, dest.as_mut_ptr()) };
		}

		Instruction::new(raw)
	}

	pub fn build_invoke(
		&self,
		ty: impl AsRef<Type<'ctx>>,
		func: &Value,
		args: &[Value],
		then: &BasicBlock,
		catch: &BasicBlock,
	) -> Instruction {
		let mut args = args
			.iter()
			.map(|v| unsafe { v.as_mut_ptr() })
			.collect::<Vec<_>>();

		Instruction::new(unsafe {
			LLVMBuildInvoke2(
				self.as_mut_ptr(),
				ty.as_ref().as_mut_ptr(),
				func.as_mut_ptr(),
				args.as_mut_ptr(),
				args.len() as u32,
				then.as_mut_ptr(),
				catch.as_mut_ptr(),
				nullstr,
			)
		})
	}

	pub fn build_unreachable(&self) -> Instruction {
		Instruction::new(unsafe { LLVMBuildUnreachable(self.as_mut_ptr()) })
	}
}

#[derive(Debug, PartialEq, Eq)]
pub enum BinOp {
	Add,
	FAdd,
	Sub,
	FSub,
	Mul,
	FMul,
	UDiv,
	SDiv,
	FDiv,
	URem,
	SRem,
	FRem,
	Shl,
	LShr,
	AShr,
	And,
	Or,
	Xor,
}

impl From<BinOp> for LLVMOpcode {
	fn from(op: BinOp) -> Self {
		match op {
			BinOp::Add => LLVMOpcode::LLVMAdd,
			BinOp::FAdd => LLVMOpcode::LLVMFAdd,
			BinOp::Sub => LLVMOpcode::LLVMSub,
			BinOp::FSub => LLVMOpcode::LLVMFSub,
			BinOp::Mul => LLVMOpcode::LLVMMul,
			BinOp::FMul => LLVMOpcode::LLVMFMul,
			BinOp::UDiv => LLVMOpcode::LLVMUDiv,
			BinOp::SDiv => LLVMOpcode::LLVMSDiv,
			BinOp::FDiv => LLVMOpcode::LLVMFDiv,
			BinOp::URem => LLVMOpcode::LLVMURem,
			BinOp::SRem => LLVMOpcode::LLVMSRem,
			BinOp::FRem => LLVMOpcode::LLVMFRem,
			BinOp::Shl => LLVMOpcode::LLVMShl,
			BinOp::LShr => LLVMOpcode::LLVMLShr,
			BinOp::AShr => LLVMOpcode::LLVMAShr,
			BinOp::And => LLVMOpcode::LLVMAnd,
			BinOp::Or => LLVMOpcode::LLVMOr,
			BinOp::Xor => LLVMOpcode::LLVMXor,
		}
	}
}

impl Builder<'_, '_, '_, '_> {
	/// ```
	/// use llvm::{Context, Function, Module};
	///
	/// let context = Context::new();
	/// let module = context.create_module("example");
	/// let function = module.create_function("main", context.void_ty(), &[], false);
	///
	/// let entry_block = function.entry_basic_block();
	/// let builder = entry_block.new_builder();
	///
	/// builder.build_add(&context.const_i32(42), &context.const_i32(42));
	/// ```
	pub fn build_add(&self, lhs: &Value, rhs: &Value) -> Instruction {
		Instruction::new(unsafe {
			LLVMBuildAdd(
				self.as_mut_ptr(),
				lhs.as_mut_ptr(),
				rhs.as_mut_ptr(),
				nullstr,
			)
		})
	}

	pub fn build_nsw_add(&self, lhs: &Value, rhs: &Value) -> Instruction {
		Instruction::new(unsafe {
			LLVMBuildNSWAdd(
				self.as_mut_ptr(),
				lhs.as_mut_ptr(),
				rhs.as_mut_ptr(),
				nullstr,
			)
		})
	}

	pub fn build_nuw_add(&self, lhs: &Value, rhs: &Value) -> Instruction {
		Instruction::new(unsafe {
			LLVMBuildNUWAdd(
				self.as_mut_ptr(),
				lhs.as_mut_ptr(),
				rhs.as_mut_ptr(),
				nullstr,
			)
		})
	}

	pub fn build_f_add(&self, lhs: &Value, rhs: &Value) -> Instruction {
		Instruction::new(unsafe {
			LLVMBuildFAdd(
				self.as_mut_ptr(),
				lhs.as_mut_ptr(),
				rhs.as_mut_ptr(),
				nullstr,
			)
		})
	}

	pub fn build_sub(&self, lhs: &Value, rhs: &Value) -> Instruction {
		Instruction::new(unsafe {
			LLVMBuildSub(
				self.as_mut_ptr(),
				lhs.as_mut_ptr(),
				rhs.as_mut_ptr(),
				nullstr,
			)
		})
	}

	pub fn build_nsw_sub(&self, lhs: &Value, rhs: &Value) -> Instruction {
		Instruction::new(unsafe {
			LLVMBuildNSWSub(
				self.as_mut_ptr(),
				lhs.as_mut_ptr(),
				rhs.as_mut_ptr(),
				nullstr,
			)
		})
	}

	pub fn build_nuw_sub(&self, lhs: &Value, rhs: &Value) -> Instruction {
		Instruction::new(unsafe {
			LLVMBuildNUWSub(
				self.as_mut_ptr(),
				lhs.as_mut_ptr(),
				rhs.as_mut_ptr(),
				nullstr,
			)
		})
	}

	pub fn build_f_sub(&self, lhs: &Value, rhs: &Value) -> Instruction {
		Instruction::new(unsafe {
			LLVMBuildFSub(
				self.as_mut_ptr(),
				lhs.as_mut_ptr(),
				rhs.as_mut_ptr(),
				nullstr,
			)
		})
	}

	pub fn build_mul(&self, lhs: &Value, rhs: &Value) -> Instruction {
		Instruction::new(unsafe {
			LLVMBuildMul(
				self.as_mut_ptr(),
				lhs.as_mut_ptr(),
				rhs.as_mut_ptr(),
				nullstr,
			)
		})
	}

	pub fn build_nsw_mul(&self, lhs: &Value, rhs: &Value) -> Instruction {
		Instruction::new(unsafe {
			LLVMBuildNSWMul(
				self.as_mut_ptr(),
				lhs.as_mut_ptr(),
				rhs.as_mut_ptr(),
				nullstr,
			)
		})
	}

	pub fn build_nuw_mul(&self, lhs: &Value, rhs: &Value) -> Instruction {
		Instruction::new(unsafe {
			LLVMBuildNUWMul(
				self.as_mut_ptr(),
				lhs.as_mut_ptr(),
				rhs.as_mut_ptr(),
				nullstr,
			)
		})
	}

	pub fn build_f_mul(&self, lhs: &Value, rhs: &Value) -> Instruction {
		Instruction::new(unsafe {
			LLVMBuildFMul(
				self.as_mut_ptr(),
				lhs.as_mut_ptr(),
				rhs.as_mut_ptr(),
				nullstr,
			)
		})
	}

	pub fn build_u_div(&self, lhs: &Value, rhs: &Value) -> Instruction {
		Instruction::new(unsafe {
			LLVMBuildUDiv(
				self.as_mut_ptr(),
				lhs.as_mut_ptr(),
				rhs.as_mut_ptr(),
				nullstr,
			)
		})
	}

	pub fn build_exact_u_div(&self, lhs: &Value, rhs: &Value) -> Instruction {
		Instruction::new(unsafe {
			LLVMBuildExactUDiv(
				self.as_mut_ptr(),
				lhs.as_mut_ptr(),
				rhs.as_mut_ptr(),
				nullstr,
			)
		})
	}

	pub fn build_s_div(&self, lhs: &Value, rhs: &Value) -> Instruction {
		Instruction::new(unsafe {
			LLVMBuildSDiv(
				self.as_mut_ptr(),
				lhs.as_mut_ptr(),
				rhs.as_mut_ptr(),
				nullstr,
			)
		})
	}

	pub fn build_exact_s_div(&self, lhs: &Value, rhs: &Value) -> Instruction {
		Instruction::new(unsafe {
			LLVMBuildExactSDiv(
				self.as_mut_ptr(),
				lhs.as_mut_ptr(),
				rhs.as_mut_ptr(),
				nullstr,
			)
		})
	}

	pub fn build_f_div(&self, lhs: &Value, rhs: &Value) -> Instruction {
		Instruction::new(unsafe {
			LLVMBuildFDiv(
				self.as_mut_ptr(),
				lhs.as_mut_ptr(),
				rhs.as_mut_ptr(),
				nullstr,
			)
		})
	}

	pub fn build_u_rem(&self, lhs: &Value, rhs: &Value) -> Instruction {
		Instruction::new(unsafe {
			LLVMBuildURem(
				self.as_mut_ptr(),
				lhs.as_mut_ptr(),
				rhs.as_mut_ptr(),
				nullstr,
			)
		})
	}

	pub fn build_s_rem(&self, lhs: &Value, rhs: &Value) -> Instruction {
		Instruction::new(unsafe {
			LLVMBuildSRem(
				self.as_mut_ptr(),
				lhs.as_mut_ptr(),
				rhs.as_mut_ptr(),
				nullstr,
			)
		})
	}

	pub fn build_f_rem(&self, lhs: &Value, rhs: &Value) -> Instruction {
		Instruction::new(unsafe {
			LLVMBuildFRem(
				self.as_mut_ptr(),
				lhs.as_mut_ptr(),
				rhs.as_mut_ptr(),
				nullstr,
			)
		})
	}

	pub fn build_shl(&self, lhs: &Value, rhs: &Value) -> Instruction {
		Instruction::new(unsafe {
			LLVMBuildShl(
				self.as_mut_ptr(),
				lhs.as_mut_ptr(),
				rhs.as_mut_ptr(),
				nullstr,
			)
		})
	}

	pub fn build_l_shr(&self, lhs: &Value, rhs: &Value) -> Instruction {
		Instruction::new(unsafe {
			LLVMBuildLShr(
				self.as_mut_ptr(),
				lhs.as_mut_ptr(),
				rhs.as_mut_ptr(),
				nullstr,
			)
		})
	}

	pub fn build_a_shr(&self, lhs: &Value, rhs: &Value) -> Instruction {
		Instruction::new(unsafe {
			LLVMBuildAShr(
				self.as_mut_ptr(),
				lhs.as_mut_ptr(),
				rhs.as_mut_ptr(),
				nullstr,
			)
		})
	}

	pub fn build_and(&self, lhs: &Value, rhs: &Value) -> Instruction {
		Instruction::new(unsafe {
			LLVMBuildAnd(
				self.as_mut_ptr(),
				lhs.as_mut_ptr(),
				rhs.as_mut_ptr(),
				nullstr,
			)
		})
	}

	pub fn build_or(&self, lhs: &Value, rhs: &Value) -> Instruction {
		Instruction::new(unsafe {
			LLVMBuildOr(
				self.as_mut_ptr(),
				lhs.as_mut_ptr(),
				rhs.as_mut_ptr(),
				nullstr,
			)
		})
	}

	pub fn build_xor(&self, lhs: &Value, rhs: &Value) -> Instruction {
		Instruction::new(unsafe {
			LLVMBuildXor(
				self.as_mut_ptr(),
				lhs.as_mut_ptr(),
				rhs.as_mut_ptr(),
				nullstr,
			)
		})
	}

	pub fn build_bin_op(&self, op: BinOp, lhs: &Value, rhs: &Value) -> Instruction {
		Instruction::new(unsafe {
			LLVMBuildBinOp(
				self.as_mut_ptr(),
				op.into(),
				lhs.as_mut_ptr(),
				rhs.as_mut_ptr(),
				nullstr,
			)
		})
	}

	pub fn build_neg(&self, val: &Value) -> Instruction {
		Instruction::new(unsafe { LLVMBuildNeg(self.as_mut_ptr(), val.as_mut_ptr(), nullstr) })
	}

	pub fn build_nsw_neg(&self, val: &Value) -> Instruction {
		Instruction::new(unsafe { LLVMBuildNSWNeg(self.as_mut_ptr(), val.as_mut_ptr(), nullstr) })
	}

	pub fn build_nuw_neg(&self, val: &Value) -> Instruction {
		Instruction::new(unsafe { LLVMBuildNUWNeg(self.as_mut_ptr(), val.as_mut_ptr(), nullstr) })
	}

	pub fn build_f_neg(&self, val: &Value) -> Instruction {
		Instruction::new(unsafe { LLVMBuildFNeg(self.as_mut_ptr(), val.as_mut_ptr(), nullstr) })
	}

	pub fn build_not(&self, val: &Value) -> Instruction {
		Instruction::new(unsafe { LLVMBuildNot(self.as_mut_ptr(), val.as_mut_ptr(), nullstr) })
	}
}

impl<'ctx> Builder<'ctx, '_, '_, '_> {
	// pub fn LLVMBuildMalloc(
	//   arg1: LLVMBuilderRef,
	//   Ty: LLVMTypeRef,
	//   Name: *const ::libc::c_char,
	// ) -> LLVMValueRef;

	// pub fn LLVMBuildArrayMalloc(
	//   arg1: LLVMBuilderRef,
	//   Ty: LLVMTypeRef,
	//   Val: LLVMValueRef,
	//   Name: *const ::libc::c_char,
	// ) -> LLVMValueRef;

	// pub fn LLVMBuildMemSet(
	//   B: LLVMBuilderRef,
	//   Ptr: LLVMValueRef,
	//   Val: LLVMValueRef,
	//   Len: LLVMValueRef,
	//   Align: ::libc::c_uint,
	// ) -> LLVMValueRef;

	// pub fn LLVMBuildMemCpy(
	//   B: LLVMBuilderRef,
	//   Dst: LLVMValueRef,
	//   DstAlign: ::libc::c_uint,
	//   Src: LLVMValueRef,
	//   SrcAlign: ::libc::c_uint,
	//   Size: LLVMValueRef,
	// ) -> LLVMValueRef;

	// pub fn LLVMBuildMemMove(
	//   B: LLVMBuilderRef,
	//   Dst: LLVMValueRef,
	//   DstAlign: ::libc::c_uint,
	//   Src: LLVMValueRef,
	//   SrcAlign: ::libc::c_uint,
	//   Size: LLVMValueRef,
	// ) -> LLVMValueRef;

	/// # Examples
	///
	/// ```
	/// use llvm::{Context, Function, Module};
	///
	/// let context = Context::new();
	/// let module = context.create_module("example");
	/// let function = module.create_function("main", context.void_ty(), &[], false);
	///
	/// let entry_block = function.entry_basic_block();
	/// let builder = entry_block.new_builder();
	///
	/// let ptr = builder.build_alloca(context.i32_ty()).unwrap();
	/// assert_eq!(ptr.to_string(), "  %1 = alloca i32, align 4");
	/// assert_eq!(ptr.ty().to_string(), "ptr");
	///
	/// assert_eq!(
	///   builder.build_alloca(context.void_ty()),
	///   Err("Cannot allocate unsized type".to_string())
	/// );
	/// ```
	pub fn build_alloca(&self, ty: Type<'ctx>) -> Result<Instruction, String> {
		if !ty.is_sized() {
			return Err(String::from("Cannot allocate unsized type"));
		}

		Ok(Instruction::new(unsafe {
			LLVMBuildAlloca(self.as_mut_ptr(), ty.as_mut_ptr(), nullstr)
		}))
	}

	// pub fn LLVMBuildArrayAlloca(
	//   arg1: LLVMBuilderRef,
	//   Ty: LLVMTypeRef,
	//   Val: LLVMValueRef,
	//   Name: *const ::libc::c_char,
	// ) -> LLVMValueRef;

	// pub fn LLVMBuildFree(arg1: LLVMBuilderRef, PointerVal: LLVMValueRef) -> LLVMValueRef;

	pub fn build_load(&self, ptr: &Value) -> Result<Instruction, String> {
		if !ptr.ty().is_ptr_ty() {
			return Err(format!("Cannot load value from non-pointer '{}'", ptr.ty()));
		}

		// TODO: all instruction types should have concrete methods for each of their operands?
		// TODO: this is specific to the Load instruction, but when opaque pointers
		//       (created by 'context.opaque_ptr_ty()') are used,
		//       the type of the pointer is not known, so we have to get the type from args
		let ty = unsafe { LLVMTypeOf(LLVMGetOperand(ptr.as_mut_ptr(), 0)) };

		Ok(Instruction::new(unsafe {
			LLVMBuildLoad2(self.as_mut_ptr(), ty, ptr.as_mut_ptr(), nullstr)
		}))
	}

	pub fn build_store(&self, val: &Value, ptr: &Value) -> Result<Instruction, String> {
		if !ptr.ty().is_ptr_ty() {
			return Err(format!(
				"Cannot store value into non-pointer {:?}",
				ptr.ty()
			));
		}

		Ok(Instruction::new(unsafe {
			LLVMBuildStore(self.as_mut_ptr(), val.as_mut_ptr(), ptr.as_mut_ptr())
		}))
	}

	// pub fn LLVMBuildGEP2(
	//   B: LLVMBuilderRef,
	//   Ty: LLVMTypeRef,
	//   Pointer: LLVMValueRef,
	//   Indices: *mut LLVMValueRef,
	//   NumIndices: ::libc::c_uint,
	//   Name: *const ::libc::c_char,
	// ) -> LLVMValueRef;

	// pub fn LLVMBuildInBoundsGEP2(
	//   B: LLVMBuilderRef,
	//   Ty: LLVMTypeRef,
	//   Pointer: LLVMValueRef,
	//   Indices: *mut LLVMValueRef,
	//   NumIndices: ::libc::c_uint,
	//   Name: *const ::libc::c_char,
	// ) -> LLVMValueRef;

	// pub fn LLVMBuildStructGEP2(
	//   B: LLVMBuilderRef,
	//   Ty: LLVMTypeRef,
	//   Pointer: LLVMValueRef,
	//   Idx: ::libc::c_uint,
	//   Name: *const ::libc::c_char,
	// ) -> LLVMValueRef;

	// pub fn LLVMBuildGlobalString(
	//   B: LLVMBuilderRef,
	//   Str: *const ::libc::c_char,
	//   Name: *const ::libc::c_char,
	// ) -> LLVMValueRef;

	// pub fn LLVMBuildGlobalStringPtr(
	//   B: LLVMBuilderRef,
	//   Str: *const ::libc::c_char,
	//   Name: *const ::libc::c_char,
	// ) -> LLVMValueRef;
}

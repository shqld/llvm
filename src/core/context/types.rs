use llvm_sys::{core::*, LLVMContext, LLVMType};

use crate::{
	ffi::to_cstring,
	types::{
		ArrayType, FloatType, FunctionType, IntType, LabelType, PointerType, StructType,
		VectorType, VoidType,
	},
	Context, LLVMRef, Type,
};

#[derive(Debug, PartialEq, Eq)]
pub(crate) struct CommonTypes {
	void: *mut LLVMType,
	i1: *mut LLVMType,
	i8: *mut LLVMType,
	i16: *mut LLVMType,
	i32: *mut LLVMType,
	i64: *mut LLVMType,
	i128: *mut LLVMType,
	f32: *mut LLVMType,
	f64: *mut LLVMType,
}

impl CommonTypes {
	pub fn new(context: *mut LLVMContext) -> Self {
		Self {
			void: unsafe { LLVMVoidTypeInContext(context) },
			i1: unsafe { LLVMInt1TypeInContext(context) },
			i8: unsafe { LLVMInt8TypeInContext(context) },
			i16: unsafe { LLVMInt16TypeInContext(context) },
			i32: unsafe { LLVMInt32TypeInContext(context) },
			i64: unsafe { LLVMInt64TypeInContext(context) },
			i128: unsafe { LLVMInt128TypeInContext(context) },
			f32: unsafe { LLVMFloatTypeInContext(context) },
			f64: unsafe { LLVMDoubleTypeInContext(context) },
		}
	}
}

impl Context {
	/// # Examples
	///
	/// ```
	/// use llvm::Context;
	///
	/// let context = Context::new();
	/// let ty = context.void_ty();
	/// assert_eq!(ty.to_string(), "void");
	/// ```
	pub fn void_ty(&self) -> Type {
		VoidType::new(self.common_types().void).into()
	}

	/// # Examples
	///
	/// ```
	/// use llvm::Context;
	///
	/// let context = Context::new();
	/// let ty = context.i1_ty();
	/// assert_eq!(ty.to_string(), "i1");
	/// ```
	pub fn i1_ty(&self) -> Type {
		IntType::new(self.common_types().i1).into()
	}

	/// # Examples
	///
	/// ```
	/// use llvm::Context;
	///
	/// let context = Context::new();
	/// let ty = context.i8_ty();
	/// assert_eq!(ty.to_string(), "i8");
	/// ```
	pub fn i8_ty(&self) -> Type {
		IntType::new(self.common_types().i8).into()
	}

	/// # Examples
	///
	/// ```
	/// use llvm::Context;
	///
	/// let context = Context::new();
	/// let ty = context.i16_ty();
	/// assert_eq!(ty.to_string(), "i16");
	/// ```
	pub fn i16_ty(&self) -> Type {
		IntType::new(self.common_types().i16).into()
	}

	/// # Examples
	///
	/// ```
	/// use llvm::Context;
	///
	/// let context = Context::new();
	/// let ty = context.i32_ty();
	/// assert_eq!(ty.to_string(), "i32");
	/// ```
	pub fn i32_ty(&self) -> Type {
		IntType::new(self.common_types().i32).into()
	}

	/// # Examples
	///
	/// ```
	/// use llvm::Context;
	///
	/// let context = Context::new();
	/// let ty = context.i64_ty();
	/// assert_eq!(ty.to_string(), "i64");
	/// ```
	pub fn i64_ty(&self) -> Type {
		IntType::new(self.common_types().i64).into()
	}

	/// # Examples
	///
	/// ```
	/// use llvm::Context;
	///
	/// let context = Context::new();
	/// let ty = context.i128_ty();
	/// assert_eq!(ty.to_string(), "i128");
	/// ```
	pub fn i128_ty(&self) -> Type {
		IntType::new(self.common_types().i128).into()
	}

	/// # Examples
	///
	/// ```
	/// use llvm::Context;
	///
	/// let context = Context::new();
	/// let ty = context.f32_ty();
	/// assert_eq!(ty.to_string(), "float");
	/// ```
	pub fn f32_ty(&self) -> Type {
		FloatType::new(self.common_types().f32).into()
	}

	/// # Examples
	///
	/// ```
	/// use llvm::Context;
	///
	/// let context = Context::new();
	/// let ty = context.f64_ty();
	/// assert_eq!(ty.to_string(), "double");
	/// ```
	pub fn f64_ty(&self) -> Type {
		FloatType::new(self.common_types().f64).into()
	}

	/// # Examples
	///
	/// ```
	/// use llvm::Context;
	///
	/// let context = Context::new();
	/// let ty = context.int_ty(24);
	/// assert_eq!(ty.to_string(), "i24");
	/// ```
	pub fn int_ty(&self, bits: u32) -> Type {
		IntType::new(unsafe { LLVMIntTypeInContext(self.as_mut_ptr(), bits) }).into()
	}

	/// # Examples
	///
	/// ```
	/// use llvm::Context;
	///
	/// let context = Context::new();
	/// let ty = context.func_ty(
	///   context.void_ty(),
	///   &[],
	///   false
	/// );
	/// assert_eq!(ty.to_string(), "void ()");
	///
	/// let ty = context.func_ty(
	///   context.i32_ty(),
	///   &[context.i32_ty(), context.ptr_ty(context.i8_ty())],
	///   false
	/// );
	/// assert_eq!(ty.to_string(), "i32 (i32, ptr)");
	///
	/// let ty = context.func_ty(
	///   context.i32_ty(),
	///   &[context.ptr_ty(context.i8_ty())],
	///   true
	/// );
	/// assert_eq!(ty.to_string(), "i32 (ptr, ...)");
	/// ```
	pub fn func_ty<'ctx>(
		&'ctx self,
		ret: Type<'ctx>,
		params: &[Type<'ctx>],
		is_var_args: bool,
	) -> FunctionType {
		let mut params = params
			.iter()
			.map(|t| unsafe { t.as_mut_ptr() })
			.collect::<Vec<_>>();

		FunctionType::new(unsafe {
			LLVMFunctionType(
				ret.as_mut_ptr(),
				params.as_mut_ptr(),
				params.len() as u32,
				is_var_args as i32,
			)
		})
	}

	/// # Examples
	///
	/// ```
	/// use llvm::Context;
	///
	/// let context = Context::new();
	/// let ty = context.ptr_ty(context.i32_ty());
	/// assert_eq!(ty.to_string(), "ptr");
	/// ```
	pub fn ptr_ty<'ctx>(&'ctx self, ty: impl Into<Type<'ctx>>) -> Type {
		PointerType::new(unsafe { LLVMPointerType(ty.into().as_mut_ptr(), 0) }).into()
	}

	/// # Examples
	///
	/// ```
	/// use llvm::Context;
	///
	/// let context = Context::new();
	/// let ty = context.ptr_ty_with_addrspace(context.i32_ty(), 0);
	/// assert_eq!(ty.to_string(), "ptr");
	///
	/// let ty = context.ptr_ty_with_addrspace(context.i32_ty(), 1);
	/// assert_eq!(ty.to_string(), "ptr addrspace(1)");
	/// ```
	pub fn ptr_ty_with_addrspace<'ctx>(
		&'ctx self,
		ty: impl Into<Type<'ctx>>,
		addrspace: u32,
	) -> Type {
		PointerType::new(unsafe { LLVMPointerType(ty.into().as_mut_ptr(), addrspace) }).into()
	}

	/// # Examples
	///
	/// ```
	/// use llvm::Context;
	///
	/// let context = Context::new();
	/// let ty = context.opaque_ptr_ty();
	/// assert_eq!(ty.to_string(), "ptr");
	/// ```
	pub fn opaque_ptr_ty(&self) -> Type {
		PointerType::new(unsafe { LLVMPointerTypeInContext(self.as_mut_ptr(), 0) }).into()
	}

	/// # Examples
	///
	/// ```
	/// use llvm::Context;
	///
	/// let context = Context::new();
	/// let ty = context.opaque_ptr_ty_with_addrspace(0);
	/// assert_eq!(ty.to_string(), "ptr");
	///
	/// let ty = context.opaque_ptr_ty_with_addrspace(1);
	/// assert_eq!(ty.to_string(), "ptr addrspace(1)");
	/// ```
	pub fn opaque_ptr_ty_with_addrspace(&self, addrspace: u32) -> Type {
		PointerType::new(unsafe { LLVMPointerTypeInContext(self.as_mut_ptr(), addrspace) }).into()
	}

	/// # Examples
	///
	/// ```
	/// use llvm::Context;
	///
	/// let context = Context::new();
	/// let ty = context.struct_ty(
	///   &[context.i32_ty(), context.i32_ty()]
	/// );
	/// assert_eq!(ty.to_string(), "{ i32, i32 }");
	/// ```
	pub fn struct_ty(&self, element_tys: &[Type]) -> Type {
		StructType::new(unsafe {
			LLVMStructTypeInContext(
				self.as_mut_ptr(),
				element_tys
					.iter()
					.map(|ty| ty.as_mut_ptr())
					.collect::<Vec<_>>()
					.as_mut_ptr(),
				element_tys.len() as u32,
				0,
			)
		})
		.into()
	}

	/// # Examples
	///
	/// ```
	/// use llvm::Context;
	///
	/// let context = Context::new();
	/// let ty = context.packed_struct_ty(
	///   &[context.i32_ty(), context.i32_ty()]
	/// );
	/// assert_eq!(ty.to_string(), "<{ i32, i32 }>");
	/// ```
	pub fn packed_struct_ty(&self, element_tys: &[Type]) -> Type {
		StructType::new(unsafe {
			LLVMStructTypeInContext(
				self.as_mut_ptr(),
				element_tys
					.iter()
					.map(|ty| ty.as_mut_ptr())
					.collect::<Vec<_>>()
					.as_mut_ptr(),
				element_tys.len() as u32,
				1,
			)
		})
		.into()
	}

	/// # Examples
	///
	/// ```
	/// use llvm::Context;
	///
	/// let context = Context::new();
	/// let ty = context.named_struct_ty("Foo");
	/// assert_eq!(ty.to_string(), "%Foo = type opaque");
	///
	/// let ty = context.named_struct_ty("Bar").with_body(
	///   &[context.i32_ty(), context.i32_ty()],
	///   false
	/// );
	/// assert_eq!(ty.to_string(), "%Bar = type { i32, i32 }");
	/// ```
	pub fn named_struct_ty(&self, name: &str) -> StructType {
		StructType::new(unsafe {
			LLVMStructCreateNamed(self.as_mut_ptr(), to_cstring(name).as_ptr())
		})
	}

	/// # Examples
	///
	/// ```
	/// use llvm::Context;
	///
	/// let context = Context::new();
	/// let ty = context.array_ty(context.i32_ty(), 1);
	/// assert_eq!(ty.to_string(), "[1 x i32]");
	/// ```
	pub fn array_ty<'ctx>(&'ctx self, element_ty: impl Into<Type<'ctx>>, size: u32) -> Type {
		ArrayType::new(unsafe { LLVMArrayType(element_ty.into().as_mut_ptr(), size) }).into()
	}

	/// # Examples
	///
	/// ```
	/// use llvm::Context;
	///
	/// let context = Context::new();
	/// let ty = context.vector_ty(context.i32_ty(), 1);
	/// assert_eq!(ty.to_string(), "<1 x i32>");
	/// ```
	pub fn vector_ty<'ctx>(&'ctx self, element_ty: impl Into<Type<'ctx>>, size: u32) -> Type {
		VectorType::new(unsafe { LLVMVectorType(element_ty.into().as_mut_ptr(), size) }).into()
	}

	/// # Examples
	///
	/// ```
	/// use llvm::Context;
	///
	/// let context = Context::new();
	/// let ty = context.scalable_vector_ty(context.i32_ty(), 1);
	/// assert_eq!(ty.to_string(), "<vscale x 1 x i32>");
	/// ```
	pub fn scalable_vector_ty<'ctx>(
		&'ctx self,
		element_ty: impl Into<Type<'ctx>>,
		size: u32,
	) -> Type {
		VectorType::new(unsafe { LLVMScalableVectorType(element_ty.into().as_mut_ptr(), size) })
			.into()
	}

	/// # Examples
	///
	/// ```
	/// use llvm::Context;
	///
	/// let context = Context::new();
	/// let ty = context.label_ty();
	/// assert_eq!(ty.to_string(), "label");
	/// ```
	pub fn label_ty(&self) -> Type {
		LabelType::new(unsafe { LLVMLabelTypeInContext(self.as_mut_ptr()) }).into()
	}
}

#[cfg(test)]
mod tests {
	use llvm_sys::{core::*, LLVMTypeKind};

	use crate::LLVMRef;

	#[test]

	fn void() {
		let context = crate::Context::new();
		let ty = context.void_ty();

		unsafe { assert_eq!(LLVMGetTypeContext(ty.as_mut_ptr()), context.as_mut_ptr()) };
		assert_eq!(
			unsafe { LLVMGetTypeKind(ty.as_mut_ptr()) },
			LLVMTypeKind::LLVMVoidTypeKind
		);
	}

	#[test]
	fn int() {
		let context = crate::Context::new();
		let ty = context.int_ty(24);

		unsafe { assert_eq!(LLVMGetTypeContext(ty.as_mut_ptr()), context.as_mut_ptr()) };
		assert_eq!(
			unsafe { LLVMGetTypeKind(ty.as_mut_ptr()) },
			LLVMTypeKind::LLVMIntegerTypeKind
		);

		for ty in [
			context.i1_ty(),
			context.i8_ty(),
			context.i16_ty(),
			// context.i32_ty(),
			context.i64_ty(),
			context.i128_ty(),
		] {
			unsafe { assert_eq!(LLVMGetTypeContext(ty.as_mut_ptr()), context.as_mut_ptr()) };
			assert_eq!(
				unsafe { LLVMGetTypeKind(ty.as_mut_ptr()) },
				LLVMTypeKind::LLVMIntegerTypeKind
			);
		}
	}

	#[test]
	fn float() {
		let context = crate::Context::new();
		let ty = context.f32_ty();

		unsafe { assert_eq!(LLVMGetTypeContext(ty.as_mut_ptr()), context.as_mut_ptr()) };
		assert_eq!(
			unsafe { LLVMGetTypeKind(ty.as_mut_ptr()) },
			LLVMTypeKind::LLVMFloatTypeKind
		);

		let ty = context.f64_ty();

		unsafe { assert_eq!(LLVMGetTypeContext(ty.as_mut_ptr()), context.as_mut_ptr()) };
		assert_eq!(
			unsafe { LLVMGetTypeKind(ty.as_mut_ptr()) },
			LLVMTypeKind::LLVMDoubleTypeKind
		);
	}

	#[test]
	fn function() {
		let context = crate::Context::new();
		let ty = context.func_ty(context.void_ty(), &[], false);

		unsafe { assert_eq!(LLVMGetTypeContext(ty.as_mut_ptr()), context.as_mut_ptr()) };
		assert_eq!(
			unsafe { LLVMGetTypeKind(ty.as_mut_ptr()) },
			LLVMTypeKind::LLVMFunctionTypeKind
		);
	}

	#[test]
	fn pointer() {
		let context = crate::Context::new();

		for ty in [
			context.ptr_ty(context.i32_ty()),
			context.ptr_ty_with_addrspace(context.i32_ty(), 0),
			context.opaque_ptr_ty(),
			context.opaque_ptr_ty_with_addrspace(0),
		] {
			let ty = ty.as_ptr_ty().unwrap();
			unsafe { assert_eq!(LLVMGetTypeContext(ty.as_mut_ptr()), context.as_mut_ptr()) };
			assert_eq!(
				unsafe { LLVMGetTypeKind(ty.as_mut_ptr()) },
				LLVMTypeKind::LLVMPointerTypeKind
			);
			assert_eq!(ty.addrspace(), 0);
			assert!(ty.is_opaque());
		}

		for ty in [
			context.ptr_ty_with_addrspace(context.i32_ty(), 42),
			context.opaque_ptr_ty_with_addrspace(42),
		] {
			let ty = ty.as_ptr_ty().unwrap();
			unsafe { assert_eq!(LLVMGetTypeContext(ty.as_mut_ptr()), context.as_mut_ptr()) };
			assert_eq!(
				unsafe { LLVMGetTypeKind(ty.as_mut_ptr()) },
				LLVMTypeKind::LLVMPointerTypeKind
			);
			assert_eq!(ty.addrspace(), 42);
			assert!(ty.is_opaque());
		}
	}

	#[test]
	fn struct_() {
		let context = crate::Context::new();

		let ty = context.struct_ty(&[context.i32_ty(), context.i32_ty()]);

		unsafe { assert_eq!(LLVMGetTypeContext(ty.as_mut_ptr()), context.as_mut_ptr()) };
		assert_eq!(
			unsafe { LLVMGetTypeKind(ty.as_mut_ptr()) },
			LLVMTypeKind::LLVMStructTypeKind
		);

		let ty = context.packed_struct_ty(&[context.i32_ty(), context.i32_ty()]);

		unsafe { assert_eq!(LLVMGetTypeContext(ty.as_mut_ptr()), context.as_mut_ptr()) };
		assert_eq!(
			unsafe { LLVMGetTypeKind(ty.as_mut_ptr()) },
			LLVMTypeKind::LLVMStructTypeKind
		);

		let ty = context.named_struct_ty("Foo");

		unsafe { assert_eq!(LLVMGetTypeContext(ty.as_mut_ptr()), context.as_mut_ptr()) };
		assert_eq!(
			unsafe { LLVMGetTypeKind(ty.as_mut_ptr()) },
			LLVMTypeKind::LLVMStructTypeKind
		);
	}

	#[test]
	fn array() {
		let context = crate::Context::new();
		let ty = context.array_ty(context.i32_ty(), 1);

		unsafe { assert_eq!(LLVMGetTypeContext(ty.as_mut_ptr()), context.as_mut_ptr()) };
		assert_eq!(
			unsafe { LLVMGetTypeKind(ty.as_mut_ptr()) },
			LLVMTypeKind::LLVMArrayTypeKind
		);
	}

	#[test]
	fn vector() {
		let context = crate::Context::new();
		let ty = context.vector_ty(context.i32_ty(), 1);

		unsafe { assert_eq!(LLVMGetTypeContext(ty.as_mut_ptr()), context.as_mut_ptr()) };
		assert_eq!(
			unsafe { LLVMGetTypeKind(ty.as_mut_ptr()) },
			LLVMTypeKind::LLVMVectorTypeKind
		);
	}

	#[test]
	fn scalable_vector() {
		let context = crate::Context::new();
		let ty = context.scalable_vector_ty(context.i32_ty(), 1);

		unsafe { assert_eq!(LLVMGetTypeContext(ty.as_mut_ptr()), context.as_mut_ptr()) };
		assert_eq!(
			unsafe { LLVMGetTypeKind(ty.as_mut_ptr()) },
			LLVMTypeKind::LLVMScalableVectorTypeKind
		);
	}

	#[test]
	fn label() {
		let context = crate::Context::new();
		let ty = context.label_ty();

		unsafe { assert_eq!(LLVMGetTypeContext(ty.as_mut_ptr()), context.as_mut_ptr()) };
		assert_eq!(
			unsafe { LLVMGetTypeKind(ty.as_mut_ptr()) },
			LLVMTypeKind::LLVMLabelTypeKind
		);
	}
}

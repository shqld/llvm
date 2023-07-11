use std::fmt::Display;

use llvm_sys::{core::*, LLVMType, LLVMTypeKind};

use crate::{ffi::from_cstr, types::*, LLVMRef};

#[derive(Debug, PartialEq, Eq)]
pub enum Type<'ctx> {
	Void(VoidType<'ctx>),
	Int(IntType<'ctx>),
	Float(FloatType<'ctx>),
	Pointer(PointerType<'ctx>),
	Function(FunctionType<'ctx>),
	Struct(StructType<'ctx>),
	Array(ArrayType<'ctx>),
	Vector(VectorType<'ctx>),
	Label(LabelType<'ctx>),
}

impl Display for Type<'_> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(
			f,
			"{}",
			from_cstr(unsafe { LLVMPrintTypeToString(self.as_mut_ptr()) })
		)
	}
}

impl From<*mut LLVMType> for Type<'_> {
	#[allow(clippy::not_unsafe_ptr_arg_deref)]
	fn from(ty: *mut LLVMType) -> Self {
		match unsafe { LLVMGetTypeKind(ty) } {
			LLVMTypeKind::LLVMVoidTypeKind => Self::Void(VoidType::new(ty)),
			LLVMTypeKind::LLVMIntegerTypeKind => Self::Int(IntType::new(ty)),
			LLVMTypeKind::LLVMFloatTypeKind => Self::Float(FloatType::new(ty)),
			LLVMTypeKind::LLVMDoubleTypeKind => Self::Float(FloatType::new(ty)),
			LLVMTypeKind::LLVMPointerTypeKind => Self::Pointer(PointerType::new(ty)),
			LLVMTypeKind::LLVMFunctionTypeKind => Self::Function(FunctionType::new(ty)),
			LLVMTypeKind::LLVMStructTypeKind => Self::Struct(StructType::new(ty)),
			LLVMTypeKind::LLVMArrayTypeKind => Self::Array(ArrayType::new(ty)),
			LLVMTypeKind::LLVMVectorTypeKind => Self::Vector(VectorType::new(ty)),
			LLVMTypeKind::LLVMScalableVectorTypeKind => Self::Vector(VectorType::new(ty)),
			LLVMTypeKind::LLVMLabelTypeKind => Self::Label(LabelType::new(ty)),
			_ => unimplemented!("{:?}", unsafe { LLVMGetTypeKind(ty) }),
		}
	}
}

impl LLVMRef<LLVMType> for Type<'_> {
	unsafe fn as_mut_ptr(&self) -> *mut LLVMType {
		match self {
			Self::Void(ty) => ty.as_mut_ptr(),
			Self::Int(ty) => ty.as_mut_ptr(),
			Self::Float(ty) => ty.as_mut_ptr(),
			Self::Pointer(ty) => ty.as_mut_ptr(),
			Self::Function(ty) => ty.as_mut_ptr(),
			Self::Struct(ty) => ty.as_mut_ptr(),
			Self::Array(ty) => ty.as_mut_ptr(),
			Self::Vector(ty) => ty.as_mut_ptr(),
			Self::Label(ty) => ty.as_mut_ptr(),
		}
	}
}

macro_rules! impl_is_ty {
	(pub fn $method:ident(&self) -> bool, $variant:path) => {
		pub fn $method(&self) -> bool {
			matches!(self, $variant(_))
		}
	};
}

macro_rules! impl_as_ty {
	(pub fn $method:ident(&self) -> Option<&$ty:ty>, $variant:path) => {
		pub fn $method(&self) -> Option<&$ty> {
			if let $variant(ty) = self {
				Some(ty)
			} else {
				None
			}
		}
	};
}

macro_rules! impl_expect_ty {
	(pub fn $method:ident(&self) -> $ty:ty, $variant:path, $message:literal) => {
		pub fn $method(self) -> $ty {
			if let $variant(ty) = self {
				ty
			} else {
				panic!($message);
			}
		}
	};
}

impl<'ctx> Type<'ctx> {
	pub fn is_sized(&self) -> bool {
		unsafe { LLVMTypeIsSized(self.as_mut_ptr()) == 1 }
	}

	impl_is_ty!(pub fn is_void_ty(&self) -> bool, Self::Void);
	impl_is_ty!(pub fn is_int_ty(&self) -> bool, Self::Int);
	impl_is_ty!(pub fn is_float_ty(&self) -> bool, Self::Float);
	impl_is_ty!(pub fn is_ptr_ty(&self) -> bool, Self::Pointer);
	impl_is_ty!(pub fn is_func_ty(&self) -> bool, Self::Function);
	impl_is_ty!(pub fn is_struct_ty(&self) -> bool, Self::Struct);
	impl_is_ty!(pub fn is_array_ty(&self) -> bool, Self::Array);
	impl_is_ty!(pub fn is_vector_ty(&self) -> bool, Self::Vector);
	impl_is_ty!(pub fn is_label_ty(&self) -> bool, Self::Label);

	impl_as_ty!(pub fn as_void_ty(&self) -> Option<&VoidType<'ctx>>, Self::Void);
	impl_as_ty!(pub fn as_int_ty(&self) -> Option<&IntType<'ctx>>, Self::Int);
	impl_as_ty!(pub fn as_float_ty(&self) -> Option<&FloatType<'ctx>>, Self::Float);
	impl_as_ty!(pub fn as_ptr_ty(&self) -> Option<&PointerType<'ctx>>, Self::Pointer);
	impl_as_ty!(pub fn as_func_ty(&self) -> Option<&FunctionType<'ctx>>, Self::Function);
	impl_as_ty!(pub fn as_struct_ty(&self) -> Option<&StructType<'ctx>>, Self::Struct);
	impl_as_ty!(pub fn as_array_ty(&self) -> Option<&ArrayType<'ctx>>, Self::Array);
	impl_as_ty!(pub fn as_vector_ty(&self) -> Option<&VectorType<'ctx>>, Self::Vector);
	impl_as_ty!(pub fn as_label_ty(&self) -> Option<&LabelType<'ctx>>, Self::Label);

	impl_expect_ty!(pub fn expect_void_ty(&self) -> VoidType<'ctx>, Self::Void, "expect void type");
	impl_expect_ty!(pub fn expect_int_ty(&self) -> IntType<'ctx>, Self::Int, "expect int type");
	impl_expect_ty!(pub fn expect_float_ty(&self) -> FloatType<'ctx>, Self::Float, "expect float type");
	impl_expect_ty!(pub fn expect_ptr_ty(&self) -> PointerType<'ctx>, Self::Pointer, "expect ptr type");
	impl_expect_ty!(pub fn expect_func_ty(&self) -> FunctionType<'ctx>, Self::Function, "expect func type");
	impl_expect_ty!(pub fn expect_struct_ty(&self) -> StructType<'ctx>, Self::Struct, "expect struct type");
	impl_expect_ty!(pub fn expect_array_ty(&self) -> ArrayType<'ctx>, Self::Array, "expect array type");
	impl_expect_ty!(pub fn expect_vector_ty(&self) -> VectorType<'ctx>, Self::Vector, "expect vector type");
	impl_expect_ty!(pub fn expect_label_ty(&self) -> LabelType<'ctx>, Self::Label, "expect label type");
}

pub mod types {
	use std::{cell::Cell, fmt::Display, marker::PhantomData};

	use llvm_sys::{core::*, LLVMType};

	use super::Type;
	use crate::{
		ffi::{from_cstr, to_cstring},
		Context, LLVMRef,
	};

	macro_rules! impl_type_variant {
		($Name:ident, $Variant:path) => {
			#[derive(Debug, PartialEq, Eq)]
			pub struct $Name<'ctx> {
				raw: *mut LLVMType,
				_marker: std::marker::PhantomData<Cell<&'ctx Context>>,
			}

			impl Display for $Name<'_> {
				fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
					write!(
						f,
						"{}",
						from_cstr(unsafe { LLVMPrintTypeToString(self.raw) })
					)
				}
			}

			impl<'ctx> From<$Name<'ctx>> for Type<'ctx> {
				fn from(ty: $Name<'ctx>) -> Self {
					$Variant(ty)
				}
			}

			impl LLVMRef<LLVMType> for $Name<'_> {
				unsafe fn as_mut_ptr(&self) -> *mut LLVMType {
					self.raw
				}
			}

			impl<'ctx> $Name<'ctx> {
				pub fn new(raw: *mut LLVMType) -> Self {
					Self {
						raw,
						_marker: PhantomData,
					}
				}
			}
		};
	}

	impl_type_variant!(VoidType, Type::Void);
	impl_type_variant!(IntType, Type::Int);
	impl_type_variant!(FloatType, Type::Float);
	impl_type_variant!(PointerType, Type::Pointer);
	impl_type_variant!(FunctionType, Type::Function);
	impl_type_variant!(StructType, Type::Struct);
	impl_type_variant!(ArrayType, Type::Array);
	impl_type_variant!(VectorType, Type::Vector);
	impl_type_variant!(LabelType, Type::Label);

	impl PointerType<'_> {
		pub fn pointee_type(&self) -> Option<Self> {
			if self.is_opaque() {
				return None;
			}

			Some(Self::new(unsafe { LLVMGetElementType(self.raw) }))
		}

		pub fn is_opaque(&self) -> bool {
			unsafe { LLVMPointerTypeIsOpaque(self.raw) == 1 }
		}

		pub fn addrspace(&self) -> u32 {
			unsafe { LLVMGetPointerAddressSpace(self.raw) }
		}
	}

	impl StructType<'_> {
		pub fn name(&self) -> Option<&str> {
			let raw_name = unsafe { LLVMGetStructName(self.raw) };

			if raw_name.is_null() {
				None
			} else {
				Some(from_cstr(raw_name))
			}
		}

		/// # Caution
		///
		/// This method is experimental and may be removed in the future.
		///
		/// This method causes unexpected behavior when called for the same object more than once
		/// because there is no API in llvm-c for updating the name in place
		/// and this method creates a new struct type each time called.
		///
		/// ```
		/// use llvm::Context;
		///
		/// let context = Context::new();
		/// let mut struct_ty = context.struct_ty(&[]).expect_struct_ty();
		///
		/// struct_ty.set_name_experimental("Foo");
		/// assert_eq!(struct_ty.name(), Some("Foo"));
		///
		/// struct_ty.set_name_experimental("Foo");
		/// assert_eq!(struct_ty.name(), Some("Foo.0")); // the previous type remains in the context and llvm adds suffix to the new one
		/// ```
		///
		/// I'm looking for a better way to achieve this functionality, such as
		/// utilizing other APIs, adding a new API to llvm-c or using `autocxx` to interact with c++ layer.
		pub fn set_name_experimental(&mut self, name: &str) {
			let named_raw = unsafe {
				LLVMStructCreateNamed(LLVMGetTypeContext(self.raw), to_cstring(name).as_ptr())
			};

			unsafe {
				let element_types_len = LLVMCountStructElementTypes(self.raw) as usize;
				let mut element_types: Vec<*mut LLVMType> = Vec::with_capacity(element_types_len);
				LLVMGetStructElementTypes(self.raw, element_types.as_mut_ptr());
				LLVMStructSetBody(
					named_raw,
					element_types.as_mut_ptr(),
					element_types_len as u32,
					LLVMIsPackedStruct(self.raw),
				);
			}

			self.raw = named_raw;
		}

		/// # Caution
		///
		/// See [`StructType::set_name_experimental`].
		pub fn with_name_experimental(mut self, name: &str) -> Self {
			self.set_name_experimental(name);
			self
		}

		pub fn set_body(&mut self, element_tys: &[Type], packed: bool) {
			unsafe {
				LLVMStructSetBody(
					self.raw,
					element_tys
						.iter()
						.map(|ty| ty.as_mut_ptr())
						.collect::<Vec<_>>()
						.as_mut_ptr(),
					element_tys.len() as u32,
					packed as i32,
				);
			}
		}

		pub fn with_body(mut self, element_tys: &[Type], packed: bool) -> Self {
			self.set_body(element_tys, packed);
			self
		}

		pub fn is_packed(&self) -> bool {
			unsafe { LLVMIsPackedStruct(self.raw) == 1 }
		}

		pub fn is_opaque(&self) -> bool {
			unsafe { LLVMIsOpaqueStruct(self.raw) == 1 }
		}
	}

	#[cfg(test)]
	mod tests {
		mod StructType {
			use crate::Context;

			#[test]
			fn name() {
				let context = Context::new();
				let mut struct_ty = context
					.struct_ty(&[context.i32_ty(), context.i32_ty()])
					.expect_struct_ty();

				assert_eq!(struct_ty.name(), None);
				assert_eq!(struct_ty.to_string(), "{ i32, i32 }");

				struct_ty.set_name_experimental("Foo");

				assert_eq!(struct_ty.name(), Some("Foo"));
				assert_eq!(struct_ty.to_string(), "%Foo = type { i32, i32 }");

				let struct_ty = struct_ty.with_name_experimental("Bar");

				assert_eq!(struct_ty.name(), Some("Bar"));
				assert_eq!(struct_ty.to_string(), "%Bar = type { i32, i32 }");
			}

			#[test]
			fn set_body() {
				let context = Context::new();
				let mut struct_ty = context.struct_ty(&[]).expect_struct_ty();
				let mut named_struct_ty = context.named_struct_ty("Foo");

				assert_eq!(struct_ty.to_string(), "{}");
				assert_eq!(named_struct_ty.to_string(), "%Foo = type opaque");

				struct_ty.set_body(&[context.i32_ty(), context.i32_ty()], false);
				named_struct_ty.set_body(&[context.i32_ty(), context.i32_ty()], false);

				assert_eq!(struct_ty.to_string(), "{ i32, i32 }");
				assert_eq!(named_struct_ty.to_string(), "%Foo = type { i32, i32 }");
			}

			#[test]
			fn with_body() {
				let context = Context::new();
				let named_struct_ty = context
					.named_struct_ty("Foo")
					.with_body(&[context.i32_ty(), context.i32_ty()], false);

				assert_eq!(named_struct_ty.to_string(), "%Foo = type { i32, i32 }");
			}

			#[test]
			fn is_packed() {
				let context = Context::new();
				let struct_ty = context.packed_struct_ty(&[]).expect_struct_ty();
				assert!(struct_ty.is_packed());

				let mut struct_ty = context.struct_ty(&[]).expect_struct_ty();
				assert!(!struct_ty.is_packed());

				struct_ty.set_body(&[], true);
				assert!(struct_ty.is_packed());
			}

			#[test]
			fn is_opaque() {
				let context = Context::new();
				let struct_ty = context.named_struct_ty("Foo");
				assert!(struct_ty.is_opaque());

				let struct_ty = context.struct_ty(&[]).expect_struct_ty();
				assert!(!struct_ty.is_opaque());
			}
		}
	}
}

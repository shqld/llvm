use llvm_sys::core::*;

use crate::{ffi::to_cstring, types::StructType, Constant, Context, LLVMRef, Type};

impl Context {
	/// # Examples
	///
	/// ```
	/// use llvm::Context;
	///
	/// let context = Context::new();
	/// let value = context.const_null(context.i32_ty());
	/// assert_eq!(value.to_string(), "i32 0");
	///
	/// let value = context.const_null(context.f64_ty());
	/// assert_eq!(value.to_string(), "double 0.000000e+00");
	///
	/// let value = context.const_null(context.opaque_ptr_ty());
	/// assert_eq!(value.to_string(), "ptr null");
	///
	/// let value = context.const_null(context.struct_ty(&[context.i32_ty(), context.i32_ty()]));
	/// assert_eq!(value.to_string(), "{ i32, i32 } zeroinitializer");
	///
	/// let value = context.const_null(context.named_struct_ty("Foo").into());
	/// assert_eq!(value.to_string(), "%Foo zeroinitializer");
	/// ```
	pub fn const_null<'ctx>(&'ctx self, ty: Type<'ctx>) -> Constant {
		Constant::new(unsafe { LLVMConstNull(ty.as_mut_ptr()) })
	}

	/// # Examples
	///
	/// ```
	/// use llvm::Context;
	///
	/// let context = Context::new();
	/// let value = context.get_undef(context.i32_ty());
	/// assert_eq!(value.to_string(), "i32 undef");
	///
	/// let value = context.get_undef(context.f64_ty());
	/// assert_eq!(value.to_string(), "double undef");
	///
	/// let value = context.get_undef(context.opaque_ptr_ty());
	/// assert_eq!(value.to_string(), "ptr undef");
	///
	/// let value = context.get_undef(context.struct_ty(&[context.i32_ty(), context.i32_ty()]));
	/// assert_eq!(value.to_string(), "{ i32, i32 } undef");
	///
	/// let value = context.get_undef(context.named_struct_ty("Foo").into());
	/// assert_eq!(value.to_string(), "%Foo undef");
	/// ```
	pub fn get_undef<'ctx>(&'ctx self, ty: Type<'ctx>) -> Constant {
		Constant::new(unsafe { LLVMGetUndef(ty.as_mut_ptr()) })
	}

	/// # Examples
	///
	/// ```
	/// use llvm::Context;
	///
	/// let context = Context::new();
	/// let value = context.get_poison(context.i32_ty());
	/// assert_eq!(value.to_string(), "i32 poison");
	///
	/// let value = context.get_poison(context.f64_ty());
	/// assert_eq!(value.to_string(), "double poison");
	///
	/// let value = context.get_poison(context.opaque_ptr_ty());
	/// assert_eq!(value.to_string(), "ptr poison");
	///
	/// let value = context.get_poison(context.struct_ty(&[context.i32_ty(), context.i32_ty()]));
	/// assert_eq!(value.to_string(), "{ i32, i32 } poison");
	///
	/// let value = context.get_poison(context.named_struct_ty("Foo").into());
	/// assert_eq!(value.to_string(), "%Foo poison");
	/// ```
	pub fn get_poison<'ctx>(&'ctx self, ty: Type<'ctx>) -> Constant {
		Constant::new(unsafe { LLVMGetPoison(ty.as_mut_ptr()) })
	}

	/// # Examples
	///
	/// ```
	/// use llvm::Context;
	///
	/// let context = Context::new();
	/// let value = context.const_int(24, 42, false);
	/// assert_eq!(value.to_string(), "i24 42");
	/// ```
	pub fn const_int(&self, bits: u32, value: u64, sign_extend: bool) -> Constant {
		Constant::new(unsafe {
			LLVMConstInt(self.int_ty(bits).as_mut_ptr(), value, sign_extend as i32)
		})
	}

	/// # Examples
	///
	/// ```
	/// use llvm::Context;
	///
	/// let context = Context::new();
	/// let u128_num = context.const_int_with_arbitrary_precision(
	///   256, // u128 in Rust will overflow i128 in LLVM
	///   &[u128::MAX as u64, (u128::MAX >> 64) as u64]
	/// );
	/// assert_eq!(u128_num.to_string(), format!("i256 {}", u128::MAX));
	/// ```
	pub fn const_int_with_arbitrary_precision(&self, bits: u32, value: &[u64]) -> Constant {
		Constant::new(unsafe {
			LLVMConstIntOfArbitraryPrecision(
				self.int_ty(bits).as_mut_ptr(),
				value.len() as u32,
				value.as_ptr(),
			)
		})
	}

	/// # Examples
	///
	/// ```
	/// use llvm::Context;
	///
	/// let context = Context::new();
	/// let u128_num = context.const_int_from_str(
	///   256, // u128 in Rust will overflow i128 in LLVM
	///   u128::MAX.to_string().as_str(),
	///   10
	/// );
	/// assert_eq!(u128_num.to_string(), format!("i256 {}", u128::MAX));
	/// ```
	pub fn const_int_from_str(&self, bits: u32, value: &str, radix: u8) -> Constant {
		Constant::new(unsafe {
			LLVMConstIntOfString(
				self.int_ty(bits).as_mut_ptr(),
				to_cstring(value).as_ptr(),
				radix,
			)
		})
	}

	/// # Examples
	///
	/// ```
	/// use llvm::Context;
	///
	/// let context = Context::new();
	/// let value = context.const_i1(true);
	/// assert_eq!(value.to_string(), "i1 true");
	///
	/// let value = context.const_i1(false);
	/// assert_eq!(value.to_string(), "i1 false");
	/// ```
	pub fn const_i1(&self, value: bool) -> Constant {
		Constant::new(unsafe {
			LLVMConstInt(self.i1_ty().as_mut_ptr(), value as u64, false as i32)
		})
	}

	/// # Examples
	///
	/// ```
	/// use llvm::Context;
	///
	/// let context = Context::new();
	/// let value = context.const_i8(42);
	/// assert_eq!(value.to_string(), "i8 42");
	/// ```
	pub fn const_i8(&self, value: i8) -> Constant {
		Constant::new(unsafe {
			LLVMConstInt(self.i8_ty().as_mut_ptr(), value as u64, false as i32)
		})
	}

	/// # Examples
	///
	/// ```
	/// use llvm::Context;
	///
	/// let context = Context::new();
	/// let value = context.const_i16(42);
	/// assert_eq!(value.to_string(), "i16 42");
	/// ```
	pub fn const_i16(&self, value: i16) -> Constant {
		Constant::new(unsafe {
			LLVMConstInt(self.i16_ty().as_mut_ptr(), value as u64, false as i32)
		})
	}

	/// # Examples
	///
	/// ```
	/// use llvm::Context;
	///
	/// let context = Context::new();
	/// let value = context.const_i32(42);
	/// assert_eq!(value.to_string(), "i32 42");
	/// ```
	pub fn const_i32(&self, value: i32) -> Constant {
		Constant::new(unsafe {
			LLVMConstInt(self.i32_ty().as_mut_ptr(), value as u64, false as i32)
		})
	}

	/// # Examples
	///
	/// ```
	/// use llvm::Context;
	///
	/// let context = Context::new();
	/// let value = context.const_i64(42);
	/// assert_eq!(value.to_string(), "i64 42");
	/// ```
	pub fn const_i64(&self, value: i64) -> Constant {
		Constant::new(unsafe {
			LLVMConstInt(self.i64_ty().as_mut_ptr(), value as u64, false as i32)
		})
	}

	/// # Examples
	///
	/// ```
	/// use llvm::Context;
	///
	/// let context = Context::new();
	/// let value = context.const_i128(42);
	/// assert_eq!(value.to_string(), "i128 42");
	/// ```
	pub fn const_i128(&self, value: i128) -> Constant {
		self.const_int_with_arbitrary_precision(128, &[value as u64, (value >> 64) as u64])
	}

	/// # Examples
	///
	/// ```
	/// use llvm::Context;
	///
	/// let context = Context::new();
	/// let value = context.const_f32(42.0);
	/// assert_eq!(value.to_string(), "float 4.200000e+01");
	/// ```
	pub fn const_f32(&self, value: f32) -> Constant {
		Constant::new(unsafe { LLVMConstReal(self.f32_ty().as_mut_ptr(), value as f64) })
	}

	/// # Examples
	///
	/// ```
	/// use llvm::Context;
	///
	/// let context = Context::new();
	/// let value = context.const_f64(42.0);
	/// assert_eq!(value.to_string(), "double 4.200000e+01");
	/// ```
	pub fn const_f64(&self, value: f64) -> Constant {
		Constant::new(unsafe { LLVMConstReal(self.f64_ty().as_mut_ptr(), value) })
	}

	/// # Examples
	///
	/// ```
	/// use llvm::Context;
	///
	/// let context = Context::new();
	/// let value = context.const_string("Hello, world!");
	/// assert_eq!(value.to_string(), "[13 x i8] c\"Hello, world!\"");
	/// ```
	pub fn const_string(&self, value: &str) -> Constant {
		Constant::new(unsafe {
			LLVMConstString(value.as_ptr() as *const i8, value.len() as u32, 1)
		})
	}

	/// # Examples
	///
	/// ```
	/// use llvm::Context;
	///
	/// let context = Context::new();
	/// let value = context.const_string_nul_terminated("Hello, world!");
	/// assert_eq!(value.to_string(), "[14 x i8] c\"Hello, world!\\00\"");
	/// ```
	pub fn const_string_nul_terminated(&self, value: &str) -> Constant {
		Constant::new(unsafe {
			LLVMConstString(value.as_ptr() as *const i8, value.len() as u32, 0)
		})
	}

	/// # Examples
	///
	/// ```
	/// use llvm::Context;
	///
	/// let context = Context::new();
	/// let value = context.const_struct(
	///   &[context.const_i32(42), context.const_i32(42)]
	/// );
	/// assert_eq!(value.ty().to_string(), "{ i32, i32 }");
	/// assert_eq!(value.to_string(), "{ i32, i32 } { i32 42, i32 42 }");
	pub fn const_struct(&self, values: &[Constant]) -> Constant {
		Constant::new(unsafe {
			LLVMConstStruct(
				values
					.iter()
					.map(|value| value.as_mut_ptr())
					.collect::<Vec<_>>()
					.as_mut_ptr(),
				values.len() as u32,
				0,
			)
		})
	}

	/// # Examples
	///
	/// ```
	/// use llvm::Context;
	///
	/// let context = Context::new();
	/// let value = context.const_packed_struct(
	///   &[context.const_i32(42), context.const_i32(42)]
	/// );
	/// assert_eq!(value.ty().to_string(), "<{ i32, i32 }>");
	/// assert_eq!(value.to_string(), "<{ i32, i32 }> <{ i32 42, i32 42 }>");
	pub fn const_packed_struct(&self, values: &[Constant]) -> Constant {
		Constant::new(unsafe {
			LLVMConstStruct(
				values
					.iter()
					.map(|value| value.as_mut_ptr())
					.collect::<Vec<_>>()
					.as_mut_ptr(),
				values.len() as u32,
				1,
			)
		})
	}

	/// # Examples
	///
	/// ```
	/// use llvm::Context;
	///
	/// let context = Context::new();
	/// let value = context.const_named_struct(
	///   context.named_struct_ty("Foo"),
	///   &[context.const_i32(42), context.const_i32(42)]
	/// );
	/// assert_eq!(value.ty().to_string(), "%Foo = type opaque");
	/// assert_eq!(value.to_string(), "%Foo { i32 42, i32 42 }");
	///
	/// let value = context.const_named_struct(
	///   context.named_struct_ty("Bar").with_body(&[context.i32_ty()], false),
	///   &[context.const_i32(42)]
	/// );
	/// assert_eq!(value.ty().to_string(), "%Bar = type { i32 }");
	/// assert_eq!(value.to_string(), "%Bar { i32 42 }");
	/// ```
	pub fn const_named_struct(&self, ty: StructType, values: &[Constant]) -> Constant {
		Constant::new(unsafe {
			LLVMConstNamedStruct(
				ty.as_mut_ptr(),
				values
					.iter()
					.map(|value| value.as_mut_ptr())
					.collect::<Vec<_>>()
					.as_mut_ptr(),
				values.len() as u32,
			)
		})
	}

	/// # Examples
	///
	/// ```
	/// use llvm::Context;
	///
	/// let context = Context::new();
	/// let value = context.const_array(
	///   context.i32_ty(),
	///   &[context.const_i32(42), context.const_i32(42)]
	/// ).unwrap();
	/// assert_eq!(value.ty().to_string(), "[2 x i32]");
	/// assert_eq!(value.to_string(), "[2 x i32] [i32 42, i32 42]");
	///
	/// let result = context.const_array(context.i8_ty(), &[context.const_i32(42)]);
	/// assert_eq!(result, Err("each type of values must be i8".to_string()));
	/// ```
	pub fn const_array<'ctx>(
		&'ctx self,
		ty: Type<'ctx>,
		elements: &[Constant],
	) -> Result<Constant, String> {
		for element in elements.iter() {
			if unsafe { ty.as_mut_ptr() != element.ty().as_mut_ptr() } {
				return Err(format!("each type of values must be {}", ty));
			}
		}

		Ok(Constant::new(unsafe {
			LLVMConstArray(
				ty.as_mut_ptr(),
				elements
					.iter()
					.map(|element| element.as_mut_ptr())
					.collect::<Vec<_>>()
					.as_mut_ptr(),
				elements.len() as u32,
			)
		}))
	}

	/// # Examples
	///
	/// ```
	/// use llvm::Context;
	///
	/// let context = Context::new();
	/// let value = context.const_vector(
	///   context.i32_ty(),
	///   &[context.const_i32(42), context.const_i32(42)]
	/// ).unwrap();
	/// assert_eq!(value.ty().to_string(), "<2 x i32>");
	/// assert_eq!(value.to_string(), "<2 x i32> <i32 42, i32 42>");
	///
	/// let result = context.const_vector(context.i8_ty(), &[context.const_i32(42)]);
	/// assert_eq!(result, Err("each type of values must be i8".to_string()));
	/// ```
	pub fn const_vector<'ctx>(
		&'ctx self,
		ty: Type<'ctx>,
		elements: &[Constant],
	) -> Result<Constant, String> {
		for element in elements.iter() {
			if unsafe { ty.as_mut_ptr() != element.ty().as_mut_ptr() } {
				return Err(format!("each type of values must be {}", ty));
			}
		}

		Ok(Constant::new(unsafe {
			LLVMConstVector(
				elements
					.iter()
					.map(|element| element.as_mut_ptr())
					.collect::<Vec<_>>()
					.as_mut_ptr(),
				elements.len() as u32,
			)
		}))
	}
}

#[cfg(test)]
mod tests {
	use crate::Context;

	#[test]
	fn const_int() {
		let context = Context::new();

		assert_eq!(context.const_int(64, 42, false).to_string(), "i64 42");
		assert_eq!(
			context.const_int(64, -42i64 as u64, false).to_string(),
			"i64 -42"
		);
		assert_eq!(context.const_int(24, 42, false).to_string(), "i24 42");
		assert_eq!(
			context.const_int(24, !42u64 + 1, false).to_string(),
			"i24 -42"
		);
	}

	#[test]
	fn const_int_with_arbitrary_precision() {
		let context = Context::new();

		assert_eq!(
			context
				.const_int_with_arbitrary_precision(8, &[42u64])
				.to_string(),
			"i8 42"
		);
		assert_eq!(
			context
				.const_int_with_arbitrary_precision(8, &[!42u64 + 1])
				.to_string(),
			"i8 -42"
		);

		assert_eq!(
			context
				.const_int_with_arbitrary_precision(8, &[42u64, 0, 0])
				.to_string(),
			"i8 42"
		);
		assert_eq!(
			context
				.const_int_with_arbitrary_precision(8, &[!42u64 + 1, 0, 0])
				.to_string(),
			"i8 -42"
		);

		let n = i128::MAX;
		assert_eq!(
			context
				.const_int_with_arbitrary_precision(128, &[n as u64, (n >> 64) as u64])
				.to_string(),
			format!("i128 {}", n)
		);
		let n = i128::MIN;
		assert_eq!(
			context
				.const_int_with_arbitrary_precision(
					128,
					&[(!n as u128 + 1) as u64, ((!n as u128 + 1) >> 64) as u64]
				)
				.to_string(),
			format!("i128 {}", n)
		);
	}

	#[test]
	fn const_i1() {
		let context = Context::new();

		assert_eq!(context.const_i1(true).to_string(), "i1 true");
		assert_eq!(context.const_i1(false).to_string(), "i1 false");
	}

	#[test]
	fn const_i8() {
		let context = Context::new();

		assert_eq!(context.const_i8(0).to_string(), "i8 0");
		assert_eq!(context.const_i8(-0).to_string(), "i8 0");

		assert_eq!(context.const_i8(42).to_string(), "i8 42");
		assert_eq!(context.const_i8(-42).to_string(), "i8 -42");

		assert_eq!(
			context.const_i8(i8::MAX).to_string(),
			format!("i8 {}", i8::MAX)
		);
		assert_eq!(
			context.const_i8(i8::MIN).to_string(),
			format!("i8 {}", i8::MIN)
		);
	}

	#[test]
	fn const_16() {
		let context = Context::new();

		assert_eq!(context.const_i16(0).to_string(), "i16 0");
		assert_eq!(context.const_i16(-0).to_string(), "i16 0");

		assert_eq!(context.const_i16(42).to_string(), "i16 42");
		assert_eq!(context.const_i16(-42).to_string(), "i16 -42");

		assert_eq!(
			context.const_i16(i16::MAX).to_string(),
			format!("i16 {}", i16::MAX)
		);
		assert_eq!(
			context.const_i16(i16::MIN).to_string(),
			format!("i16 {}", i16::MIN)
		);
	}

	#[test]
	fn const_i32() {
		let context = Context::new();

		assert_eq!(context.const_i32(0).to_string(), "i32 0");
		assert_eq!(context.const_i32(-0).to_string(), "i32 0");

		assert_eq!(context.const_i32(42).to_string(), "i32 42");
		assert_eq!(context.const_i32(-42).to_string(), "i32 -42");

		assert_eq!(
			context.const_i32(i32::MAX).to_string(),
			format!("i32 {}", i32::MAX)
		);
		assert_eq!(
			context.const_i32(i32::MIN).to_string(),
			format!("i32 {}", i32::MIN)
		);
	}

	#[test]
	fn const_i64() {
		let context = Context::new();

		assert_eq!(context.const_i64(0).to_string(), "i64 0");
		assert_eq!(context.const_i64(-0).to_string(), "i64 0");

		assert_eq!(context.const_i64(42).to_string(), "i64 42");
		assert_eq!(context.const_i64(-42).to_string(), "i64 -42");

		assert_eq!(
			context.const_i64(i64::MAX).to_string(),
			format!("i64 {}", i64::MAX)
		);
		assert_eq!(
			context.const_i64(i64::MIN).to_string(),
			format!("i64 {}", i64::MIN)
		);
	}

	#[test]
	fn const_i128() {
		let context = Context::new();

		assert_eq!(context.const_i128(0).to_string(), "i128 0");
		assert_eq!(context.const_i128(-0).to_string(), "i128 0");

		assert_eq!(context.const_i128(42).to_string(), "i128 42");
		assert_eq!(context.const_i128(-42).to_string(), "i128 -42");

		assert_eq!(
			context.const_i128(i128::MAX).to_string(),
			format!("i128 {}", i128::MAX)
		);
		assert_eq!(
			context.const_i128(i128::MIN).to_string(),
			format!("i128 {}", i128::MIN)
		);
	}

	#[test]
	fn const_f32() {
		let context = Context::new();

		assert_eq!(context.const_f32(0.0).to_string(), "float 0.000000e+00");
		assert_eq!(context.const_f32(-0.0).to_string(), "float -0.000000e+00");

		assert_eq!(context.const_f32(42.0).to_string(), "float 4.200000e+01");
		assert_eq!(context.const_f32(-42.0).to_string(), "float -4.200000e+01");

		assert_eq!(
			context.const_f32(f32::MAX).to_string(),
			format!(
				"float 0x{}",
				format_args!("{:x}", (f32::MAX as f64).to_bits())
					.to_string()
					.to_uppercase()
			)
		);

		assert_eq!(
			context.const_f32(f32::MIN).to_string(),
			format!(
				"float 0x{}",
				format_args!("{:x}", (f32::MIN as f64).to_bits())
					.to_string()
					.to_uppercase()
			)
		)
	}

	#[test]
	fn const_f64() {
		let context = Context::new();

		assert_eq!(context.const_f64(0.0).to_string(), "double 0.000000e+00");
		assert_eq!(context.const_f64(-0.0).to_string(), "double -0.000000e+00");

		assert_eq!(context.const_f64(42.0).to_string(), "double 4.200000e+01");
		assert_eq!(context.const_f64(-42.0).to_string(), "double -4.200000e+01");

		assert_eq!(
			context.const_f64(f64::MAX).to_string(),
			format!(
				"double 0x{}",
				format_args!("{:x}", f64::MAX.to_bits())
					.to_string()
					.to_uppercase()
			)
		);
		assert_eq!(
			context.const_f64(f64::MIN).to_string(),
			format!(
				"double 0x{}",
				format_args!("{:x}", f64::MIN.to_bits())
					.to_string()
					.to_uppercase()
			)
		)
	}
}

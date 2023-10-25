use std::{
	marker::PhantomData,
	mem::{self, ManuallyDrop},
	ops::Deref,
	ptr,
};

use llvm_sys::{
	core::LLVMGetGlobalParent,
	execution_engine::*,
	target::{
		LLVM_InitializeNativeAsmParser, LLVM_InitializeNativeAsmPrinter,
		LLVM_InitializeNativeDisassembler, LLVM_InitializeNativeTarget,
	},
	target_machine::LLVMCodeModel,
};

use super::*;
use crate::ffi::{from_cstr, to_cstring};

#[derive(Debug)]
pub struct ExecutionEngine<'ctx> {
	inner: ExecutionEngineInner,
	modules: Vec<ManuallyDrop<Module<'ctx>>>,
}

impl Drop for ExecutionEngine<'_> {
	fn drop(&mut self) {
		unsafe {
			LLVMDisposeExecutionEngine(self.as_mut_ptr());
		}
	}
}

impl LLVMRef<LLVMOpaqueExecutionEngine> for ExecutionEngine<'_> {
	unsafe fn as_mut_ptr(&self) -> *mut LLVMOpaqueExecutionEngine {
		self.inner.raw
	}
}

impl<'ctx> ExecutionEngine<'ctx> {
	pub fn new_mc_jit_compiler(module: Module<'ctx>) -> Result<ExecutionEngine<'ctx>, String> {
		unsafe {
			assert_ne!(LLVM_InitializeNativeTarget(), 1);
			assert_ne!(LLVM_InitializeNativeAsmParser(), 1);
			assert_ne!(LLVM_InitializeNativeAsmPrinter(), 1);
			assert_ne!(LLVM_InitializeNativeDisassembler(), 1);
		};

		Ok(Self {
			inner: ExecutionEngineInner::create_mc_jit_compiler(&module)?,
			modules: vec![ManuallyDrop::new(module)],
		})
	}

	pub fn modules(&self) -> impl IntoIterator<Item = &Module<'ctx>> {
		self.modules.iter().map(|module| &module as &Module<'ctx>)
	}

	pub fn get_module(&self, module_id: &str) -> Option<&Module<'ctx>> {
		self.modules()
			.into_iter()
			.find(|module| module.module_id() == module_id)
	}

	pub fn add_module(&mut self, module: Module<'ctx>) {
		unsafe {
			LLVMAddModule(self.as_mut_ptr(), module.as_mut_ptr());
		}

		self.modules.push(ManuallyDrop::new(module))
	}

	pub fn get_function(&self, name: &str) -> Option<Function<'ctx, '_>> {
		unsafe {
			let mut raw = mem::MaybeUninit::uninit();
			let value = LLVMFindFunction(
				self.as_mut_ptr(),
				to_cstring(name).as_ptr(),
				raw.as_mut_ptr(),
			);

			if value == 0 {
				Some(Function::from_raw(raw.assume_init()))
			} else {
				None
			}
		}
	}

	/// # Safety
	///
	/// It's caller's responsibility to ensure that the function is compiled
	pub unsafe fn get_compiled_function<F: Sized>(
		&self,
		name: &str,
	) -> Option<CompiledFunction<'ctx, '_, F>> {
		unsafe {
			let addr = LLVMGetFunctionAddress(self.as_mut_ptr(), to_cstring(name).as_ptr());

			if addr == 0 {
				None
			} else {
				Some(CompiledFunction::new(addr))
			}
		}
	}

	/// # Safety
	///
	/// It's caller's responsibility to ensure that the function is compiled
	pub unsafe fn run_main(&self, args: &[&str], envs: &[&str]) -> Result<i32, String> {
		let function = self.get_function("main");
		if let Some(function) = function {
			self.run_function_as_main(&function, args, envs)
		} else {
			Err("Function 'main' not found".to_string())
		}
	}

	pub fn run_function_as_main(
		&self,
		function: &Function<'ctx, '_>,
		args: &[&str],
		envs: &[&str],
	) -> Result<i32, String> {
		unsafe {
			let raw_module = LLVMGetGlobalParent(function.as_mut_ptr());

			if self
				.modules()
				.into_iter()
				.any(|module| module.as_mut_ptr() != raw_module)
			{
				return Err("Function is not contained in this execution engine".to_string());
			}
		}

		let code = unsafe {
			LLVMRunFunctionAsMain(
				self.as_mut_ptr(),
				function.as_mut_ptr(),
				args.len() as u32,
				args.iter()
					.map(|value| to_cstring(value).as_ptr())
					.collect::<Vec<_>>()
					.as_ptr(),
				envs.iter()
					.map(|value| to_cstring(value).as_ptr())
					.collect::<Vec<_>>()
					.as_ptr(),
			)
		};

		Ok(code)
	}
}

#[derive(Debug)]
struct ExecutionEngineInner {
	raw: *mut LLVMOpaqueExecutionEngine,
}

impl ExecutionEngineInner {
	fn create_mc_jit_compiler(module: &Module) -> Result<Self, String> {
		let mut options = LLVMMCJITCompilerOptions {
			OptLevel: 3,
			CodeModel: LLVMCodeModel::LLVMCodeModelDefault,
			EnableFastISel: 1,
			MCJMM: ptr::null_mut(),
			NoFramePointerElim: 0,
		};

		let mut raw = mem::MaybeUninit::uninit();
		let mut out = mem::MaybeUninit::uninit();

		unsafe {
			LLVMLinkInMCJIT();

			let res = LLVMCreateMCJITCompilerForModule(
				raw.as_mut_ptr(),
				module.as_mut_ptr(),
				&mut options,
				1,
				out.as_mut_ptr(),
			);

			if res == 0 {
				Ok(ExecutionEngineInner {
					raw: raw.assume_init(),
				})
			} else {
				Err(from_cstr(out.assume_init()).to_string())
			}
		}
	}
}

#[cfg(test)]
mod tests {
	use crate::{Context, ExecutionEngine};

	#[test]
	#[should_panic(expected = "Function is not contained in this execution engine")]
	fn run_function_as_main_function_is_not_contained() {
		let context = Context::new();
		let module = context.create_module("");
		let engine = ExecutionEngine::new_mc_jit_compiler(module).unwrap();

		let module2 = context.create_module("");
		let function = module2
			.create_function("main", context.i32_ty(), &[], false)
			.build(|builder| {
				builder.build_ret(&context.const_i32(42));
				Ok(())
			})
			.unwrap();

		engine.run_function_as_main(&function, &[], &[]).unwrap();
	}
}

pub struct CompiledFunction<'ctx, 'ee, F: Sized> {
	inner: F,
	_marker: PhantomData<&'ee ExecutionEngine<'ctx>>,
}

impl<F: Sized> Deref for CompiledFunction<'_, '_, F> {
	type Target = F;

	fn deref(&self) -> &Self::Target {
		&self.inner
	}
}

impl<F> CompiledFunction<'_, '_, F> {
	fn new(addr: u64) -> Self {
		assert_eq!(
			std::mem::size_of::<F>(),
			std::mem::size_of::<usize>(),
			"The type `F` must have the same size as a function pointer"
		);

		let inner = unsafe { mem::transmute_copy::<u64, F>(&addr) };

		Self {
			inner,
			_marker: PhantomData,
		}
	}
}

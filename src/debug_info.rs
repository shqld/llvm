use std::ops::Deref;

use llvm_sys::{core::LLVMGetModuleContext, debuginfo::*, LLVMOpaqueDIBuilder, LLVMOpaqueMetadata};

use crate::{LLVMRef, Metadata, Module};

#[allow(non_upper_case_globals)]
const null_ptr: *mut LLVMOpaqueMetadata = std::ptr::null_mut();

#[derive(Debug, PartialEq, Eq)]
pub struct DebugInfo {
	inner: Metadata,
}

impl DebugInfo {
	pub(crate) fn new(raw: *mut LLVMOpaqueMetadata) -> Self {
		Self {
			inner: Metadata::new(raw),
		}
	}

	pub fn as_metadata(&self) -> &Metadata {
		&self.inner
	}
}

impl Deref for DebugInfo {
	type Target = Metadata;

	fn deref(&self) -> &Self::Target {
		&self.inner
	}
}

#[derive(Debug, PartialEq, Eq)]
pub struct DebugInfoBuilder<'ctx, 'mdl> {
	module: &'mdl Module<'ctx>,
	raw: *mut LLVMOpaqueDIBuilder,
}

impl LLVMRef<LLVMOpaqueDIBuilder> for DebugInfoBuilder<'_, '_> {
	unsafe fn as_mut_ptr(&self) -> *mut LLVMOpaqueDIBuilder {
		self.raw
	}
}

impl<'ctx, 'mdl> DebugInfoBuilder<'ctx, 'mdl> {
	pub(crate) fn new(module: &'mdl Module<'ctx>) -> Self {
		let raw = unsafe { llvm_sys::debuginfo::LLVMCreateDIBuilder(module.as_mut_ptr()) };

		Self { module, raw }
	}
}

impl Drop for DebugInfoBuilder<'_, '_> {
	fn drop(&mut self) {
		unsafe { llvm_sys::debuginfo::LLVMDisposeDIBuilder(self.raw) }
	}
}

impl<'ctx, 'mdl> DebugInfoBuilder<'ctx, 'mdl> {
	pub fn build_location(
		&self,
		line: u32,
		column: u32,
		scope: DebugInfo,
		inlined_at: Option<DebugInfo>,
	) -> DebugInfo {
		DebugInfo::new(unsafe {
			LLVMDIBuilderCreateDebugLocation(
				LLVMGetModuleContext(self.module.as_mut_ptr()),
				line,
				column,
				scope.as_metadata().as_mut_ptr(),
				inlined_at.map_or(null_ptr, |m| m.as_metadata().as_mut_ptr()),
			)
		})
	}

	pub fn build_file(&self, filename: &str, directory: &str) -> DebugInfo {
		DebugInfo::new(unsafe {
			LLVMDIBuilderCreateFile(
				self.as_mut_ptr(),
				filename.as_ptr() as *const _,
				filename.len(),
				directory.as_ptr() as *const _,
				directory.len(),
			)
		})
	}

	pub fn build_expression(&self, addrs: &[u64]) -> DebugInfo {
		DebugInfo::new(unsafe {
			LLVMDIBuilderCreateExpression(self.as_mut_ptr(), addrs.as_ptr() as *mut _, addrs.len())
		})
	}

	#[allow(clippy::too_many_arguments)]
	pub fn build_compile_unit(
		&self,
		// language: DWARFSourceLanguage,
		file: &DebugInfo,
		producer: &str,
		is_optimized: bool,
		flags: &str,
		// runtime_ver: libc::c_uint,
		runtime_ver: u32,
		split_name: &str,
		// kind: DWARFEmissionKind,
		dwo_id: u32,
		split_debug_inlining: bool,
		debug_info_for_profiling: bool,
		sysroot: &str,
		sdk: &str,
	) -> DebugInfo {
		DebugInfo::new(unsafe {
			LLVMDIBuilderCreateCompileUnit(
				self.as_mut_ptr(),
				// language.into(),
				LLVMDWARFSourceLanguage::LLVMDWARFSourceLanguageC89,
				file.as_mut_ptr(),
				producer.as_ptr() as _,
				producer.len(),
				is_optimized as _,
				flags.as_ptr() as _,
				flags.len(),
				runtime_ver,
				split_name.as_ptr() as _,
				split_name.len(),
				// kind.into(),
				LLVMDWARFEmissionKind::LLVMDWARFEmissionKindNone,
				dwo_id,
				split_debug_inlining as _,
				debug_info_for_profiling as _,
				sysroot.as_ptr() as _,
				sysroot.len(),
				sdk.as_ptr() as _,
				sdk.len(),
			)
		})
	}

	#[allow(clippy::too_many_arguments)]
	pub fn build_subprogram(
		&self,
		// TODO:
		_scope: &DebugInfo,
		name: &str,
		linkage_name: &str,
		file: &DebugInfo,
		line_no: u32,
		// TODO:
		_ty: &DebugInfo,
		is_local_to_unit: bool,
		is_definition: bool,
		scope_line: u32,
		flags: u32,
		is_optimized: bool,
	) -> DebugInfo {
		unsafe {
			let raw = LLVMDIBuilderCreateFunction(
				self.as_mut_ptr(),
				self.build_compile_unit(
					file,
					"producer",
					false,
					"flags",
					1,
					"split_name",
					1,
					false,
					false,
					"sysroot",
					"sdk",
				)
				.as_mut_ptr(),
				name.as_ptr() as _,
				name.len(),
				linkage_name.as_ptr() as _,
				linkage_name.len(),
				file.as_mut_ptr(),
				line_no,
				// ty.as_mut_ptr(),
				LLVMDIBuilderCreateSubroutineType(
					self.as_mut_ptr(),
					file.as_mut_ptr(),
					null_ptr as _,
					0,
					0,
				),
				is_local_to_unit as _,
				is_definition as _,
				scope_line,
				flags as _,
				is_optimized as _,
			);
			LLVMDIBuilderFinalizeSubprogram(self.as_mut_ptr(), raw);

			DebugInfo::new(raw)
		}
	}
}

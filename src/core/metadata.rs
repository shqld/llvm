use llvm_sys::LLVMOpaqueMetadata;

use crate::LLVMRef;

#[derive(Debug, PartialEq, Eq)]
pub struct Metadata {
	raw: *mut LLVMOpaqueMetadata,
}

impl LLVMRef<LLVMOpaqueMetadata> for Metadata {
	unsafe fn as_mut_ptr(&self) -> *mut LLVMOpaqueMetadata {
		self.raw
	}
}

impl Metadata {
	pub(crate) fn new(raw: *mut LLVMOpaqueMetadata) -> Self {
		Self { raw }
	}
}

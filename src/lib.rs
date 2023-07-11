#![doc = include_str!("../README.md")]
#![deny(clippy::all)]
#![allow(non_snake_case, clippy::from_over_into)]

mod ffi;

pub mod core;
pub mod debug_info;
pub mod execution_engine;

pub use core::*;

pub use debug_info::*;
pub use execution_engine::*;

pub trait LLVMRef<Target> {
	/// # Safety
	///
	/// The pointer points to the value in the C++ land.
	unsafe fn as_mut_ptr(&self) -> *mut Target;
}

#[macro_export]
macro_rules! named {
	(let $name:ident = $value:expr) => {
		let $name = $value.with_name(stringify!($name));
	};
	(let mut $name:ident = $value:expr) => {
		let mut $name = $value.with_name(stringify!($name));
	};
	($name:ident = $value:expr) => {
		$name = $value.with_name(stringify!($name));
	};
}

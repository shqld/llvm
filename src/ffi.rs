use std::ffi::{CStr, CString};

pub(crate) fn to_cstring(str: &str) -> CString {
	CString::new(str).expect("string contains null character")
}

pub(crate) fn from_cstr<'a>(cstr: *const std::ffi::c_char) -> &'a str {
	unsafe { CStr::from_ptr(cstr) }
		.to_str()
		.expect("c string is not utf8")
}

#[allow(non_upper_case_globals)]
pub(crate) const nullstr: *const std::ffi::c_char =
	unsafe { std::ffi::CStr::from_bytes_with_nul_unchecked(b"\0").as_ptr() };

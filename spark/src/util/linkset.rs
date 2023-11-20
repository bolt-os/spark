/*
 * Copyright (c) 2023 xvanc and contributors
 * SPDX-License-Identifier: BSD-3-Clause
 */

pub struct Linkset<T: ?Sized + 'static> {
    start: *mut Entry<T>,
    stop: *mut Entry<T>,
}

unsafe impl<T: 'static> Send for Linkset<T> {}
unsafe impl<T: 'static> Sync for Linkset<T> {}

impl<T: ?Sized + 'static> Linkset<T> {
    pub fn as_slice(&self) -> &[&T] {
        let start = self.start;
        let stop = self.stop;
        #[allow(clippy::cast_sign_loss)]
        unsafe {
            let len = stop.offset_from(start) as usize;
            core::slice::from_raw_parts(start.cast::<&T>(), len)
        }
    }
}

pub macro declare($vis:vis $name:ident: $t:ty) {
    #[allow(non_upper_case_globals)]
    $vis static $name: Linkset<$t> = unsafe {
        #[allow(improper_ctypes)]
        extern {
            #[link_name = concat!("__start___linkset_", stringify!($name))]
            static mut _start: Entry<$t>;
            #[link_name = concat!("__stop___linkset_", stringify!($name))]
            static mut _stop: Entry<$t>;
        }
        let start = ::core::ptr::addr_of_mut!(_start);
        let stop =  ::core::ptr::addr_of_mut!(_stop);
        Linkset { start, stop }
    };
}

#[repr(transparent)]
pub struct Entry<T: ?Sized + 'static> {
    _ptr: *mut T,
}

unsafe impl<T: 'static> Send for Entry<T> {}
unsafe impl<T: 'static> Sync for Entry<T> {}

pub macro entry($set:ident: $t:ty, $value:expr) {
    const _: () = {
        #[used]
        #[link_section = concat!("__linkset_", stringify!($set))]
        static ENTRY: Entry<$t> = unsafe {
            let _: &Linkset<$t> = &$set;
            static mut VALUE: $t = $value;
            let _ptr = ::core::ptr::addr_of_mut!(VALUE);
            Entry { _ptr }
        };
    };
}

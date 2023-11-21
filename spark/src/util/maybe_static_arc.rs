// SPDX-FileCopyrightText:  2022-2023 xvanc and contributors
// SPDX-License-Identifier: BSD-3-Clause

use alloc::sync::Arc;
use core::ops::Deref;

pub enum MaybeStaticArc<T: ?Sized + 'static> {
    Static(&'static T),
    Arc(Arc<T>),
}

impl<T: ?Sized + 'static> Deref for MaybeStaticArc<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        match self {
            Self::Static(ptr) => ptr,
            Self::Arc(ptr) => ptr,
        }
    }
}

impl<T: ?Sized + 'static> Clone for MaybeStaticArc<T> {
    fn clone(&self) -> Self {
        match self {
            Self::Arc(arc) => Self::Arc(Arc::clone(arc)),
            Self::Static(ptr) => Self::Static(ptr),
        }
    }
}

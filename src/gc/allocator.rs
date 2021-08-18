use crate::{ ObjectBase, Value };
use std::{alloc::Layout, ptr::NonNull, cell::RefCell};

struct AllocatorImpl {
    object_list_head: Option<NonNull<ObjectBase>>,
}

impl AllocatorImpl {
    fn new() -> Self {
        Self {
            object_list_head: None,
        }
    }

    fn allocate_prototype_object(&mut self, layout: Layout) -> Option<NonNull<ObjectBase>> {
        ObjectBase::allocate_prototype_object(layout)
    }
}

pub struct Allocator(RefCell<AllocatorImpl>);

impl Allocator {
    pub fn new() -> Self {
        Self(RefCell::new(AllocatorImpl::new()))
    }

    pub fn allocate_prototype_object(&self, layout: Layout) -> Option<NonNull<ObjectBase>> { self.0.borrow_mut().allocate_prototype_object(layout) }
}

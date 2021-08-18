use num_enum::{IntoPrimitive, TryFromPrimitive};
use std::{alloc::{alloc, Layout}, mem::MaybeUninit, convert::TryFrom, fmt, ptr::NonNull};

#[repr(u8)]
#[derive(Clone, Copy, Debug, PartialEq, IntoPrimitive, TryFromPrimitive)]
pub enum ObjectType {
    Empty = 0,
    String = 1,
}

#[derive(Clone, Copy, PartialEq)]
#[repr(transparent)]
pub struct PackedObjectHeader(u64);

impl PackedObjectHeader {
    fn new(object_type: ObjectType, next: Option<NonNull<ObjectBase>>) -> Self {
        let ptr_u64 = next.map(|ptr| ptr.as_ptr() as u64).unwrap_or(0);
        assert_eq!(
            ptr_u64 & 0xffff_0000_0000_0007,
            0,
            "Pointer is not correctly aligned, and/or is kernel mode"
        );

        let shifted_type = u64::from(u8::from(object_type)) << 56;

        Self(shifted_type | ptr_u64)
    }

    fn object_type(&self) -> ObjectType {
        ObjectType::try_from((self.0 >> 56) as u8).expect("Invalid object type")
    }

    fn next_pointer(&self) -> Option<NonNull<ObjectBase>> {
        NonNull::new((self.0 & 0x0000_ffff_ffff_fff8) as *mut ObjectBase)
    }
}

impl Default for PackedObjectHeader {
    fn default() -> Self {
        Self::new(ObjectType::Empty, None)
    }
}

impl fmt::Debug for PackedObjectHeader {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        f.debug_struct("PackedObjectHeader")
            .field("type", &self.object_type())
            .field("next_pointer", &self.next_pointer())
            .field("raw", &format!("0x{:016x}", self.0))
            .finish()
    }
}

#[repr(C)]
pub struct ObjectBase {
    header: PackedObjectHeader,
    size: usize,
}

const MIN_OBJECT_ALIGNMENT: usize = std::mem::align_of::<ObjectBase>();
const OBJECT_HEADER_SIZE: usize = std::mem::size_of::<ObjectBase>();

impl ObjectBase {
    pub fn allocate_prototype_object(layout: Layout) -> Option<NonNull<ObjectBase>> {
        assert!(MIN_OBJECT_ALIGNMENT == 8, "Required alignment doesn't match");
        assert!(layout.align() <= MIN_OBJECT_ALIGNMENT, "Cannot satisfy required alignment");
        
        let layout = Layout::from_size_align(layout.size() + OBJECT_HEADER_SIZE, MIN_OBJECT_ALIGNMENT).expect("Failed to generate layout");
        
        unsafe { 
            NonNull::new(alloc(layout) as *mut MaybeUninit<ObjectBase>).map(|mut alloc| {
                NonNull::new_unchecked(alloc.as_mut().write(ObjectBase { header: PackedObjectHeader::default(), size: layout.size() }) as *mut ObjectBase)
            })
        }
    }

    pub fn payload_size(&self) -> usize {
        self.size
    }

    pub fn payload_ptr<T>(&self) -> NonNull<MaybeUninit<T>> {
        unsafe { NonNull::new_unchecked(((self as *const _ as usize) + std::mem::size_of::<Self>()) as *mut MaybeUninit<T>) }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_alloc_object() {
        let object = ObjectBase::allocate_prototype_object(Layout::new::<u8>()).expect("allocated object");

        assert!(unsafe { object.as_ref() }.payload_size() >= 1);
        assert_eq!(unsafe { object.as_ref().payload_ptr::<u8>() }.as_ptr() as u64 & 0x07, 0);

        let payload_ptr = unsafe { object.as_ref().payload_ptr::<u8>() };

        // Write something into the payload
        payload_ptr.as_mut().write(0);
        assert_eq!(payload_ptr.as_ref().read(), 0);
    }
}
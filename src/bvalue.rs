use crate::{LoxError, Nil};
use core::{convert::TryFrom, fmt};

#[derive(Clone, Copy, PartialEq)]
#[repr(transparent)]
pub struct BValue(u64);

//const MASK_SIGN: u64 = 0x8000_0000_0000_0000;
const MASK_EXPONENT: u64 = 0x7ff0_0000_0000_0000;
const MASK_QUIET: u64 = 0x0008_0000_0000_0000;
const MASK_TYPE: u64 = 0x0007_0000_0000_0000;

#[derive(Clone, Copy, Debug, PartialEq)]
#[repr(u64)]
pub enum BValueType {
    Number = 0x0000_0000_0000_0000,
    Nil = 0x0001_0000_0000_0000,
    True = 0x0002_0000_0000_0000,
    False = 0x0003_0000_0000_0000,

    // Define these for completeness
    Unused4 = 0x0004_0000_0000_0000,
    Unused5 = 0x0005_0000_0000_0000,
    Unused6 = 0x0006_0000_0000_0000,
    Unused7 = 0x0007_0000_0000_0000,
}

impl fmt::Display for BValueType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        f.write_str(match self {
            Self::Number => "number",
            Self::Nil => "nil",
            Self::True => "true",
            Self::False => "false",
            Self::Unused4 => "Unused4",
            Self::Unused5 => "Unused5",
            Self::Unused6 => "Unused6",
            Self::Unused7 => "Unused7",
        })
    }
}

const NAN_BVALUE: BValue = unsafe { BValue::from_bits_unchecked(MASK_EXPONENT) };
const NIL_BVALUE: BValue = unsafe { BValue::from_type_unchecked(BValueType::Nil, 0) };

impl BValue {
    pub const unsafe fn from_bits_unchecked(num: u64) -> Self {
        Self(num)
    }

    pub const unsafe fn from_type_unchecked(t: BValueType, num: u64) -> Self {
        Self::from_bits_unchecked(MASK_EXPONENT | MASK_QUIET | t as u64 | num)
    }

    pub unsafe fn from_number_unchecked(num: f64) -> Self {
        Self::from_bits_unchecked(core::mem::transmute(num))
    }

    pub fn from_number(num: f64) -> Self {
        if num.is_nan() {
            NAN_BVALUE
        } else {
            unsafe { Self::from_number_unchecked(num) }
        }
    }

    pub fn value_type(&self) -> BValueType {
        if MASK_EXPONENT != self.0 & MASK_EXPONENT {
            BValueType::Number
        } else {
            unsafe { core::mem::transmute(self.0 & MASK_TYPE) }
        }
    }

    pub fn is_nil(&self) -> bool {
        self.value_type() == BValueType::Nil
    }

    pub const fn nil_val() -> Self {
        NIL_BVALUE
    }

    pub fn is_f64(&self) -> bool {
        self.value_type() == BValueType::Number
    }

    pub unsafe fn to_f64_unchecked(self) -> f64 {
        core::mem::transmute(self.0)
    }

    pub fn to_f64(self) -> Result<f64, LoxError> {
        match self.value_type() {
            BValueType::Number => Ok(unsafe { self.to_f64_unchecked() }),
            _ => Err(LoxError::BValueTypeError(self, BValueType::Number)),
        }
    }

    pub fn is_bool(&self) -> bool {
        matches!(self.value_type(), BValueType::True | BValueType::False)
    }

    pub fn to_bool(self) -> bool {
        !matches!(self.value_type(), BValueType::False | BValueType::Nil)
    }

    pub const fn true_val() -> Self {
        unsafe { Self::from_type_unchecked(BValueType::True, 0) }
    }

    pub const fn false_val() -> Self {
        unsafe { Self::from_type_unchecked(BValueType::False, 0) }
    }

    pub const fn from_bool(b: bool) -> Self {
        if b {
            Self::true_val()
        } else {
            Self::false_val()
        }
    }
}

impl fmt::Debug for BValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        write!(f, "BValue({} 0x{:016x})", self.value_type(), self.0)
    }
}

impl fmt::Display for BValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        match self.value_type() {
            BValueType::Nil => f.write_str("nil"),
            BValueType::True => f.write_str("true"),
            BValueType::False => f.write_str("false"),
            BValueType::Number => unsafe { self.to_f64_unchecked() }.fmt(f),

            unknown_type => write!(f, "Unknown type {}", unknown_type),
        }
    }
}

impl From<Nil> for BValue {
    fn from(_: Nil) -> Self {
        Self::nil_val()
    }
}

impl TryFrom<BValue> for Nil {
    type Error = LoxError;

    fn try_from(v: BValue) -> Result<Self, Self::Error> {
        if v.is_nil() {
            Ok(Self)
        } else {
            Err(LoxError::BValueTypeError(v, BValueType::Nil))
        }
    }
}

impl From<bool> for BValue {
    fn from(b: bool) -> Self {
        Self::from_bool(b)
    }
}

impl From<BValue> for bool {
    fn from(v: BValue) -> bool {
        v.to_bool()
    }
}

impl From<f64> for BValue {
    fn from(f: f64) -> Self {
        Self::from_number(f)
    }
}

impl TryFrom<BValue> for f64 {
    type Error = LoxError;

    fn try_from(v: BValue) -> Result<Self, Self::Error> {
        v.to_f64()
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::LoxResult;
    use std::convert::TryInto;

    fn check_invalid_type<T>(value: BValue, expected: BValueType, result: LoxResult<T>) -> bool {
        match result {
            Err(LoxError::BValueTypeError(error_value, error_expected)) => {
                value == error_value && expected == error_expected
            }
            _ => false,
        }
    }

    #[test]
    fn nil_test() {
        const TEST_VAL: BValue = BValue::nil_val();
        assert_eq!(
            TEST_VAL.value_type(),
            BValueType::Nil,
            "Invalid value {:?}",
            TEST_VAL
        );
        assert!(unsafe { TEST_VAL.to_f64_unchecked() }.is_nan());
        assert!(check_invalid_type(
            TEST_VAL,
            BValueType::Number,
            TEST_VAL.to_f64()
        ));
    }

    #[test]
    fn number_test() {
        let num: BValue = BValue::from_number(0.0);
        assert_eq!(num.value_type(), BValueType::Number);
        assert_eq!(unsafe { num.to_f64_unchecked() }, 0.0);
        assert_eq!(num.to_f64().expect("failed to convert"), 0.0);

        let num2 = BValue::from(42.0);
        assert_ne!(num, num2);

        assert_eq!(42.0, num2.try_into().expect("failed to convert"));
    }

    #[test]
    fn bool_test() {
        let true_val = BValue::from_bool(true);
        let false_val = BValue::from_bool(false);
        assert_eq!(true_val, BValue::true_val());
        assert_ne!(true_val, BValue::false_val());
        assert_ne!(false_val, BValue::true_val());
        assert_eq!(false_val, BValue::false_val());

        assert_eq!(true, bool::from(BValue::true_val()));
        assert_eq!(false, bool::from(BValue::false_val()));
        assert_eq!(false, bool::from(BValue::nil_val()));
        assert_eq!(true, bool::from(BValue::from_number(0.0)));
    }

    #[test]
    fn display_test() {
        assert_eq!(BValue::nil_val().to_string(), "nil");
        assert_eq!(BValue::true_val().to_string(), "true");
        assert_eq!(BValue::false_val().to_string(), "false");
        assert_eq!(BValue::from_number(0.0).to_string(), "0");
        assert_eq!(BValue::from_number(-17.0).to_string(), "-17");
        //assert_eq!(Value::String("hello".into()).to_string(), "hello");
    }
}

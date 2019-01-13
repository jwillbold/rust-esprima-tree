use serde::ser::{Serializer};

#[cfg(test)]
pub fn check_se_de<T>(t: T, json: serde_json::Value) where for<'de> T: serde::Serialize +
                                                                   serde::Deserialize<'de> +
                                                                   std::fmt::Debug +
                                                                   std::cmp::PartialEq{
    assert_eq!(serde_json::to_value(&t).unwrap(), json);
    assert_eq!(t, serde_json::from_value::<T>(json).unwrap());
}


// macro_rules! derive_private_shadow_struct {
//     (pub struct $si:ident {
//         $(pub $f:ident: $t:ty,)*
//     }, $tof:ident) => {
//         pub struct $si {
//             $(
//                 pub $f: $t
//             )*
//         }
//
//         pub fn $tof<S>(t: $si, s: S) -> Result<S::Ok, S::Error>
//             where S: Serializer {
//
//             #[derive(Serialize)]
//             #[serde(tag="type", rename="$si")]
//             struct PrivateShadowStruct<'a> {
//                 $(
//                     $f: &'a$t
//                 )*
//             }
//
//             PrivateShadowStruct {
//                 $(
//                     $f: &t.$f
//                 )*
//             }.serialize(s)
//         }
//     }
// }
//
// derive_private_shadow_struct!{
//     pub struct Test {
//         pub a: u64,
//     },
//     test_as_obj
// }

//
// pub fn serialize_as_opt<T, S>(f: &'static &Fn(&T, S) -> Result<S::Ok, S::Error>)
//     -> impl Fn(&Option<T>, S) -> Result<S::Ok, S::Error>
//     where S: Serializer
// {
//     move |t: &Option<T>, s: S| -> Result<S::Ok, S::Error> {
//         match t {
//             Some(x) => f(x, s),
//             None => s.serialize_none()
//         }
//     }
// }

pub fn serialize_as_opt<T, S, F>(f: &'static F) -> impl Fn(&Option<T>, S) -> Result<S::Ok, S::Error>
    where S: Serializer, F: Fn(&T, S) -> Result<S::Ok, S::Error>
{
    move |t: &Option<T>, s: S| -> Result<S::Ok, S::Error> {
        match t {
            Some(x) => f(x, s),
            None => s.serialize_none()
        }
    }
}


// pub fn serialize_as_opt<S>(ident: &Option<Identifier>, s: S) -> Result<S::Ok, S::Error>
//     where S: Serializer{
//     match ident {
//         Some(x) => serialize_ident_as_obj(x, s),
//         None => s.serialize_none(),
//     }
// }

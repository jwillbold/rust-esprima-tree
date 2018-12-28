#[cfg(test)]
pub fn check_se_de<T>(t: T, json: serde_json::Value) where for<'de> T: serde::Serialize +
                                                                   serde::Deserialize<'de> +
                                                                   std::fmt::Debug +
                                                                   std::cmp::PartialEq{
    assert_eq!(serde_json::to_value(&t).unwrap(), json);
    assert_eq!(t, serde_json::from_value::<T>(json).unwrap());
}
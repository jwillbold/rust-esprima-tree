use std::fs::File;
use std::io::BufReader;
use std::error::Error;

extern crate serde_json;

extern crate jsast;


#[cfg(test)]
fn check(json: serde_json::Value) {
    let program = serde_json::from_value::<jsast::Program>(json.clone()).unwrap();
    let program_json = serde_json::to_value(program).unwrap();

    assert_eq!(json, program_json);
}

#[cfg(test)]
fn get_testdata(path: std::path::PathBuf) -> Result<serde_json::Value, Box<Error>> {
    // FYI: https://github.com/serde-rs/json/issues/160
    let file = File::open(path)?;
    let reader = BufReader::new(file);

    Ok(serde_json::from_reader(reader)?)
}

#[test]
fn test_stuff() {
    let paths = std::fs::read_dir("./tests/data").unwrap();

    for path in paths {
        let path = path.unwrap().path();
        if path.extension().map(|s| s == "json").unwrap_or(false) {
            println!("Checking: {}", path.display());

            check(get_testdata(path).unwrap());
        }
    }
}

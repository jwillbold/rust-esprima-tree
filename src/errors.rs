pub mod jsast {
    #[derive(Debug)]
    pub struct ParsingError {
        kind: ParsingErrorKind
    }

    #[derive(Debug)]
    pub enum ParsingErrorKind {
        UnknownType,
        UnexpectedDataType,
        ImplementationMissing,
    }

    impl ParsingError {
        pub fn new(kind: ParsingErrorKind) -> Self {
            ParsingError {
                kind
            }
        }
    }


    #[derive(Debug)]
    pub struct SerilizationError {
        kind: SerilizationErrorKind,
    }

    impl SerilizationError {
        pub fn new(kind: SerilizationErrorKind) -> Self {
            SerilizationError {
                kind
            }
        }
    }

    #[derive(Debug)]
    pub enum SerilizationErrorKind {
        ImplementationMissing,
    }

}

pub mod ast_printer;
pub mod expr;

#[derive(Debug)]
pub struct LoxParser {}

#[cfg(test)]
mod tests {
    use crate::LoxParser;

    #[test]
    fn it_works() {
        let _ = LoxParser {};
    }
}

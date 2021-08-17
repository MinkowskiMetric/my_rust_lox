use std::io;

mod bvalue;
mod callable;
mod class;
mod error;
mod error_collector;
mod instance;
mod interpreter;
mod lexer;
mod parser;
mod position;
mod resolver;
mod settings;
mod value;

pub use bvalue::{BValue, BValueType};
pub use callable::{Callable, NativeCallable, ScriptCallable};
pub use class::Class;
pub use error::{LoxError, LoxResult, UnwindableLoxError, UnwindableLoxResult};
pub use error_collector::{
    adapt_errors, CollectedErrors, CollectibleErrors, CompilerResult, ErrorCollector,
};
pub use instance::{Instance, InstanceRef};
pub use interpreter::{interpret, Environment, EnvironmentRef, Interpretable, Interpreter};
pub use lexer::{tokenize, tokenize_file, PositionedToken, SimpleToken, Token};
pub use parser::{
    parse, BaseExpression, BinaryOp, ClassDefinition, Expression, ExpressionVisitor,
    FuncDefinition, FuncType, LogicalBinaryOp, Parseable, Parser, ResolvedClassDefinition,
    ResolvedExpression, ResolvedFuncDefinition, ResolvedIdentifier, ResolvedStatement, Statement,
    StatementVisitor, UnaryOp,
};
pub use position::{FilePos, Position};
pub use resolver::{resolve, resolve_statement, Resolvable};
pub use value::{Nil, Value};

fn print_error(error: LoxError) {
    println!("Error: {}", error);
}

fn run_interactive() -> LoxResult<()> {
    loop {
        let mut parse_data = String::new();
        let mut interpreter = Interpreter::new()?;
        let result = loop {
            let mut line = String::new();
            if io::stdin().read_line(&mut line).is_err() {
                break Ok(());
            }

            parse_data.push_str(&line);

            let result: CompilerResult<Vec<_>> = tokenize(&parse_data, "interactive", 1)
                .parse()
                .resolve()
                .result();
            match result {
                CompilerResult::Succeeded(stmts) => match stmts.interpret(&mut interpreter) {
                    Ok(_) => (),
                    Err(err) => break Err(err),
                },

                CompilerResult::Partial(_) => continue,
                CompilerResult::Failed(errors) => break Err(LoxError::MultipleErrors(errors)),
            }
        };

        if let Err(err) = result {
            print_error(err);
        }
    }
}

fn run_file(input_file: &str) -> LoxResult<()> {
    let stmts = match tokenize_file(input_file)?
        .parse()
        .resolve()
        .result::<Vec<_>>()
    {
        CompilerResult::Failed(errors) | CompilerResult::Partial(errors) => {
            Err(LoxError::MultipleErrors(errors))
        }

        CompilerResult::Succeeded(stmts) => Ok(stmts),
    }?;

    interpret(&stmts)
}

fn main() {
    let settings = settings::Settings::parse_cmd_line();
    let result = if settings.interactive {
        run_interactive()
    } else {
        run_file(&settings.input_file)
    };

    if let Err(err) = result {
        print_error(err);
    }
}

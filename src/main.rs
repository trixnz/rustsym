extern crate syntex_syntax as syntax;
extern crate rustc_serialize;
extern crate clap;
extern crate walkdir;
extern crate syncbox;
extern crate num_cpus;

#[cfg(test)]
extern crate tempfile;

mod visitor;

use rustc_serialize::{json, Encodable, Encoder};

use clap::{App, Arg, SubCommand, AppSettings};

use syntax::parse::{self, ParseSess};
use syntax::visit;

use std::path::Path;

#[derive(Debug, PartialEq)]
enum MatchKind {
    Module,

    Struct,
    Method,
    Field,

    Function,

    Constant,
    Static,
    Enum,

    Macro,
    Trait,
}

// Required to convert the `MatchKind` enum fields to lowercase in the JSON output
impl Encodable for MatchKind {
    fn encode<E: Encoder>(&self, e: &mut E) -> Result<(), E::Error> {
        try!(e.emit_str(match *self {
            MatchKind::Module => "module",

            MatchKind::Struct => "struct",
            MatchKind::Method => "method",
            MatchKind::Field => "field",

            MatchKind::Function => "function",

            MatchKind::Constant => "constant",
            MatchKind::Static => "static",
            MatchKind::Enum => "enum",

            MatchKind::Macro => "macro",
            MatchKind::Trait => "trait",
        }));

        Ok(())
    }
}

#[derive(Debug, RustcEncodable)]
pub struct Match {
    path: String,
    name: String,
    container: String,
    kind: MatchKind,
    line: usize,
}

#[derive(PartialEq)]
enum SearchType {
    // Limited to the specific rust file provided
    Local,
    // Limited to the specific rust file provided and any imported modules
    // This is the default search option
    LocalChildren,
    // Searches the entire crate root for any *.rs files
    Global,
}

fn dump_ast(matches: &clap::ArgMatches) {
    let file = matches.value_of("file").unwrap();

    let session = ParseSess::new();
    let krate = parse::parse_crate_from_file(file.as_ref(), vec![], &session).unwrap();

    // Pretty print the parsed AST
    println!("{:#?}", krate.module);
}

fn search_symbol_global(path: &str, query: &str) -> Vec<Match> {
    use walkdir::WalkDir;
    use syncbox::{ThreadPool, Run};
    use std::sync::mpsc::channel;
    use std::sync::Arc;
    use std::sync::atomic::{AtomicUsize, Ordering};

    let crate_root = Path::new(path);

    // Check for the presence of Cargo.toml. When searching globally, the user should search from
    // the crate root as we make some assumptions about the environment.
    match std::fs::metadata(crate_root.join("Cargo.toml")) {
        Ok(_) => {}
        Err(_) => {
            panic!("Failed to open Cargo.toml. When searching globally, you must begin at the \
                    crate root")
        }
    };

    let target_dir = crate_root.join("target");

    let started_jobs = Arc::new(AtomicUsize::new(0));
    let completed_jobs = Arc::new(AtomicUsize::new(0));

    let (tx, rx) = channel();

    let mut expected_jobs = 0;

    let pool = ThreadPool::fixed_size(num_cpus::get() as u32);
    for entry in WalkDir::new(path).into_iter().filter_map(|e| e.ok()) {
        let path = entry.path();

        // Filter out target directory
        if path.starts_with(&target_dir) {
            continue;
        }

        let filename = match path.file_name().and_then(|s| s.to_str()) {
            Some(s) => s,
            None => continue,
        };

        // Filter out anything that isn't a rust source unit
        if !filename.ends_with(".rs") {
            continue;
        }

        if let Some(path) = path.to_str() {
            let path = path.to_owned();
            let query = query.to_owned();

            let started_jobs_clone = started_jobs.clone();
            let completed_jobs_clone = completed_jobs.clone();
            let tx = tx.clone();

            pool.run(move || {
                // Indicate that this job has started. The job may not actually
                // finish as serde can panic on malformed Rust syntax, but
                // `completed_jobs` can catch this.
                started_jobs_clone.fetch_add(1, Ordering::SeqCst);

                let matches = search_symbol_file(&path, &query, false);

                tx.send(matches).unwrap();

                // Indicate that we have a completed result
                completed_jobs_clone.fetch_add(1, Ordering::SeqCst);
            });

            expected_jobs += 1;
        }
    }

    // Wait for all of the jobs to start
    while started_jobs.load(Ordering::SeqCst) != expected_jobs {}

    // Stop any new jobs and wait for all existing jobs to finish
    pool.shutdown_now();
    pool.await_termination();

    // Fold the list of matches into a single list
    let completed_jobs = completed_jobs.load(Ordering::Relaxed);
    rx.iter().take(completed_jobs).fold(vec![], |mut v, m| {
        v.extend(m);
        v
    })
}

fn search_symbol_file(file: &str, query: &str, search_children: bool) -> Vec<Match> {
    let session = ParseSess::new();
    let krate = match parse::parse_crate_from_file(file.as_ref(), vec![], &session) {
        Ok(krate) => krate,
        Err(_) => return vec![],
    };

    use visitor::SymbolVisitor;
    let mut visitor = SymbolVisitor {
        matches: vec![],
        codemap: session.codemap(),
        search_children: search_children,
        query: query.to_lowercase(),
    };

    visit::walk_crate(&mut visitor, &krate);

    visitor.matches
}

fn search_symbol(matches: &clap::ArgMatches) -> Vec<Match> {
    let file = matches.value_of("file").unwrap();
    let query = matches.value_of("query").unwrap_or_default();

    let mut search_type = SearchType::LocalChildren;
    if matches.is_present("local") {
        search_type = SearchType::Local;
    } else if matches.is_present("global") {
        search_type = SearchType::Global;
    }

    match search_type {
        SearchType::Global => search_symbol_global(file, query),
        SearchType::Local => search_symbol_file(file, query, false),
        SearchType::LocalChildren => search_symbol_file(file, query, true),
    }
}

fn main() {
    let app = App::new("rustsym")
        .version(env!("CARGO_PKG_VERSION"))
        .author(env!("CARGO_PKG_AUTHORS"))
        .about(env!("CARGO_PKG_DESCRIPTION"))
        .settings(&[AppSettings::GlobalVersion, AppSettings::SubcommandRequiredElseHelp])
        .subcommand(SubCommand::with_name("ast")
            .about("Pretty print the AST for the provided rust source file")
            .setting(AppSettings::ArgRequiredElseHelp)
            .arg(Arg::with_name("file")
                .required(true)
                .help("The rust source file to print the AST for")))
        .subcommand(SubCommand::with_name("search")
            .about("Search for a symbol")
            .setting(AppSettings::ArgRequiredElseHelp)
            .arg(Arg::with_name("local")
                .long("local")
                .short("l")
                .help("Search for symbols found only in the provided rust source file"))
            .arg(Arg::with_name("global")
                .long("global")
                .short("g")
                .conflicts_with("local")
                .help("Search for symbols found in the entire crate"))
            .arg(Arg::with_name("file")
                .required(true)
                .help("The rust source file or crate root to search for symbols in"))
            .arg(Arg::with_name("query").help("Optional string to match symbols against")));
    let matches = app.get_matches();

    if let (cmd, Some(m)) = matches.subcommand() {
        match cmd {
            "ast" => dump_ast(m),
            "search" => {
                let matches = search_symbol(m);

                let encoded = json::encode(&matches).unwrap();
                println!("{}", encoded);
            }
            _ => unreachable!(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{Match, MatchKind, search_symbol_file};
    use tempfile::NamedTempFile;

    fn create_source_tmp(src: &'static str) -> NamedTempFile {
        use std::io::Write;

        let mut file = NamedTempFile::new().expect("failed to create temporary file");
        file.write_all(src.as_bytes()).unwrap();

        file
    }

    fn get_symbol_matches(src: &'static str, symbol_name: &str) -> Vec<Match> {
        let file = create_source_tmp(src);
        search_symbol_file(file.path().to_str().unwrap(), symbol_name, false)
    }

    fn test_symbol<F>(src: &'static str, symbol_name: &str, test_fn: &F)
        where F: Fn(&Match)
    {
        let matches = get_symbol_matches(src, symbol_name);
        let candidate: &Match = matches.first().expect("Failed to find any matches");

        test_fn(candidate);
    }

    fn test_symbols<F>(src: &'static str, symbols: Vec<&str>, test_fn: F)
        where F: Fn(&Match)
    {
        for symbol in symbols {
            test_symbol(src, symbol, &test_fn);
        }
    }

    #[test]
    fn check_struct() {
        let src = "struct TestStruct {}";

        test_symbols(src, vec!["test", "TestStruct"], |symbol: &Match| {
            assert_eq!(symbol.name, "TestStruct");
            assert_eq!(symbol.container, "");
            assert_eq!(symbol.kind, MatchKind::Struct);
            assert_eq!(symbol.line, 1);
        });
    }

    #[test]
    fn check_struct_impl() {
        let src = "
            struct TestStruct {}

            impl TestStruct {
            fn test_fn() {}
            }
        ";

        test_symbols(src,
                     vec!["test_fn", "TEST_fn"],
                     &|symbol: &Match| {
                         assert_eq!(symbol.name, "test_fn");
                         assert_eq!(symbol.container, "TestStruct");
                         assert_eq!(symbol.kind, MatchKind::Method);
                         assert_eq!(symbol.line, 5);
                     });
    }

    #[test]
    fn check_struct_field() {
        let src = "
            struct TestStruct {
            test_field: u32
            }
        ";

        test_symbols(src,
                     vec!["test_field", "field"],
                     &|symbol: &Match| {
                         assert_eq!(symbol.name, "test_field");
                         assert_eq!(symbol.container, "TestStruct");
                         assert_eq!(symbol.kind, MatchKind::Field);
                         assert_eq!(symbol.line, 3);
                     });
    }

    #[test]
    fn check_function() {
        let src = "fn test_fn() {}";

        test_symbols(src,
                     vec!["test_fn", "TEST_fn"],
                     &|symbol: &Match| {
                         assert_eq!(symbol.name, "test_fn");
                         assert_eq!(symbol.container, "");
                         assert_eq!(symbol.kind, MatchKind::Function);
                         assert_eq!(symbol.line, 1);
                     });
    }

    #[test]
    fn check_constant() {
        let src = "const ConstSymbol: u32 = 0;";

        test_symbols(src,
                     vec!["ConstSymbol", "symbol"],
                     &|symbol: &Match| {
                         assert_eq!(symbol.name, "ConstSymbol");
                         assert_eq!(symbol.container, "");
                         assert_eq!(symbol.kind, MatchKind::Constant);
                         assert_eq!(symbol.line, 1);
                     });
    }

    #[test]
    fn check_static() {
        let src = "static StaticSymbol: u32 = 0;";

        test_symbols(src,
                     vec!["StaticSymbol", "symbol"],
                     &|symbol: &Match| {
                         assert_eq!(symbol.name, "StaticSymbol");
                         assert_eq!(symbol.container, "");
                         assert_eq!(symbol.kind, MatchKind::Static);
                         assert_eq!(symbol.line, 1);
                     });
    }

    #[test]
    fn check_enum() {
        let src = "enum TestEnum {}";

        test_symbols(src,
                     vec!["TestEnum", "test"],
                     &|symbol: &Match| {
                         assert_eq!(symbol.name, "TestEnum");
                         assert_eq!(symbol.container, "");
                         assert_eq!(symbol.kind, MatchKind::Enum);
                         assert_eq!(symbol.line, 1);
                     });
    }

    #[test]
    fn check_macro() {
        let src = "
            macro_rules! test_macro {
            () => ()
            }
        ";

        test_symbols(src,
                     vec!["test_macro", "test"],
                     &|symbol: &Match| {
                         assert_eq!(symbol.name, "test_macro");
                         assert_eq!(symbol.container, "");
                         assert_eq!(symbol.kind, MatchKind::Macro);
                         assert_eq!(symbol.line, 2);
                     });
    }

    #[test]
    fn check_struct_inclusive() {
        let src = "
            struct TestStruct {
                test_field: u32
            }

            impl TestStruct {
                fn test_fn() {}   
            }
        ";

        let matches = get_symbol_matches(src, "");
        assert!(matches.len() == 3);

        let ref symbol = matches[0];
        assert_eq!(symbol.name, "TestStruct");
        assert_eq!(symbol.container, "");
        assert_eq!(symbol.kind, MatchKind::Struct);
        assert_eq!(symbol.line, 2);

        let ref symbol = matches[1];
        assert_eq!(symbol.name, "test_field");
        assert_eq!(symbol.container, "TestStruct");
        assert_eq!(symbol.kind, MatchKind::Field);
        assert_eq!(symbol.line, 3);

        let ref symbol = matches[2];
        assert_eq!(symbol.name, "test_fn");
        assert_eq!(symbol.container, "TestStruct");
        assert_eq!(symbol.kind, MatchKind::Method);
        assert_eq!(symbol.line, 7);
    }

    #[test]
    fn check_trait() {
        let src = "
            trait TestTrait {
                fn test_fn();
                const test_constant: i32;
            }
        ";

        let matches = get_symbol_matches(src, "");
        assert!(matches.len() == 3);

        let ref symbol = matches[0];
        assert_eq!(symbol.name, "TestTrait");
        assert_eq!(symbol.container, "");
        assert_eq!(symbol.kind, MatchKind::Trait);
        assert_eq!(symbol.line, 2);

        let ref symbol = matches[1];
        assert_eq!(symbol.name, "test_fn");
        assert_eq!(symbol.container, "TestTrait");
        assert_eq!(symbol.kind, MatchKind::Method);
        assert_eq!(symbol.line, 3);

        let ref symbol = matches[2];
        assert_eq!(symbol.name, "test_constant");
        assert_eq!(symbol.container, "TestTrait");
        assert_eq!(symbol.kind, MatchKind::Constant);
        assert_eq!(symbol.line, 4);
    }

    #[test]
    fn check_enum_inclusive() {
        let src = "
            enum TestEnum {
                Member1,
                Member2(u32),
                Member3(TestEnum)
            }
        ";

        let matches = get_symbol_matches(src, "");
        assert!(matches.len() == 4);

        let ref symbol = matches[0];
        assert_eq!(symbol.name, "TestEnum");
        assert_eq!(symbol.container, "");
        assert_eq!(symbol.kind, MatchKind::Enum);
        assert_eq!(symbol.line, 2);

        let ref symbol = matches[1];
        assert_eq!(symbol.name, "Member1");
        assert_eq!(symbol.container, "TestEnum");
        assert_eq!(symbol.kind, MatchKind::Constant);
        assert_eq!(symbol.line, 3);

        let ref symbol = matches[2];
        assert_eq!(symbol.name, "Member2");
        assert_eq!(symbol.container, "TestEnum");
        assert_eq!(symbol.kind, MatchKind::Constant);
        assert_eq!(symbol.line, 4);

        let ref symbol = matches[3];
        assert_eq!(symbol.name, "Member3");
        assert_eq!(symbol.container, "TestEnum");
        assert_eq!(symbol.kind, MatchKind::Constant);
        assert_eq!(symbol.line, 5);
    }
}
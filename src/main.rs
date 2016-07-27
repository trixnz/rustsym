extern crate syntex_syntax as syntax;
extern crate rustc_serialize;
extern crate clap;
extern crate walkdir;
extern crate threadpool;
extern crate num_cpus;

mod visitor;

use rustc_serialize::{json, Encodable, Encoder};

use clap::{App, Arg, SubCommand, AppSettings};

use syntax::parse::{self, ParseSess};
use syntax::visit;

use std::path::Path;

#[derive(Debug)]
enum MatchKind {
    Struct,
    Method,
    Field,

    Function,

    Constant,
    Static,
    Enum,
}

// Required to convert the `MatchKind` enum fields to lowercase in the JSON output
impl Encodable for MatchKind {
    fn encode<E: Encoder>(&self, e: &mut E) -> Result<(), E::Error> {
        try!(e.emit_str(match *self {
            MatchKind::Struct => "struct",
            MatchKind::Method => "method",
            MatchKind::Field => "field",

            MatchKind::Function => "function",

            MatchKind::Constant => "constant",
            MatchKind::Static => "static",
            MatchKind::Enum => "enum",
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
    use threadpool::ThreadPool;
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

    let pool = ThreadPool::new(num_cpus::get());
    let (tx, rx) = channel();

    let num_jobs = Arc::new(AtomicUsize::new(0));

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

            let tx = tx.clone();
            let num_jobs_clone = num_jobs.clone();
            pool.execute(move || {
                let matches = search_symbol_file(&path, &query, false);
                tx.send(matches).unwrap();

                num_jobs_clone.fetch_add(1, Ordering::SeqCst);
            });
        }
    }

    // Wait for all of the jobs to finish
    while pool.active_count() > 0 {}

    // Fold the list of matches into a single list
    let num_jobs = num_jobs.load(Ordering::SeqCst);
    rx.iter().take(num_jobs).fold(vec![], |mut v, m| {
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
        query: query.into(),
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
    let app = App::new("rust-symbols")
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

use std::{error::Error, path::PathBuf, io::{BufRead, self, BufReader}, fs::File};

use regex::bytes::Regex;

fn run<R>(pat: &str, reader: R) -> Result<(), Box<dyn Error>>
where R: BufRead
{
    let re = Regex::new(pat)?;
    for l in reader.split(b'\n') {
        let rec = l?;
        if re.is_match(&rec) {
            println!("{}", String::from_utf8(rec)?);
        }
    }

    Ok(())
}

fn rawr_main(mut args: impl Iterator<Item = String>) -> Result<(), Box<dyn Error>> {
    let pat = match args.next() {
        Some(pat) => pat,
        None => panic!("usage: rawr PATTERN FILE..."),
    };
    let paths = args.map(PathBuf::from).collect::<Vec<_>>();
    if paths.is_empty() {
        let stdin = io::stdin();
        run(&pat, stdin.lock())?;
    } else {
        for p in paths {
            let f = File::open(p)?;
            run(&pat, BufReader::new(f))?;
        }
    }

    Ok(())
}

fn main() {
    let args = std::env::args().skip(1);
    let result = rawr_main(args);
    if let Err(err) = result {
        eprintln!("{}", err);
        std::process::exit(1);
    }
}

use clap::Parser;
use std::{fs::File, io::Read};

use lexer::Lexer;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about)]
struct Args{
    // Input source file to be compiled
    #[arg(short, value_name = "file_path")]
    file_path: std::path::PathBuf,
}

fn process_filepath( file_path: &std::path::PathBuf) -> String
{
    let mut file_as_string = String::new();
    match File::open(&file_path){
        Ok(mut file) => {
            if let Err(error) = file.read_to_string(&mut file_as_string){
                eprintln!("Error on reading file with error: {error}");
            }
            file_as_string
        },
        Err(e) => {
            eprintln!("Error on opening filestream to file.");
            if let Some(path_string) = file_path.to_str().map(String::from) {
                eprintln!("Unable to read file: {path_string}.");
            }
            else{
                eprintln!("Unable to read file, can't print the filename as it contains some OS specific symbol.");
            }
            eprintln!("With error: {e}");

            std::process::exit(-1);
        }
    }

}

fn main() {
    let args = Args::parse();

    if ! args.file_path.exists(){
        println!("Input file does not exist");
        std::process::exit(-1);
    }

    let file_as_string = process_filepath(&args.file_path);

    let mut lexer = Lexer::new(args.file_path, file_as_string);
    let tokens = lexer.scan();

    println!("Parsed tokens: {:?}.", &tokens);
}
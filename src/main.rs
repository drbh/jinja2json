use clap::{Parser, Subcommand};
use jinja2json::{analyze_template, reconstruct_template_formatted};
use serde_json::Value;
use std::fs;
use std::io::{self, Read};

#[derive(Parser)]
#[command(name = "jinja2json")]
#[command(about = "Convert between Jinja2 templates and JSON VM analysis")]
#[command(version)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    #[command(about = "Convert Jinja2 template to JSON VM analysis")]
    ToJson {
        #[arg(help = "Input Jinja2 template file (use '-' for stdin)")]
        input: String,
        #[arg(short, long, help = "Output JSON file (default: stdout)")]
        output: Option<String>,
        #[arg(short, long, help = "Test context JSON file for rendering")]
        context: Option<String>,
    },
    #[command(about = "Convert JSON VM analysis back to Jinja2 template")]
    ToJinja {
        #[arg(help = "Input JSON VM analysis file (use '-' for stdin)")]
        input: String,
        #[arg(short, long, help = "Output Jinja2 template file (default: stdout)")]
        output: Option<String>,
    },
}

fn main() {
    let cli = Cli::parse();

    let result = match cli.command {
        Commands::ToJson {
            input,
            output,
            context,
        } => handle_to_json(input, output, context),
        Commands::ToJinja { input, output } => handle_to_jinja(input, output),
    };

    if let Err(e) = result {
        eprintln!("Error: {}", e);
        std::process::exit(1);
    }
}

fn handle_to_json(
    input: String,
    output: Option<String>,
    context: Option<String>,
) -> Result<(), Box<dyn std::error::Error>> {
    let template_content = read_input(&input)?;

    let test_context = if let Some(context_file) = context {
        let context_str = fs::read_to_string(&context_file)?;
        Some(serde_json::from_str(&context_str)?)
    } else {
        None
    };

    let template_path = if input == "-" { "stdin" } else { &input };
    let analysis = analyze_template(&template_content, template_path, test_context)?;

    let json_output = serde_json::to_string_pretty(&analysis)?;

    write_output(&json_output, output)?;
    Ok(())
}

fn handle_to_jinja(
    input: String,
    output: Option<String>,
) -> Result<(), Box<dyn std::error::Error>> {
    let json_content = read_input(&input)?;
    let vm_analysis: Value = serde_json::from_str(&json_content)?;

    let template = reconstruct_template_formatted(&vm_analysis)?;

    write_output(&template, output)?;
    Ok(())
}

fn read_input(input: &str) -> Result<String, Box<dyn std::error::Error>> {
    if input == "-" {
        let mut buffer = String::new();
        io::stdin().read_to_string(&mut buffer)?;
        Ok(buffer)
    } else {
        Ok(fs::read_to_string(input)?)
    }
}

fn write_output(content: &str, output: Option<String>) -> Result<(), Box<dyn std::error::Error>> {
    match output {
        Some(file_path) => {
            fs::write(&file_path, content)?;
            eprintln!("Output written to: {}", file_path);
        }
        None => {
            print!("{}", content);
        }
    }
    Ok(())
}

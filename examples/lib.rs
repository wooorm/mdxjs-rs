extern crate mdxjs;

fn main() -> Result<(), String> {
    println!("{}", mdxjs::compile("## Hi there!", &Default::default())?);

    Ok(())
}

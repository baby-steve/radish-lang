use radish_lang::common::source::Source;

use radish_lang::Radish;

fn main() {
    println!("Hello, Radish!");

    let mut radish = Radish::new();

    let source = Source::new(
        "
        fun main(a, b) {
            var i = 0
            while i < a loop
                i += 1
                if i == b then
                    print \"a is equal to b\"
                else
                    print i
                endif
            endloop
        }

        main(13, 8)
        ",
        "./test_file",
    );

    radish.run_from_source(source);
}

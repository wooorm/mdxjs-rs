# mdxjs-rs

[![Build][build-badge]][build]
[![Coverage][coverage-badge]][coverage]
[![GitHub][repo-badge]][repo]
[![docs.rs][docs-badge]][docs]
[![crates.io][crate-badge]][crate]

Compile MDX to JavaScript in Rust.

## When should I use this?

You can use this Rust crate when you’re dealing with the Rust language and want
to compile MDX to JavaScript.
To parse the MDX format to a syntax tree, use [`markdown-rs`][markdown-rs] instead.

This project does not yet support plugins.
To benefit from the unified (remark and rehype) ecosystem, use
[`@mdx-js/mdx`][mdx-js].

## What is this?

This Rust crate works exactly like the npm package [`@mdx-js/mdx`][mdx-js].
It uses the Rust crates [`markdown-rs`][markdown-rs] and [SWC][] to deal with the
markdown and JavaScript inside MDX.

## Questions

*   to learn MDX, see [`mdxjs.com`][mdx-site]
*   for the API, see the [crate docs][docs]
*   for questions, see [Discussions][chat]
*   to help, see [contribute][] or [sponsor][] below

## Contents

*   [Install](#install)
*   [Use](#use)
*   [API](#api)
*   [Project](#project)
    *   [Test](#test)
    *   [Version](#version)
    *   [Security](#security)
    *   [Contribute](#contribute)
    *   [Sponsor](#sponsor)
    *   [Thanks](#thanks)
*   [License](#license)

## Install

With [Rust][] (rust edition 2018+, ±version 1.56+), install with `cargo`:

```sh
cargo add mdxjs
```

## Use

```rs
extern crate mdxjs;

fn main() {
    To do.
}
```

Yields:

```javascript
To do.
```

To do: example with options.

## API

`mdxjs-rs` exposes
[`compile`](https://docs.rs/mdxjs/0.0.0/mdxjs/fn.compile.html),
[`JsxRuntime`](https://docs.rs/mdxjs/0.0.0/mdxjs/struct.JsxRuntime.html),
[`Options`](https://docs.rs/mdxjs/0.0.0/mdxjs/struct.Options.html),
and a few other structs and enums.

See the [crate docs][docs] for more info.

## Project

### Test

`mdxjs-rs` is tested with a lot of tests.
These tests reach all branches in the code, which means that this project has
100% code coverage.

The following bash scripts are useful when working on this project:

*   run examples:
    ```sh
    RUST_BACKTRACE=1 RUST_LOG=debug cargo run --example lib
    ```
*   format:
    ```sh
    cargo fmt
    ```
*   lint:
    ```sh
    cargo fmt --check && cargo clippy --examples --tests --benches
    ```
*   test:
    ```sh
    RUST_BACKTRACE=1 cargo test
    ```
*   docs:
    ```sh
    cargo doc --document-private-items
    ```

### Version

`mdxjs-rs` follows [SemVer](https://semver.org).

### Security

MDX is a programming language.
It is JavaScript.
It is not safe to let people you don’t trust write MDX.

### Contribute

See [`contributing.md`][contributing] for ways to help.
See [`support.md`][support] for ways to get help.
See [`code-of-conduct.md`][coc] for how to communicate in and around this
project.

### Sponsor

Support this effort and give back by sponsoring:

*   [GitHub Sponsors](https://github.com/sponsors/wooorm)
    (personal; monthly or one-time)
*   [OpenCollective](https://opencollective.com/unified) or
    [GitHub Sponsors](https://github.com/sponsors/unifiedjs)
    (unified; monthly or one-time)

### Thanks

Special thanks go out to:

*   [Vercel][] for funding the initial development

## License

[MIT][license] © [Titus Wormer][author]

[build-badge]: https://github.com/wooorm/mdxjs-rs/workflows/main/badge.svg

[build]: https://github.com/wooorm/mdxjs-rs/actions

[coverage-badge]: https://img.shields.io/codecov/c/github/wooorm/mdxjs-rs.svg

[coverage]: https://codecov.io/github/wooorm/mdxjs-rs

[repo-badge]: https://img.shields.io/badge/GitHub-wooorm%2Fmdxjs--rs-brightgreen

[repo]: https://github.com/wooorm/mdxjs-rs

[docs-badge]: https://img.shields.io/docsrs/mdxjs

[docs]: https://docs.rs/mdxjs/

[crate-badge]: https://img.shields.io/crates/v/mdxjs

[crate]: https://crates.io/crates/mdxjs/

[chat]: https://github.com/wooorm/mdxjs-rs/discussions

[license]: license

[author]: https://wooorm.com

[mdx-js]: https://mdxjs.com/packages/mdx/

[mdx-site]: https://mdxjs.com

[markdown-rs]: https://github.com/wooorm/markdown-rs

[swc]: https://swc.rs

[rust]: https://www.rust-lang.org

[vercel]: https://vercel.com

[contribute]: #contribute

[sponsor]: #sponsor

[contributing]: .github/contribute.md

[support]: .github/support.md

[coc]: .github/code-of-conduct.md

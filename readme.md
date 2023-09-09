# mdxjs-rs

[![Build][build-badge]][build]
[![Coverage][coverage-badge]][coverage]
[![GitHub][repo-badge]][repo]
[![docs.rs][docs-badge]][docs]
[![crates.io][crate-badge]][crate]

Compile MDX to JavaScript in Rust.

## When should I use this?

You can use this crate when you’re dealing with the Rust language and want
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

fn main() -> Result<(), String> {
    println!(
        "{}",
        mdxjs::compile(
            r###"
import {Chart} from './snowfall.js'
export const year = 2018

# Last year’s snowfall

In {year}, the snowfall was above average.
It was followed by a warm spring which caused
flood conditions in many of the nearby rivers.

<Chart year={year} color="#fcb32c" />
"###,
            &Default::default()
        )?
    );

    Ok(())
}
```

Yields (prettified):

```javascript
import {Fragment as _Fragment, jsx as _jsx, jsxs as _jsxs} from 'react/jsx-runtime'
import {Chart} from './snowfall.js'
export const year = 2018

function _createMdxContent(props) {
  const _components = Object.assign({h1: 'h1', p: 'p'}, props.components)
  return _jsxs(_Fragment, {
    children: [
      _jsx(_components.h1, {children: 'Last year’s snowfall'}),
      '\n',
      _jsxs(_components.p, {
        children: [
          'In ',
          year,
          ', the snowfall was above average.\nIt was followed by a warm spring which caused\nflood conditions in many of the nearby rivers.'
        ]
      }),
      '\n',
      _jsx(Chart, {year: year, color: '#fcb32c'})
    ]
  })
}

function MDXContent(props = {}) {
  const {wrapper: MDXLayout} = props.components || {}
  return MDXLayout
    ? _jsx(MDXLayout, Object.assign({}, props, {children: _jsx(_createMdxContent, props)}))
    : _createMdxContent(props)
}

export default MDXContent
```

## API

`mdxjs-rs` exposes
[`compile`](https://docs.rs/mdxjs/latest/mdxjs/fn.compile.html),
[`JsxRuntime`](https://docs.rs/mdxjs/latest/mdxjs/enum.JsxRuntime.html),
[`Options`](https://docs.rs/mdxjs/latest/mdxjs/struct.Options.html),
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
    RUST_BACKTRACE=1 cargo run --example lib
    ```
*   format:
    ```sh
    cargo fmt && cargo fix
    ```
*   lint:
    ```sh
    cargo fmt --check && cargo clippy --all-targets
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

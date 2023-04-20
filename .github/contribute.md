# Contribute

> 👉 **Important**: this project has a [code of conduct][coc].
> By interacting with this repository and community you agree to abide by its
> terms.

This article explains how to contribute.
Please read through the following guidelines.

## Contributions

There are several ways to contribute, not just by writing code.
See [Support][] if you have questions.

### Financial support

You can help financially.
See [Sponsor][] for more info.

### Improve docs

As a user you’re perfect to help improve the docs.
Typo corrections, error fixes, better explanations, new examples, etcetera.

### Improve issues

Some issues lack information, aren’t reproducible, or are just incorrect.
You can help by trying to make them easier to resolve.
Existing issues might benefit from your unique experience or opinions.

### Write code

Code contributions are very welcome too.
It’s probably a good idea to first post a question or open an issue to report a
bug or suggest a new feature before creating a pull request.
See [Project][] for more info.

## Submitting an issue

*   the issue tracker is for issues, discussions are for questions
*   search the issue tracker (including closed issues) before opening a new
    issue
*   ensure you’re using the latest versions of packages and other tools
*   use a clear and descriptive title
*   include as much information as possible: steps to reproduce the issue,
    error message, version, operating system, etcetera
*   the more time you put into an issue, the better help you can get
*   the best issue report is a [failing test][unit-test] proving it

## Submitting a pull request

*   run `cargo fmt` and `cargo test` locally to format and test your changes
*   non-trivial changes are often best discussed in an issue first, to prevent
    you from doing unnecessary work
*   for ambitious tasks, you should try to get your work in front of the
    community for feedback as soon as possible
*   new features should be accompanied by tests and documentation
*   don’t include unrelated changes
*   write a convincing description of why your pull request should land:
    it’s your job to be convincing

## Project (for maintainers)

See [Project][project] in the readme for info on how the project is structured
and how to run useful scripts.

### Commit

Look at the commits in the project for the style being used.

For example:

```git-commit
Update `swc_core`

Some long description here

Closes GH-24.
```

Some points:

*   short descriptive message as title
*   no issue/PR references in title
*   reference the issues/PRs that are closed in the commit body
*   optionally you can include who reviewed or co-authored:
    ```
    Reviewed-by: Titus Wormer <tituswormer@gmail.com>

    Co-authored-by: Titus Wormer <tituswormer@gmail.com>
    ```

### Release

Perform the following steps locally, no PR needed:

*   update the `version` field in `Cargo.toml`
*   `git commit --all --message 1.2.3 && git tag 1.2.3 && git push && git push --tags`
*   `cargo publish`

For the release notes, here’s what I do.
You can also look at the existing release notes for how to do it.

*   go to releases: <https://github.com/wooorm/mdxjs-rs/releases>
*   click “Draft a new release”
*   click “Choose a release”, choose the one you just released
*   click “Generate release notes”, it might generate for example:
    ```markdown
    ## What's Changed
    * Update `swc_core` by @kdy1 in https://github.com/wooorm/mdxjs-rs/pull/25

    **Full Changelog**: https://github.com/wooorm/mdxjs-rs/compare/0.1.10...0.1.11
    ```
*   locally I run `git l` (git alias for
    `l = log --pretty=oneline --graph --abbrev-commit`) to produce a markdown
    list of the commits, such as:
    ```markdown
    * 4513866 (HEAD -> main, tag: 0.1.11, origin/main) 0.1.11
    * 833eacf Update `swc_core`
    ```
*   finally I manually merge the two results to get:
    ```
    * 833eacf Update `swc_core`
      by @kdy1 in https://github.com/wooorm/mdxjs-rs/pull/25

    **Full Changelog**: https://github.com/wooorm/mdxjs-rs/compare/0.1.10...0.1.11
    ```
*   for long release notes with important info, I think about what a reader
    wants and needs.
    What is breaking?
    What is actually important?
    Sometimes I reorder and amend stuff to highlight what’s important and how
    users need to migrate!

## Resources

*   [how to contribute to open source](https://opensource.guide/how-to-contribute/)
*   [making your first contribution](https://medium.com/@vadimdemedes/making-your-first-contribution-de6576ddb190)
*   [using pull requests](https://help.github.com/articles/about-pull-requests/)
*   [GitHub help](https://help.github.com)

## License

[CC-BY-4.0][license] © [Titus Wormer][author]

<!-- Definitions -->

[license]: https://creativecommons.org/licenses/by/4.0/

[author]: https://wooorm.com

[unit-test]: https://twitter.com/sindresorhus/status/579306280495357953

[support]: support.md

[coc]: code-of-conduct.md

[sponsor]: https://github.com/wooorm/mdxjs-rs/#sponsor

[project]: https://github.com/wooorm/mdxjs-rs/#project

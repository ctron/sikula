# SiKuLa

[![CI](https://github.com/ctron/sikula/workflows/CI/badge.svg)](https://github.com/ctron/sikula/actions?query=workflow%3A%22CI%22)
[![GitHub release (latest SemVer)](https://img.shields.io/github/v/tag/ctron/sikula?sort=semver)](https://github.com/ctron/sikula/releases)
[![crates.io](https://img.shields.io/crates/v/sikula.svg)](https://crates.io/crates/sikula)
[![docs.rs](https://docs.rs/sikula/badge.svg)](https://docs.rs/sikula)

<u>Si</u>mple <u>Qu</u>ery <u>La</u>nguage - [ˈziːˈkuːˈlaː]

## Rationale

_Another query language, are you serious?_

Actually it isn't that new. But naming it "Query language similar to GitHub's search syntax" (QLSTGHSS)
wasn't a real option.

Think of it more as an implementation of a familiar syntax.

_What's the difference then?_

They are subtle. But I don't want to spoil the surprise. Or maybe I am just too lazy documenting it. 🤷

## Example

Assuming you have an enum defined for searching e-mails:

```rust
use sikula::prelude::*;

#[derive(Search, Clone, Debug, PartialEq, Eq)]
enum DeriveResource<'a> {
    /// Standard qualifier: `author:someone`.
    #[search(sort, scope)]
    Author(&'a str),
    /// Default primary: `warranty`.
    #[search(default)]
    Subject(Primary<'a>),
    /// Non-default primary: `warranty in:message`, to search in both: `warranty in:message in:subject`.
    #[search(scope)]
    Message(Primary<'a>),

    /// Predicate: `is:read`.
    Read,

    /// Numeric qualifier example:
    /// * `size:100` (equals)
    /// * `size:>=100` (size greater than or equals 100)
    /// * `size:100..200` (size between 100 inclusive and 200 exclusive)
    /// * `size:*..200` (size up to 200 exclusive)
    #[search(sort)]
    Size(Ordered<usize>),

    #[search(sort)]
    Sent(Ordered<time::OffsetDateTime>),

    Label(Qualified<'a, &'a str>),
}
```

The `Search` derive provides the trait implementation. The `#[search(default)]` attribute flags the variant `Subject`
as default scopes for the primary search terms, marking `Subject` as the default if none was selected.

In general, there are three types of terms: Primary, Qualifiers, Predicates. Predicates are simple "is this condition
true" style of filters. If an enum variant doesn't have any value, it is a predicate.

Qualifiers are additional matching criteria, which depend on the type of the value.

With the `#[search(sort)]` flag, a field can be used for sorting the result. 

Now, you can do the following queries:

| Query                                             | Retrieves all entries…                                                                                |
|---------------------------------------------------|-------------------------------------------------------------------------------------------------------|
| `foo`                                             | … containing "foo" in the "subject"                                                                   |
| `foo in:subject in:message`                       | … containing "foo" in either "subject" or "message"                                                   |
| `foo in:subject in:message is:read`               | … containing "foo" in either "subject" or "message" being "read"                                      |
| `foo bar`                                         | … containing "foo" and "bar" in the "subject"                                                         |
| `size:>10000`                                     | … having the "size" greater than 10000                                                                |
| `size:100..200`                                   | … having the "size" between 100 (inclusive) and 200 (exclusive)                                       |
| `-is:read`                                        | … being "not read"                                                                                    |
| `foo sort:sent`                                   | … containing "foo" in the "subject", sorted by "sent" ascending                                       |
| `foo -sort:sent`                                  | … containing "foo" in the "subject", sorted by "sent" descending                                      |
| `author:"Max Mustermann"`                         | … having the "author" of `Max Mustermann`                                                             |
| `author:"Max Mustermann" author:"Eva Mustermann"` | … having the "author" of `Max Mustermann` and `Eva Mustermann` (most likely no results will be found) |
| `author:"Max Mustermann","Eva Mustermann"`        | … having the "author" of `Max Mustermann` or `Eva Mustermann`                                         |
| `foo OR bar`                                      | … containing "foo" or "bar" in the "subject"                                                          |
| `foo AND bar`                                     | … containing "foo" and "bar" in the "subject"                                                         |
| `foo OR bar AND baz`                              | … containing either "foo" or ( "bar" and "baz" ) in the "subject"                                     |
| `(foo OR bar) AND baz`                            | … containing ( "foo" or "bar" ) and "baz" in the "subject"                                            |
| `foo OR bar baz`                                  | … containing ( "foo" or "bar" ) and "baz" in the "subject"                                            |

For testing more examples with the resource above, you can run the `cli` example:

```shell
cargo run --example cli --features time -- -is:read AND foo
```

Which will give you a structured output of the parsed query:

```
Input: '-is:read AND foo'

MIR:
Query {
    term: And {
        scopes: [],
        terms: [
            Not(
                Match {
                    qualifier: Qualifier(
                        [
                            "read",
                        ],
                    ),
                    expression: Predicate,
                },
            ),
            Match {
                qualifier: Qualifier(
                    [],
                ),
                expression: Simple(
                    "foo",
                ),
            },
        ],
    },
    sorting: [],
}

LIR:
Query {
    terms: And(
        [
            Not(
                Match(
                    Read,
                ),
            ),
            Match(
                Subject(
                    Partial(
                        "foo",
                    ),
                ),
            ),
        ],
    ),
    sorting: [],
}
```

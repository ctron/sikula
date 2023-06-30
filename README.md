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

#[derive(Search)]
pub enum MyResource<'a> {
     #[search(scope, default)]
     Subject(Primary<'a>),
     #[search(scope)]
     Body(Primary<'a>),

     Sender(&'a str),

     #[search(sort)]
     Sent(Ordered<time::OffsetDateTime>),
     #[search(sort)]
     Size(Ordered<usize>),

     #[search(sort)]
     Header(Qualified<'a, &'a str>),

     Read,
     Important,
 }
```

The `Query` derive provides the trait implementation. The `#[query(scope)]` attribute flags the variant `Subject`
as `Body` scopes for the primary search terms, marking `Subject` as the default if none was selected.

In general, there are three types of terms: Primary, Qualifiers, Predicates. Predicates are simple "is this condition
true" style of filters. If an enum variant doesn't have any value, it is a predicate.

Qualifiers are additional matching criteria, which depend on the type of the value.

With the `#[query(sort)]` flag, a field can be used for sorting the result. 

Now, you can do the following queries:

| Query                                             | Retrieves all entries…                                                                            |
|---------------------------------------------------|---------------------------------------------------------------------------------------------------|
| `foo`                                             | … containing "foo" in the "subject"                                                               |
| `foo in:subject in:body`                          | … containing "foo" in either "subject" or "body"                                                  |
| `foo in:subject in:body is:read`                  | … containing "foo" in either "subject" or "body" being "read"                                     |
| `foo bar`                                         | … containing "foo" and "bar" in the subject                                                       |
| `size:>10000`                                     | … having a size greater than 10000                                                                |
| `size:100..200`                                   | … having a size between 100 (inclusive) and 200 (exclusive)                                       |
| `-is:read`                                        | … being "not read"                                                                                |
| `foo sort:sent`                                   | … containing "foo" in the subject, sorted by "sent" ascending                                     | 
| `foo -sort:sent`                                  | … containing "foo" in the subject, sorted by "sent" descending                                    |
| `sender:"Max Mustermann"`                         | … having a sender of `Max Mustermann`                                                             |
| `sender:"Max Mustermann" sender:"Eva Mustermann"` | … having a sender of `Max Mustermann` and `Eva Mustermann` (most likely no results will be found) |
| `sender:"Max Mustermann","Eva Mustermann"`        | … having a sender of `Max Mustermann` or `Eva Mustermann`                                         |
| `foo OR bar`                                      | … containing "foo" or "bar" in the "subject"                                                      |
| `foo AND bar`                                     | … containing "foo" and "bar" in the "subject"                                                     |
| `foo OR bar AND baz`                              | … containing either "foo" or ( "bar" and "baz" ) in the "subject"                                 |
| `(foo OR bar) AND baz`                            | … containing ( "foo" or "bar" ) and "baz" in the "subject"                                        |
| `foo OR bar baz`                                  | … containing ( "foo" or "bar" ) and "baz" in the "subject"                                        |

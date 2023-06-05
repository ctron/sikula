use convert_case::{Case, Casing};
use darling::util::Flag;
use darling::FromVariant;
use proc_macro2::{Ident, TokenStream};
use quote::quote;
use syn::{parse_macro_input, Data, DataEnum, DeriveInput};

#[proc_macro_derive(Search, attributes(search))]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let DeriveInput {
        ident,
        data,
        generics: _,
        ..
    } = parse_macro_input!(input as DeriveInput);

    let data = match data {
        Data::Enum(data) => data,
        _ => panic!("Derive can only be used on enum types"),
    };

    let info = collect(&data);

    let expanded_sortables = expand_sortables(&ident, &info.sortables);
    let expanded_scopes = expand_scopes(&ident, &info.scopes);
    let expand_search = expand_search(&ident, &info);

    let expanded = quote! {
        #expanded_sortables

        #expanded_scopes

        #expand_search
    };

    proc_macro::TokenStream::from(expanded)
}

fn expand_from_names<'a>(
    name: Ident,
    names: impl IntoIterator<Item = (&'a str, &'a Ident)>,
) -> TokenStream {
    let (variants, mappings): (Vec<_>, Vec<_>) = names
        .into_iter()
        .map(|(name, ident)| {
            (
                quote! { #ident },
                quote! {
                    [ #name ] => Self::#ident,
                },
            )
        })
        .unzip();

    let body = match mappings.is_empty() {
        true => quote! {
            Err(())
        },
        false => quote! {
            Ok(match qualifier.as_slice() {
                #(#mappings)*
                _ => return Err(()),
            })
        },
    };

    quote! {
        #[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
        pub enum #name {
            #(#variants, )*
        }

        impl sikula::prelude::FromQualifier for #name {
            type Err = ();

            fn from_qualifier(qualifier: &sikula::prelude::Qualifier) -> Result<Self, Self::Err> {
                #body
            }
        }
    }
}

fn expand_sortables(base: &Ident, sortables: &[Sortable]) -> TokenStream {
    let name = Ident::new(&format!("{}Sortable", base), base.span());
    expand_from_names(
        name,
        sortables
            .iter()
            .map(|sortable| (sortable.name.as_str(), &sortable.ident)),
    )
}

fn expand_scopes(base: &Ident, scopes: &[Scope]) -> TokenStream {
    let name = Ident::new(&format!("{}Scope", base), base.span());
    expand_from_names(
        name,
        scopes
            .iter()
            .map(|scope| (scope.name.as_str(), &scope.ident)),
    )
}

fn expand_search(ident: &Ident, info: &Info) -> TokenStream {
    let default_scope = info
        .scopes
        .iter()
        .filter(|scope| scope.default)
        .map(|scope| {
            let ident = &scope.ident;
            quote! { Self::Scope::#ident }
        });

    let ident_sortable = Ident::new(&format!("{}Sortable", ident), ident.span());
    let ident_scope = Ident::new(&format!("{}Scope", ident), ident.span());

    let match_predicates = info.predicates.iter().map(|predicate| {
        let value = &predicate.name;
        let ident = &predicate.ident;
        quote! {
            [#value] => Term::Match(Self::#ident)
        }
    });
    let match_qualifiers = info.qualifiers.iter().map(|qualifier| {
        let value = &qualifier.name;
        let ident = &qualifier.ident;
        quote! {
            [#value, n @ ..] => sikula::prelude::Term::Match(Self::#ident (
                expression.into_expression(sikula::prelude::QualifierContext::Qualifier, n.into())?,
            ))
        }
    });
    let match_primaries = info
        .scopes
        .iter()
        .map(|scope| {
            let ident = &scope.ident;
            quote! {
                Self::Scope::#ident => {
                    sikula::prelude::Term::Match(Self::#ident(expression.into_expression(
                        sikula::prelude::QualifierContext::Primary,
                        sikula::prelude::Qualifier::empty(),
                    )?))
                }
            }
        })
        .collect::<Vec<_>>();

    let primaries = match match_primaries.is_empty() {
        true => {
            quote! {}
        }
        false => {
            quote! {
                [] => {
                    let mut terms = vec![];
                    for scope in &scopes {
                        let expression = match scope {
                            #(#match_primaries, )*
                        };
                        terms.push(expression);
                    }
                    sikula::prelude::Term::Or(terms)
                },
            }
        }
    };

    quote! {
        impl<'a> sikula::prelude::Search<'a> for #ident <'a> {
            type Parsed = #ident<'a>;
            type Sortable = #ident_sortable;
            type Scope = #ident_scope;

            fn default_scopes() -> Vec<Self::Scope> {
                vec![ #(#default_scope, )* ]
            }

            fn parse(q: &'a str) -> Result<Query<Self>, sikula::lir::Error> {
                use chumsky::Parser;

                let query = sikula::mir::Query::parse(parser().parse(q).into_result().map_err(|s| {
                    sikula::lir::Error::Parser(
                        s.into_iter()
                            .map(|s| s.to_string())
                            .collect::<Vec<_>>()
                            .join("\n"),
                    )
                })?)?;

                let scopes = if query.scope.is_empty() {
                    Self::default_scopes()
                } else {
                    let mut scopes = Vec::with_capacity(query.scope.len());
                    for qualifier in query.scope {
                        scopes.push(
                            Self::Scope::from_qualifier(&qualifier)
                                .map_err(|()| sikula::lir::Error::UnknownScopeQualifier(qualifier))?,
                        );
                    }
                    scopes
                };

                let mut terms = vec![];
                for term in query.terms {
                    let invert = term.invert;
                    let mut term = match term.expression {
                        sikula::mir::Expression::Predicate => match term.qualifier.as_slice() {
                            #(#match_predicates, )*
                            _ => return Err(sikula::lir::Error::UnknownPredicate(term.qualifier)),
                        },
                        sikula::mir::Expression::Simple(expression) => match term.qualifier.as_slice() {
                            #primaries
                            #(#match_qualifiers, )*
                            _ => return Err(sikula::lir::Error::UnknownQualifier(term.qualifier)),
                        },
                    };

                    if invert {
                        term = sikula::prelude::Term::Not(Box::new(term));
                    }

                    terms.push(term);
                }

                let mut sorting = vec![];
                for sort in query.sorting {
                    sorting.push(Sort {
                        qualifier: Self::Sortable::from_qualifier(&sort.qualifier)
                            .map_err(|()| sikula::lir::Error::UnknownSortQualifier(sort.qualifier))?,
                        direction: sort.direction,
                    })
                }

                Ok(Query {
                    term: sikula::prelude::Term::And(terms).compact(),
                    sorting,
                })
            }
        }
    }
}

struct Info {
    qualifiers: Vec<Qualifier>,
    predicates: Vec<Predicate>,
    sortables: Vec<Sortable>,
    scopes: Vec<Scope>,
}

struct Qualifier {
    ident: Ident,
    name: String,
}

struct Predicate {
    ident: Ident,
    name: String,
}

struct Sortable {
    ident: Ident,
    name: String,
}

struct Scope {
    ident: Ident,
    name: String,
    default: bool,
}

#[derive(FromVariant, Default)]
#[darling(default, attributes(search))]
struct VariantOpts {
    sort: Flag,
    scope: Flag,
    // default scope
    default: Flag,
}

fn collect(data: &DataEnum) -> Info {
    let mut qualifiers = vec![];
    let mut predicates = vec![];
    let mut sortables = vec![];
    let mut scopes = vec![];

    for variant in &data.variants {
        let name = variant.ident.to_string();
        let name = name.to_case(Case::Camel);
        let opts = VariantOpts::from_variant(&variant).expect("Unable to parse variant options");

        if opts.sort.is_present() {
            sortables.push(Sortable {
                ident: variant.ident.clone(),
                name: name.clone(),
            })
        }

        if opts.default.is_present() {
            scopes.push(Scope {
                ident: variant.ident.clone(),
                name: name.clone(),
                default: true,
            })
        } else if opts.scope.is_present() {
            scopes.push(Scope {
                ident: variant.ident.clone(),
                name: name.clone(),
                default: false,
            })
        }

        let num_fields = variant.fields.len();

        if num_fields == 0 {
            // predicate
            predicates.push(Predicate {
                ident: variant.ident.clone(),
                name,
            })
        } else if num_fields == 1 {
            qualifiers.push(Qualifier {
                ident: variant.ident.clone(),
                name,
            });
        } else {
            panic!("Variant must have zero or one field exactly");
        }
    }

    Info {
        qualifiers,
        predicates,
        sortables,
        scopes,
    }
}

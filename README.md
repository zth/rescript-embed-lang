# rescript-embed-lang

A general purpose PPX and library for embedding other languages into ReScript, via code generation.

The PPX itself is very very simple - just swap out the embedded language string with a reference to the code generated for that embed. The code generation **happens elsewhere**. This way embedding languages is flexible and light weight.

> This package will eventually ship with a set of utils for making the code generation part easy to set up as well.

## Installation

```bash
npm i rescript-embed-lang
```

And then add the PPX to your `rescript.json`:

```json
"ppx-flags": ["rescript-embed-lang/ppx"]
```

There, all set!

### Why one general PPX

PPXes can be complex and difficult to maintain, and costs at bit of performance. Therefore, this PPX is intended to be extended to support as many use cases around embedding other languages into ReScript as possible. This way, all language embeds built can use one central PPX rather than implementing their own. Maintenance becomes drastically easier, and performance is only hit once if you use several language embeds.

## Supported extensions

### EdgeQL

You can embed [EdgeQL](https://www.edgedb.com/) directly as an assignment to a `let` binding:

```rescript
// Movies.res
let findMovieQuery = %edgeql(`
  # @name findMovieQuery
  select Movie {
    id
    title
  } filter .id = <uuid>$movieId
`)
```

Is transformed into:

```rescript
// Movies.res
let findMovieQuery = Movies__edgedb.FindMovieQuery.query
```

You can also embed it via a module, in case you want easy access to all of the things emitted in the generated code:

```rescript
// Movies.res
module FindMovieQuery = %edgeql(`
  # @name findMovieQuery
  select Movie {
    id
    title
  } filter .id = <uuid>$movieId
`)
```

Is transformed into:

```rescript
// Movies.res
module FindMovieQuery = Movies__edgedb.FindMovieQuery
```

### Generic transform

`rescript-embed-lang` ships with a _generic transform_, intended to make experimenting with writing new language embeds + generating code for them much easier in user land, without needing you to add a full transform to this PPX. It expects a specific structure (more below) in order to connect your generated code with your ReScript source.

You turn it on by passing `-enable-generic-transform` in your PPX flags config:

```json
"ppx-flags": [["rescript-embed-lang/ppx", "-enable-generic-transform"]]
```

It works like this:

```rescript
// SomeFile.res
let myThing = %generated.css(`
  .button {
    color: blue;
  }
`)
```

This will be transformed into:

```rescript
// SomeFile.res
let myThing = SomeFile__css.M1.default
```

It also works with module references:

```rescript
// SomeFile.res
module MyThing = %generated.css(`
  .button {
    color: blue;
  }
`)
```

Is transformed into:

```rescript
// SomeFile.res
module MyThing = SomeFile__css.M1
```

> Notice that you can put anything to the right of `%generated`. The example shows `css`, but you could use anything else as well. Example: `%generated.openapi("...")`.

The formula for what code to refer to when transforming is be: `<filename>__<generated-extension>.M<module-count-for-extension>.default`. When using module bindings, the last part `.default` is omitted.

1. We're in `SomeFile.res` and using `generated.css`, so the generated module is expected to be called `SomeFile__css`.
2. Each submodule in your generated file will be called `M` + what number of transform for that extension it is, in the local file. So, the first `%generated.css` module is `M1`, the second in that same file is `M2`, and so on.
3. Finally, we add a generic `default` a target value name, just to have something to refer to.

> Remember, the actual codegen creating the module we're referring to here from the source `css` text isn't part of this package. This package is just about making it simple to tie together generated things with its source in ReScript.


### Deterministic named generation

Generators can opt into one generated file per embed by deriving a stable name with the same ECMAScript regular expression in Node and the native PPX:

```rescript
let embed = RescriptEmbedLang.make(
  ~extensionPattern=Generic("gqlExternalSchema"),
  ~generatedName=Regex({
    pattern: "^[ \\t]*(?:query|mutation|subscription)[ \\t\\r\\n]+([_A-Za-z][_0-9A-Za-z]*)",
    flags: "m",
    capture: Numbered(1),
    cardinality: ExactlyOne,
  }),
  ~setup=RescriptEmbedLang.defaultSetup,
  ~generate,
  ~cliHelpText,
)
```

Value embeds are the primary API:

```rescript
// Ga4Setup.res
let query = %generated.gqlExternalSchema(`
  query Ga4Properties {
    ga4Properties {
      id
    }
  }
`)

await client->run(query, variables)
```

They also work inline in any expression position:

```rescript
await client->run(
  %generated.gqlExternalSchema(`
    query Ga4Properties {
      ga4Properties {
        id
      }
    }
  `),
  variables,
)
```

The generator emits the stable module `Ga4Setup__gqlExternalSchema__Ga4Properties.res`. Its generated content is exposed at the module root, so a GraphQL generator can provide `variables`, `response`, `operation`, and `default`. A value embed expands directly to:

```rescript
Ga4Setup__gqlExternalSchema__Ga4Properties.default
```

Use a module embed when callers also want a convenient local name for the generated types and operation:

```rescript
module Ga4Properties = %generated.gqlExternalSchema(`
  query Ga4Properties {
    ga4Properties {
      id
    }
  }
`)

type variables = Ga4Properties.variables
type response = Ga4Properties.response

await client->run(Ga4Properties.default, variables)
```

Generation must run before ReScript compilation. There are no source hashes in the generated API or PPX target; operation names provide stable generated filenames and module references.

The generator writes the versioned PPX configuration itself, so the regular expression is not duplicated in `rescript.json`:

```bash
my-generator generate --src ./src --output ./lib/bs \
  --embed-lang-config ./lib/bs/rescript-embed-lang.json
```

Pass that file explicitly to the PPX:

```json
{
  "ppx-flags": [
    [
      "rescript-embed-lang/ppx",
      "-enable-generic-transform",
      "-embed-lang-config",
      "./lib/bs/rescript-embed-lang.json"
    ]
  ]
}
```

Generation is staged before commit, detects case-insensitive and user-module collisions, removes only files recorded in its ownership index, and includes extra emitted artifacts in the same transaction. Watch runs are serialized and coalesced.

`Sequential` remains the default, preserving the existing `M1`, `M2`, and monolithic-file behavior.


### SQL

Embedding for Postgres SQL via [pgtyped-rescript](https://github.com/zth/pgtyped-rescript).

```rescript
// Movies.res
let findMovieQuery = %sql.one(`
  /* @name findMovieQuery */
  select id, title from movies where id = :id
`)
```

Is transformed into:

```rescript
// Movies.res
let findMovieQuery = Movies__sql.FindMovieQuery.one
```

## Adding more language embeds

Adding more embeds should be straight forward. Reach out if you're interested!

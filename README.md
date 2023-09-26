# rescript-embed-lang

A general purpose PPX and library for embedding other languages into ReScript, via code generation.

The PPX itself is very very simple - just swap out the embedded language string with a reference to the code generated for that embed. The code generation **happens elsewhere**. This way embedding languages is flexible and light weight.

> This package will eventually ship with a set of utils for making the code generation part easy to set up as well.

## Installation

```bash
npm i rescript-embed-lang
```

And then add the PPX to your `bsconfig.json`:

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

### SQL (WIP)

This is not finished yet, but will provide embedding for Postgres SQL via [pgtyped-rescript](https://github.com/zth/pgtyped-rescript).

```rescript
// Movies.res
let findMovieQuery = %sql(`
  /* @name findMovieQuery */
  select id, title from movies where id = :id
`)
```

Is transformed into:

```rescript
// Movies.res
let findMovieQuery = Movies__sql.FindMovieQuery.query
```

### Generic transform (WIP)

It would be easy to extend `rescript-embed-lang` to support a _generic_ transform. This would make experimenting with writing new language embeds + generating code for them much easier in user land, without needing you to add a full transform to this PPX. It could look like this:

```rescript
// SomeFile.res
let myThing = %generated.css(`
  .button {
    color: blue;
  }
`)
```

Could be (generically) transformed into:

```rescript
// SomeFile.res
let myThing = SomeFile__css.M1.default
```

The formula for what code to refer to when transforming could be: `<filename>__<generated-extension>.<module>.<generic-value-name>`

1. We're in `SomeFile.res` and using `generated.css`, so the generated module is expected to be called `SomeFile__css`.
2. There's no name to be derived from the CSS string, so we expect the generated module to be called something generic (`M1` in this case, for the first module), referring to which generated `css` embed this is from in the file.
3. Finally, we add a generic `default` a target value name, just to have something to refer to.

> Remember, the actual codegen creating the module we're referring to here from the source `css` text isn't part of this package. This package is just about making it simple to tie together generated things with its source in ReScript.

## Adding more language embeds

Adding more embeds should be straight forward. Reach out if you're interested!

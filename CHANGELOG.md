# Changelog

## Unreleased

- Add statically linked Linux ARM64 PPX builds and select the ARM64 binary during npm installation.

## 0.6.0

- Require ReScript 12.3 and Node.js 20.11 or newer.
- Remove the `@rescript/core` and `@rescript/tools` dependencies.
- Use ReScript 12's standard library and bundled tools CLI.
- Update the sample project and CI for ReScript 12's package-based PPX resolution.
- Replace esy with stock OCaml, opam, and dune builds for the native PPX.
- Build release PPXs with OCaml 5.0, the newest compiler supported by ReScript 12.3's binary AST migration layer.
- Make the installed Windows PPX resolvable through the extensionless package path used by ReScript 12.
- Add deterministic named generation with one stable generated module per embed.
- Make value embeds the primary API, including inline expression positions, while retaining module and include embeds.
- Expose named generated content directly at the stable module root without source-hash wrappers.
- Stop recognizing legacy `// @sourceHash` outputs; remove old generated files once before upgrading.

## 0.5.5

- Fix small mistake in generic generators.

## 0.5.4

- Use `@rescript/tools` to extract embeds in a more robust way.

## 0.5.3

- Fix generated module name casing when used inside file whose name is lowercased for PgTypedSQL

## 0.5.2

- Fix correct built binaries.

## 0.5.1

- Allow unnamed `%sql` queries.

## 0.5.0

- Add `%sql`/`%sql.one`/`%sql.expectOne`/`%sql.many`/`%sql.execute` for [`pgtyped-rescript](https://github.com/zth/pgtyped-rescript).

## 0.4.0

- Add `extract <filePath>` CLI command for easily extracting content + loc info from a file.
- Allow using `%generated` with `include`.
- Bring ReScript version up to `11`.

## 0.3.0

- Propagate location and path info when running generators.

## 0.2.4

- Fix bug in `%edgeql` transform that caused nested let bindings to not work.

## 0.2.2

- Handle whitespace in and around single line comments.

## 0.2.1

- Fix so that extension nodes aren't picked up in single line comments.

## 0.2.0

- Implement `%generated.whatever` generic transform.

## 0.1.1

- Fix `%edgeql` for modules.

## 0.1.0

Initial release!

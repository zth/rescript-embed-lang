# Changelog

## master

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

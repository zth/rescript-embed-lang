// lightningcss bindings
type targets
type drafts
type nonStandard

type pseudoClasses

type visitor

type transformOptions = {
  /** The filename being transformed. Used for error messages and source maps. */
  filename: string,
  /** The source code to transform. */
  code: NodeJs.Buffer.t,
  /** Whether to enable minification. */
  minify?: bool,
  /** Whether to output a source map. */
  sourceMap?: bool,
  /** An input source map to extend. */
  inputSourceMap?: string,
  /**
   * An optional project root path, used as the source root in the output source map.
   * Also used to generate relative paths for sources used in CSS module hashes.
   */
  projectRoot?: string,
  /** The browser targets for the generated code. */
  targets?: targets,
  /** Features that should always be compiled, even when supported by targets. */
  @as("include")
  include_?: float,
  /** Features that should never be compiled, even when unsupported by targets. */
  exclude?: float,
  /** Whether to enable parsing various draft syntax. */
  drafts?: drafts,
  /** Whether to enable various non-standard syntax. */
  nonStandard?: nonStandard,
  /** Whether to compile this file as a CSS module. */
  cssModules?: bool,
  /**
   * Whether to analyze dependencies (e.g. `@import` and `url()`).
   * When enabled, `@import` rules are removed, and `url()` dependencies
   * are replaced with hashed placeholders that can be replaced with the final
   * urls later (after bundling). Dependencies are returned as part of the result.
   */
  analyzeDependencies?: bool,
  /**
   * Replaces user action pseudo classes with class names that can be applied from JavaScript.
   * This is useful for polyfills, for example.
   */
  pseudoClasses?: pseudoClasses,
  /**
   * A list of class names, ids, and custom identifiers (e.g. @keyframes) that are known
   * to be unused. These will be removed during minification. Note that these are not
   * selectors but individual names (without any . or # prefixes).
   */
  unusedSymbols?: array<string>,
  /**
   * Whether to ignore invalid rules and declarations rather than erroring.
   * When enabled, warnings are returned, and the invalid rule or declaration is
   * omitted from the output code.
   */
  errorRecovery?: bool,
  /**
   * An AST visitor object. This allows custom transforms or analysis to be implemented in JavaScript.
   * Multiple visitors can be composed into one using the `composeVisitors` function.
   * For optimal performance, visitors should be as specific as possible about what types of values
   * they care about so that JavaScript has to be called as little as possible.
   */
  visitor?: visitor,
}

type cssModuleExport = {
  /** The local (compiled) name for this export. */
  name: string,
  /** Whether the export is referenced in this file. */
  isReferenced: bool,
  /** Other names that are composed by this export. */
  composes: array<unknown>,
}

type cssModuleExports = Dict.t<cssModuleExport>

type warning = {
  message: string,
  @as("type") type_: string,
  value?: unknown,
  loc: unknown,
}

type transformResult = {
  /** The transformed code. */
  code: Uint8Array.t,
  /** The generated source map, if enabled. */
  map: option<Uint8Array.t>,
  /** CSS module exports, if enabled. */
  exports: option<cssModuleExports>,
  /** Warnings that occurred during compilation. */
  warnings: array<warning>,
}

@module("lightningcss")
external transform: transformOptions => transformResult = "transform"

let embedLang = RescriptEmbedLang.make(
  ~extensionPattern=Generic("css"),
  ~setup=RescriptEmbedLang.defaultSetup,
  ~generate=async ({content, emitExtraFile}) => {
    let {code, exports} = transform({
      filename: "generated.css",
      code: NodeJs.Buffer.fromString(content),
      cssModules: true,
    })

    emitExtraFile(~content=String.make(code), ~extension="css")

    let recordAttributes = switch exports {
    | Some(dict) =>
      dict
      ->Dict.toArray
      ->Array.map(((name, value)) => {
        (name, value.name)
      })
    | None => []
    }

    let recordBody =
      recordAttributes->Array.map(((name, _target)) => `${name}: string,`)->Array.joinWith("\n  ")

    let valueBody =
      recordAttributes
      ->Array.map(((name, target)) => `${name}: "${target}",`)
      ->Array.joinWith("\n  ")

    Ok(
      RescriptEmbedLang.NoModuleName({
        content: `%%raw(\`import "./something.css"\`)

type cssModules = {
  ${recordBody}
}

let default: cssModules = {
  ${valueBody}
}`,
      }),
    )
  },
  ~cliHelpText="<TODO>",
)

await RescriptEmbedLang.runCli(embedLang)

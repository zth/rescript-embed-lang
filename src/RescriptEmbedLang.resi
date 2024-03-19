module CodegenUtils = RescriptEmbedLang__CodegenUtils

/** Represents command line arguments. */
module CliArgs: {
  type t

  /** Checks whether the CLI was invoked with the provided arg.

   ## Example:
  ```rescript
  let watching = args->CliArgs.hasArg("--watch") // true
  ```
   */
  let hasArg: (t, string) => bool

  /** Returns the _value_ of an argument, if any. The value is
   whatever is right of the argument itself, and not starting
   with `--`.

   ## Example:
  ```rescript
  // CLI invoked with: `generate --user testUser`
  args->CliArgs.getArgValue("--user") // Some("testUser")

  // CLI invoked with: `generate --user --password`
  args->CliArgs.getArgValue("--user") // None
  ```*/
  let getArgValue: (t, array<string>) => option<string>
}

/** The pattern the language embed is implemented with.

 Any `FirstClass()` implementation needs additional support in the PPX itself, to be parsed.

 ## Examples:
 - `Generic("css")` means `%generated.css(...)`
 - `FirstClass("edgeql")` means `%edgeql(...)`
*/
type extensionPattern =
  | /**A _generic_ extension. Generic here means that it's in the form of `%generated.identifier(...)`.
  
  ## Examples
  - `Generic("css")` means `%generated.css(...)`*/
  Generic(string)
  | /**A first class extension. First class means that it's not prepended with `generated`. Using this requires dedicated support in the PPX.
  
  ## Examples
  - `FirstClass("edgeql")` means `%edgeql(...)`*/
  FirstClass(string)

/**

 */
@tag("kind")
type generated =
  | WithModuleName({moduleName: string, content: string})
  | NoModuleName({content: string})

/**
The embed language configuration. Use this to run your CLI:

```rescript
RescriptEmbedLang.runCli(embed)
```
 */
type t<'config>

/**Information about an extra emitted file returned from `emitExtraFile`. */
type emitFileReturn = {
  /** The full path to the file. */
  path: string,
  /** The filename of the file. */
  fileName: string,
}

/** A location. */
type loc = {
  line: int,
  col: int,
}

/** A location in a file. */
type fileLocation = {
  start: loc,
  end: loc,
}

/**
 Configuration passed to the `generate` config.
*/
type generateConfig<'config> = {
  /** The location of this content in the source file. */
  location: fileLocation,
  /** The path to the ReScript file the content was found in. */
  path: string,
  /** The config setup for this embed. */
  config: 'config,
  /** The content of the tag we're currently generating from.

   ## Example
   ```rescript
   let css = %generated.css(`this here is the content`)
   ```
   */
  content: string,
  /** Use this if you want to emit _extra files_ for your generated content.
  All extra files will be named according to a predictable naming scheme and automatically managed for you.
  
  Only one file per file extension can be emitted.

  `@param extension: string` - This is the extension the file should have. Example: `css` for a CSS file.

  `@param content: string` - The content to be saved into the file.

  `@param moduleName: option<string>` - The module name derived from the content you're generating from. This must match whatever you return from `generate`.
  
  `@returns` A record with the full path to the file, and the file name.
   */
  emitExtraFile: (
    ~extension: string,
    ~content: string,
    ~moduleName: option<string>,
  ) => emitFileReturn,
}

/**Config fed to the handler for other commands. */
type handleOtherCommandConfig<'config> = {
  /** The command itself. */
  command: string,
  /** The args given to this command. */
  args: CliArgs.t,
  /** The config for this embed. */
  config: 'config,
}

/** Args given to the setup function. */
type setupConfig = {
  /** All arguments given to the CLI. */
  args: CliArgs.t,
}

/** Args given to a custom file watcher when a change occurs */
type watcherOnChangeConfig = {
  /** The specific filepath that triggered the watcher */
  file: string,
  /** Trigger code generation.

   `@param files: array<string>=?` - Optional list of files to do generation for. Passing nothing triggers a full generation.
   */
  runGeneration: (~files: array<string>=?) => promise<unit>,
}

/** A custom file watcher to register when running in watch mode */
type watcher = {
  /** Pattern to register with the watcher */
  filePattern: string,
  /** Callback to trigger when a matching file changes */
  onChange: watcherOnChangeConfig => promise<unit>
}

type setupResult<'config> = {
  /** Config object to provide to future calls */
  config: 'config,
  /** Additional files to keep an eye on in watch mode */
  additionalFileWatchers?: array<watcher>
}

/** A default function for `setup`, that just returns nothing. */
let defaultSetup: setupConfig => promise<setupResult<unit>>

/** Configuration for the `onWatch` handler. */
type onWatchConfig<'config> = {
  /** The config for this embed. */
  config: 'config,
  /** Trigger code generation.

   `@param files: array<string>=?` - Optional list of files to do generation for. Passing nothing triggers a full generation.
   */
  runGeneration: (~files: array<string>=?) => promise<unit>,
  /** A function to log debug statements. Visible via passing `--debug` to the CLI. */
  debug: string => unit,
}

/** Creates an embed, that can then be used to run a CLI.

`@param extensionPattern` - The pattern to use for this embed. Ie `Generic("css")` looks for `%generated.css(...)` and generates `ModuleName__css.res` files.

`@param setup` - A function that sets up your config for this embed. Can be used to for example initiate database connections, or similar.

`@param generate` - The function responsible for generating code from the embedded source.

`@param cliHelpText` - The help text printed by the CLI, either via explicitly calling `--help`, or by calling an unhandled command. You're responsible for putting all the needed details in here yourself.

`@param handleOtherCommand` - If you want to handle other, custom commands in the CLI yourself, this is the place you do it.

`@param onWatch` - Trigger as the watch process is started. Can be used to set up additional watching to retrigger code generation. For example, monitoring changes in a database schema and regenerating types whenever that happens.

`@returns` the embed itself, that can then be used with `runCli` to start a CLI.
 */
let make: (
  ~extensionPattern: extensionPattern,
  ~setup: setupConfig => promise<setupResult<'config>>,
  ~generate: generateConfig<'config> => promise<result<generated, string>>,
  ~cliHelpText: string,
  ~handleOtherCommand: handleOtherCommandConfig<'config> => promise<unit>=?,
  ~onWatch: onWatchConfig<'config> => promise<unit>=?,
) => t<'config>

/** Runs the CLI for the target embed.

`@param embed: t<'config>` - The embed.

`@param args: array<string>=?` - Optionally pass full arguments to the CLI. If this isn't set, the CLI will read from `process.argv` (what you feed the CLI when you invoke it).

## Example
```rescript
await RescriptEmbedLang.runCli(embed)
```
 */
let runCli: (t<'a>, ~args: array<string>=?) => promise<unit>

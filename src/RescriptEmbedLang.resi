module CliArgs: {
  type t
  let hasArg: (t, string) => bool
  let getArgValue: (t, array<string>) => option<string>
}

type extensionPattern = Generic(string) | FirstClass(string)

@tag("kind")
type generated =
  | WithModuleName({moduleName: string, content: string})
  | NoModuleName({content: string})

type t<'config>

type generateConfig<'config> = {
  config: 'config,
  content: string,
  emitExtraFile: (~extension: string, ~content: string) => unit,
}

type handleOtherCommandConfig<'config> = {
  command: string,
  args: array<string>,
  config: 'config,
}

type setupConfig = {args: CliArgs.t}

let defaultSetup: setupConfig => promise<unit>

type onWatchConfig<'config> = {
  config: 'config,
  runGeneration: (~files: array<string>=?) => promise<unit>,
  debug: string => unit,
}

let make: (
  ~extensionPattern: extensionPattern,
  ~setup: setupConfig => promise<'config>,
  ~generate: generateConfig<'config> => promise<result<generated, string>>,
  ~cliHelpText: string,
  ~handleOtherCommand: handleOtherCommandConfig<'config> => promise<unit>=?,
  ~onWatch: onWatchConfig<'config> => promise<unit>=?,
) => t<'config>

let runCli: t<'a> => promise<unit>

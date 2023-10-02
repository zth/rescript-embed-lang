module Path = NodeJs.Path
module Process = NodeJs.Process
module Fs = NodeJs.Fs

module Glob = {
  @live
  type opts = {
    dot?: bool,
    cwd?: string,
    absolute?: bool,
  }

  @live
  type glob = {sync: (array<string>, opts) => array<string>}

  @module("fast-glob")
  external glob: glob = "default"
}

module Hash = {
  type createHash
  @module("crypto") external createHash: createHash = "createHash"
  let hashContents: (createHash, string) => string = %raw(`function(createHash, contents) {
    return createHash("md5").update(contents).digest("hex")
  }`)
  let hashContents = hashContents(createHash, ...)
}

let toFileBaseName = fileName => Path.basenameExt(fileName, ".res")

type extensionPattern = Generic(string) | FirstClass(string)

let writeIfHasChanges = (path, content) => {
  if Fs.existsSync(path) {
    try {
      let currentContent = Fs.readFileSync(path)
      if currentContent->NodeJs.Buffer.toString !== content {
        Fs.writeFileSync(path, NodeJs.Buffer.fromString(content))
      }
    } catch {
    | Exn.Error(_) => ()
    }
  } else {
    Fs.writeFileSync(path, NodeJs.Buffer.fromString(content))
  }
}

// Configure pattern
module FileName = {
  type t = {
    pattern: extensionPattern,
    regExp: RegExp.t,
  }
  let make = (pattern: extensionPattern) => {
    pattern,
    regExp: switch pattern {
    | Generic(extension) =>
      RegExp.fromStringWithFlags("%generated\\." ++ extension ++ "\\(`([^`]+)`\\)", ~flags="g")
    | FirstClass(extension) =>
      RegExp.fromStringWithFlags("%" ++ extension ++ "\\(`([^`]+)`\\)", ~flags="g")
    },
  }
  let getExtensionName = t =>
    switch t {
    | {pattern: Generic(extension) | FirstClass(extension)} => extension
    }
  let getFullExtension = t =>
    switch t {
    | {pattern: Generic(extension)} => `%generated.${extension}`
    | {pattern: FirstClass(extension)} => `%${extension}`
    }
  let toGeneratedFileName = (t, fileName, ~extension="res") => {
    let fileName = toFileBaseName(fileName)
    `${fileName}__${t->getExtensionName}.${extension}`
  }
}

module CliArgs = {
  type t = array<string>
  let hasArg = (args, name) => {
    args->Array.includes(name)
  }

  let getArgValue = (args, names) => {
    let argIndex = args->Array.findIndexOpt(item => names->Array.includes(item))
    switch argIndex {
    | Some(argIndex) =>
      switch args[argIndex + 1] {
      | Some(maybeArgValue) if !(maybeArgValue->String.startsWith("--")) => Some(maybeArgValue)
      | _ => None
      }
    | None => None
    }
  }
}

@tag("kind")
type generated =
  | WithModuleName({moduleName: string, content: string})
  | NoModuleName({content: string})

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

type onWatchConfig<'config> = {
  config: 'config,
  runGeneration: (~files: array<string>=?) => promise<unit>,
  debug: string => unit,
}

type t<'config> = {
  fileName: FileName.t,
  setup: setupConfig => promise<'config>,
  generate: generateConfig<'config> => promise<result<generated, string>>,
  cliHelpText: string,
  handleOtherCommand?: handleOtherCommandConfig<'config> => promise<unit>,
  onWatch?: onWatchConfig<'config> => promise<unit>,
}

let defaultSetup = async _ => ()

let make = (
  ~extensionPattern,
  ~setup,
  ~generate,
  ~cliHelpText,
  ~handleOtherCommand=?,
  ~onWatch=?,
) => {
  fileName: FileName.make(extensionPattern),
  setup,
  generate,
  cliHelpText,
  ?handleOtherCommand,
  ?onWatch,
}

let filePathInGeneratedDir = (t, ~filePath, ~outputDir) =>
  Path.join([outputDir, t->FileName.toGeneratedFileName(filePath->toFileBaseName)])

let getMatches = (t, ~root, ~includeExtension) =>
  Glob.glob.sync(
    [
      switch includeExtension {
      | false => "**/*.res"
      | true => `**/*__${t.fileName->FileName.getExtensionName}.res`
      },
    ],
    {cwd: root, absolute: true},
  )

@val
external argv: array<option<string>> = "process.argv"

@send
external matchAll: (string, RegExp.t) => Iterator.t<array<string>> = "matchAll"

let extractEmbedsFromRescript = (t: t<_>, fileText: string) => {
  let embeds =
    fileText
    ->matchAll(t.fileName.regExp)
    ->Iterator.toArray
    ->Array.map(match => {
      switch match {
      | [_, text] => Some(text)
      | _ => None
      }
    })

  embeds->Array.keepSome
}

module ReadFile = EdgeDbGenerator__Utils.ReadFile

let getFileSourceHash = async filePath => {
  switch await ReadFile.readFirstLine(filePath) {
  | Ok(firstLine) => firstLine->String.split("// @sourceHash ")->Array.get(1)
  | Error() => None
  | exception Exn.Error(_) => None
  }
}

let genereteFileForEmbeds = async (
  t: t<_>,
  path: string,
  ~fileModulesWithContent,
  ~generatedFiles,
  ~config,
  ~outputDir,
  ~debug,
) => {
  try {
    let fileText = Fs.readFileSync(path)->NodeJs.Buffer.toString
    if fileText->String.includes(`${t.fileName->FileName.getFullExtension}(`) {
      fileModulesWithContent->Set.add(path->Path.basenameExt(".res"))
      let embeds = t->extractEmbedsFromRescript(fileText)
      let generatedContent = await Promise.all(
        embeds->Array.mapWithIndex(async (content, index) => {
          let extraFiles = Dict.make()

          switch await t.generate({
            config,
            content,
            emitExtraFile: (~extension, ~content) => {
              debug(`[emit] Emitting extra file with extension .${extension}`)
              extraFiles->Dict.set(extension, content)
            },
          }) {
          | Ok(res) =>
            let (content, moduleName) = switch res {
            | WithModuleName({content, moduleName}) => (content, moduleName)
            | NoModuleName({content}) => (content, `M${Int.toString(index + 1)}`)
            }

            let extraFiles =
              extraFiles
              ->Dict.toArray
              ->Array.map(((extension, content)) => {
                let fileName = toFileBaseName(path)
                let emittedFileName = `${fileName}__${t.fileName->FileName.getExtensionName}.${moduleName}.${extension}`
                debug(`[emit] Emitting file "${emittedFileName}"`)
                (emittedFileName, content)
              })

            Some(moduleName, content, extraFiles)
          | Error(msg) =>
            Console.error(msg)
            None
          | exception Exn.Error(err) =>
            Console.error(err->Exn.message)
            None
          }
        }),
      )

      let content =
        generatedContent
        ->Array.map(c => {
          switch c {
          | None => ""
          | Some((moduleName, contents, _)) =>
            `module ${moduleName} = {\n  ${contents->String.split("\n")->Array.joinWith("\n  ")}\n}`
          }
        })
        ->Array.joinWith("\n\n")

      let extraFiles =
        generatedContent
        ->Array.filterMap(c =>
          switch c {
          | None => None
          | Some((_, _, files)) => Some(files)
          }
        )
        ->Array.flat

      let hash = Hash.hashContents(content)
      let pathInGeneratedDir = t.fileName->filePathInGeneratedDir(~filePath=path, ~outputDir)
      let contents = `// @sourceHash ${hash}\n\n${content}`

      let prettyPath = Path.basename(path)

      let shouldWriteFile = switch await getFileSourceHash(pathInGeneratedDir) {
      | None => true
      | Some(sourceHash) => sourceHash !== hash
      }
      if shouldWriteFile {
        generatedFiles->Array.push((prettyPath, extraFiles->Array.map(((path, _)) => path)))
        Fs.writeFileSync(pathInGeneratedDir, NodeJs.Buffer.fromString(contents))
      }

      extraFiles->Array.forEach(((filePath, content)) => {
        let pathInGeneratedDir = Path.join([outputDir, filePath])
        writeIfHasChanges(pathInGeneratedDir, content)
      })
    }
  } catch {
  | Exn.Error(e) =>
    Console.log(`${CliUtils.colorRed("Error in file")} ${Path.basename(path)}:`)
    Console.error(e)
  }
}

let cleanUpExtraFiles = (t, ~outputDir, ~sourceFileModuleName="*", ~keepThese=[]) => {
  let extraFiles = Glob.glob.sync(
    [`**/${sourceFileModuleName}__${t.fileName->FileName.getExtensionName}.*.*`],
    {cwd: outputDir, absolute: true},
  )
  // TODO: Promisify
  extraFiles->Array.forEach(filePath => {
    if !(keepThese->Array.includes(filePath)) && Fs.existsSync(filePath) {
      Fs.unlinkSync(filePath)
    }
  })
}

let runCli = async t => {
  let debugging = ref(false)
  let debug = msg =>
    if debugging.contents {
      Console.debug(msg)
    }
  let args = argv->Array.sliceToEnd(~start=2)->Array.keepSome
  debugging := args->CliUtils.hasArg("--debug")

  switch args[0] {
  | Some("generate") =>
    let config = await t.setup({args: args})
    let watch = args->CliUtils.hasArg("--watch")
    let pathToGeneratedDir = switch args->CliUtils.getArgValue(["--output"]) {
    | None =>
      panic(`--output must be set. It controls into what directory all generated files are emitted.`)
    | Some(outputDir) =>
      let joined = Path.join([Process.process->Process.cwd, outputDir])
      Path.resolve([joined])
    }
    let outputDir = pathToGeneratedDir
    let src = switch args->CliUtils.getArgValue(["--src"]) {
    | None => panic(`--src must be set. It controls where to look for source ReScript files.`)
    | Some(src) =>
      let joined = Path.join([Process.process->Process.cwd, src])
      Path.resolve([joined])
    }

    try {
      // Try to access the directory
      await Fs.access(pathToGeneratedDir)
    } catch {
    | Exn.Error(_) =>
      Console.log(`Output directory did not exist. Creating now...`)
      await Fs.mkdirWith(pathToGeneratedDir, Fs.mkdirOptions(~recursive=true, ()))
    }

    let runGeneration = async (~files=?) => {
      let (matches, filesInOutputDir) = switch files {
      | None => (
          t->getMatches(~root=src, ~includeExtension=false),
          t->getMatches(~root=outputDir, ~includeExtension=true),
        )
      | Some(files) => (files, [])
      }
      let fileModulesWithContent = Set.make()
      let generatedFiles = []
      if matches->Array.length === 0 {
        Console.log(`No .res files found`)
      } else {
        Console.log(`Generating files...`)

        let _ = await Promise.all(
          matches->Array.map(match =>
            t->genereteFileForEmbeds(
              match,
              ~fileModulesWithContent,
              ~config,
              ~generatedFiles,
              ~outputDir,
              ~debug,
            )
          ),
        )

        if generatedFiles->Array.length === 0 {
          ()
        } else if generatedFiles->Array.length > 5 {
          Console.log(`Generated ${generatedFiles->Array.length->Int.toString} files.`)
        } else {
          Console.log(
            `Generated:\n  ${generatedFiles
              ->Array.map(((name, _)) => name)
              ->Array.joinWith("\n  ")}`,
          )
        }

        if filesInOutputDir->Array.length > 0 {
          let hasLoggedCleaningUnusedFiles = ref(false)
          let logCleanUnusedFiles = () => {
            if !hasLoggedCleaningUnusedFiles.contents {
              Console.log("Cleaning up unused files...")
            }
            Console.time("Cleaning unused files")
            hasLoggedCleaningUnusedFiles := true
          }

          filesInOutputDir->Array.forEach(filePath => {
            let fileModuleName = Path.basenameExt(
              filePath,
              "__" ++ t.fileName->FileName.getExtensionName ++ ".res",
            )
            if !(fileModulesWithContent->Set.has(fileModuleName)) {
              Console.log(`Deleting unused file ${filePath}...`)
              logCleanUnusedFiles()
              Fs.unlinkSync(filePath)
            }
          })
          if hasLoggedCleaningUnusedFiles.contents {
            Console.timeEnd("Cleaning unused files")
          }
        }
      }

      generatedFiles
    }

    let runGeneration = async (~files=?) => {
      Console.time("Generated files in")
      let generatedFiles = await runGeneration(~files?)
      Console.timeEnd("Generated files in")
      // Clean up any existing extra files
      generatedFiles->Array.forEach(((generatedFile, validExtraFiles)) => {
        cleanUpExtraFiles(
          t,
          ~outputDir,
          ~sourceFileModuleName=generatedFile->Path.basenameExt(".res"),
          ~keepThese=validExtraFiles->Array.map(fileName => Path.resolve([outputDir, fileName])),
        )
      })
    }

    if watch {
      open CliBindings

      await runGeneration()
      Console.log(`Watching for changes in ${src}...`)

      let _theWatcher =
        Chokidar.watcher
        ->Chokidar.watch(
          `${src}/**/*.res`,
          ~options={
            ignored: ["**/node_modules", pathToGeneratedDir],
            ignoreInitial: true,
          },
        )
        ->Chokidar.Watcher.onChange(async file => {
          debug(`[changed]: ${file}`)
          await runGeneration(~files=[file])
        })
        ->Chokidar.Watcher.onAdd(async file => {
          debug(`[added]: ${file}`)
          await runGeneration(~files=[file])
        })
        ->Chokidar.Watcher.onUnlink(async file => {
          debug(`[deleted]: ${file}`)
          // Remove if accompanying generated file if it exists
          let asGeneratedFile = t.fileName->FileName.toGeneratedFileName(file)
          let potentialGeneratedFile =
            t.fileName->filePathInGeneratedDir(~outputDir, ~filePath=asGeneratedFile)
          let fileBaseName = toFileBaseName(file)

          if Fs.existsSync(potentialGeneratedFile) {
            Console.log(
              `Deleting generated file "${asGeneratedFile}" that belonged to ${fileBaseName}.res...`,
            )
            Fs.unlinkSync(potentialGeneratedFile)
            cleanUpExtraFiles(t, ~outputDir)
          }
        })

      switch t.onWatch {
      | None => ()
      | Some(onWatch) => await onWatch({config, runGeneration, debug})
      }
    } else {
      await runGeneration()
    }
  | Some(otherCommand) =>
    switch t.handleOtherCommand {
    | None => Console.log(t.cliHelpText)
    | Some(handleOtherCommand) =>
      await handleOtherCommand({args, command: otherCommand, config: await t.setup({args: args})})
    }
  | None => Console.log(t.cliHelpText)
  }
}
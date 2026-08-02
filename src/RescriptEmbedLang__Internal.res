module ReadFile = {
  @module("fs")
  external createReadStream: string => 'stream = "createReadStream"

  @live
  type createInterfaceOptions<'stream> = {
    input: 'stream,
    crlfDelay: float,
  }

  @send external destroy: 'stream => unit = "destroy"

  @send external onLine: ('a, @as("line") _, string => unit) => unit = "on"
  @send external onClose: ('a, @as("close") _, string => unit) => unit = "on"
  @send external onError: ('a, @as("error") _, string => unit) => unit = "on"
  @module("readline")
  external createInterface: createInterfaceOptions<'stream> => 'readlineInterface =
    "createInterface"

  let readFirstLine = (filePath: string): promise<result<string, unit>> => {
    let readStream = createReadStream(filePath)

    let rl = createInterface({
      input: readStream,
      crlfDelay: Float.Constants.positiveInfinity,
    })

    Promise.make((resolve, _reject) => {
      let _ = rl->onLine((line: string) => {
        let _ = rl["close"]()
        readStream->destroy
        resolve(Ok(line))
      })

      rl->onError(_err => {
        resolve(Error())
      })
    })
  }
}

@live
type loc = {
  /** 0 based */
  line: int,
  /** 0 based */
  col: int,
}

@live
type extractedContent = {
  extensionName: string,
  contents: string,
  loc: {"start": {"line": int, "character": int}, "end": {"line": int, "character": int}},
}

external jsonToExtractedContent: JSON.t => array<extractedContent> = "%identity"

module NodeModule = {
  type require

  @module("node:module")
  external createRequire: string => require = "createRequire"

  @send
  external resolve: (require, string) => string = "resolve"
}

let rescriptToolsCliPath = {
  let require = NodeModule.createRequire(
    NodeJs.Path.join([NodeJs.Process.process->NodeJs.Process.cwd, "package.json"]),
  )
  let rescriptPackageDir = require->NodeModule.resolve("rescript/package.json")->NodeJs.Path.dirname

  NodeJs.Path.join([rescriptPackageDir, "cli", "rescript-tools.js"])
}

let findContentInFile = async (filePath, tags) => {
  switch NodeJs.ChildProcess.execFileSync(
    NodeJs.Process.process->NodeJs.Process.execPath,
    [
      rescriptToolsCliPath,
      "extract-embedded",
      tags->Array.map(t => t->String.slice(~start=1))->Array.join(","),
      filePath,
    ],
  )
  ->NodeJs.Buffer.toString
  ->JSON.parseOrThrow
  ->jsonToExtractedContent {
  | exception JsExn(e) =>
    Console.error(e)
    panic("Failed")
  | extractedContent => extractedContent
  }
}

let extractContentInFile = async (filePath, tags) => {
  await findContentInFile(filePath, tags)
}

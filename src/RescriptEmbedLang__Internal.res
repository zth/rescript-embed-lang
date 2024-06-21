module ReadFile = {
  @module("fs")
  external createReadStream: string => 'stream = "createReadStream"

  type createInterfaceOptions<'stream> = {
    input: 'stream,
    crlfDelay: int,
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
      crlfDelay: %raw("Infinity"),
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

  let readLines = (filePath, callback) => {
    let readStream = createReadStream(filePath)

    let rl = createInterface({
      input: readStream,
      crlfDelay: %raw("Infinity"),
    })

    Promise.make((resolve, _reject) => {
      let _ = rl->onLine((line: string) => {
        callback(line)
      })

      rl->onClose(_ => {
        resolve(Ok())
      })

      rl->onError(_err => {
        resolve(Error())
      })
    })
  }
}

let commentOpen = "/*"
let docCommentOpen = "/**"
let commentClose = "*/"
let ending = "`)" // TODO: Regexp

type loc = {
  /** 0 based */
  line: int,
  /** 0 based */
  col: int,
}

type pushingContent = {
  tag: string,
  content: array<string>,
  start: loc,
  end: loc,
}

type extractedContent = {
  extensionName: string,
  contents: string,
  loc: {"start": {"line": int, "character": int}, "end": {"line": int, "character": int}},
}

external jsonToExtractedContent: JSON.t => array<extractedContent> = "%identity"

let findContentInFile = async (filePath, tags) => {
  switch NodeJs.ChildProcess.execFileSync(
    RescriptTools.getBinaryPath(),
    [
      "extract-embedded",
      tags->Array.map(t => t->String.sliceToEnd(~start=1))->Array.joinWith(","),
      filePath,
    ],
  )
  ->NodeJs.Buffer.toString
  ->JSON.parseExn
  ->jsonToExtractedContent {
  | exception Exn.Error(e) =>
    Console.error(e)
    panic("Failed")
  | extractedContent => extractedContent
  }
}

let extractContentInFile = async (filePath, tags) => {
  await findContentInFile(filePath, tags)
}

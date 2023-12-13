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

let findContentInFile = async (filePath, tags) => {
  let tags = tags->Array.map(t => `${t}(\``)
  let foundContent = []
  let lineCount = ref(0)
  let inComment = ref(false)
  let pushingContent = ref(None)

  switch await filePath->ReadFile.readLines(line => {
    let currentLine = lineCount.contents
    lineCount := lineCount.contents + 1

    let break = ref(false)

    let trimmedLine = line->String.trim
    let isSingleLineComment = trimmedLine->String.startsWith("//")

    if inComment.contents && line->String.includes(commentClose) {
      // Detect comment end
      inComment := false
      break := true
    } else if inComment.contents {
      // Break if in a comment
      break := true
    } else if (
      !inComment.contents &&
      (trimmedLine->String.startsWith(docCommentOpen) ||
        trimmedLine->String.startsWith(commentOpen))
    ) {
      // Break if comment starting
      // TODO: Nested comments?
      inComment := true
      break := true
    }

    if isSingleLineComment {
      break := true
    }

    if !break.contents {
      switch pushingContent.contents {
      | Some(c) =>
        if line->String.includes(ending) {
          switch line->String.split(ending) {
          | [before, _after] =>
            let lastLine = line->String.split(ending)->Array.getUnsafe(0)
            c.content->Array.push(lastLine)
            foundContent->Array.push({
              ...c,
              end: {
                line: currentLine,
                col: before->String.length - 1 /* Make 0 based */,
              },
            })
          | _ => ()
          }
          pushingContent := None
        } else {
          c.content->Array.push(line)
        }
      | None =>
        let tagOnLine = tags->Array.find(tag => line->String.includes(tag))
        switch tagOnLine {
        | None => ()
        | Some(tagOnLine) =>
          switch line->String.split(tagOnLine) {
          | [before, after] =>
            let startPos = {
              line: currentLine,
              col: before->String.length + tagOnLine->String.length - 1 /* Make 0 based */,
            }
            pushingContent :=
              Some({
                tag: tagOnLine->String.replace("(`", ""),
                content: [after],
                start: startPos,
                end: startPos,
              })
          | _ => ()
          }
        }
      }
    }
  }) {
  | Ok()
  | Error() => foundContent
  | exception Exn.Error(_) => foundContent
  }
}

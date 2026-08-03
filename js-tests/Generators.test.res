open TestFramework
open RescriptEmbedLang__Internal

module Fs = NodeJs.Fs
module Path = NodeJs.Path

describe("findContentInFile", () => {
  let testFile = Path.resolve([NodeJs.Process.process->NodeJs.Process.cwd, "js-tests/TestFile.txt"])
  let testFile2 = Path.resolve([
    NodeJs.Process.process->NodeJs.Process.cwd,
    "js-tests/TestFile2.txt",
  ])
  let testFile3 = Path.resolve([
    NodeJs.Process.process->NodeJs.Process.cwd,
    "js-tests/TestFile3.txt",
  ])

  testAsync("finds content", async () => {
    let foundContent = await testFile->findContentInFile(["%edgeql", "%css"])
    expect(foundContent)->Expect.toMatchSnapshot
  })

  testAsync("finds content 2", async () => {
    let foundContent = await testFile2->findContentInFile(["%edgeql"])
    expect(foundContent)->Expect.toMatchSnapshot
  })

  testAsync("finds content 3", async () => {
    let foundContent = await testFile3->findContentInFile(["%edgeql"])
    expect(foundContent)->Expect.toMatchSnapshot
  })
})

describe("extractContentInFile", () => {
  let testFile = Path.resolve([NodeJs.Process.process->NodeJs.Process.cwd, "js-tests/TestFile.txt"])

  testAsync("extracts content", async () => {
    let foundContent = await testFile->extractContentInFile(["%edgeql", "%css"])
    expect(foundContent)->Expect.toMatchSnapshot
  })
})

module NamedGenerationFixture = {
  @live
  type fixtureCase = {
    label: string,
    strategy: string,
    pattern: string,
    flags: string,
    captureKind: string,
    captureValue: string,
    cardinality: string,
    source: string,
    resultKind: string,
    resultValue: string,
  }

  @module("node:assert/strict")
  external equal: ('a, 'a, ~message: string=?) => unit = "equal"

  @module("node:assert/strict")
  external ok: (bool, ~message: string=?) => unit = "ok"

  let fixturePath = Path.resolve([
    NodeJs.Process.process->NodeJs.Process.cwd,
    "fixtures/named-generation.tsv",
  ])

  let fixture: array<fixtureCase> =
    fixturePath
    ->Fs.readFileSync
    ->NodeJs.Buffer.toString
    ->String.split("\n")
    ->Array.filterMap(line => {
      let line = line->String.trim
      if line === "" || line->String.startsWith("#") {
        None
      } else {
        let fields = line->String.split("\t")
        if fields->Array.length !== 10 {
          panic(`invalid named-generation fixture row: ${line}`)
        }
        let getField = index =>
          fields[index]
          ->Option.getOrThrow(~message=`missing field ${index->Int.toString}`)
          ->value => JSON.parseOrThrow(`"${value}"`)
          ->JSON.Decode.string
          ->Option.getOrThrow(~message=`invalid escaped field ${index->Int.toString}`)
        Some({
          label: getField(0),
          strategy: getField(1),
          pattern: getField(2),
          flags: getField(3),
          captureKind: getField(4),
          captureValue: getField(5),
          cardinality: getField(6),
          source: getField(7),
          resultKind: getField(8),
          resultValue: getField(9),
        })
      }
    })
}

describe("named generation shared corpus", () => {
  open NamedGenerationFixture

  fixture->Array.forEach(case => {
    test(
      case.label,
      () => {
        let config = switch case.strategy {
        | "graphqlDefinition" => RescriptEmbedLang.GraphqlDefinition
        | "nameDirective" => RescriptEmbedLang.NameDirective
        | "regex" =>
          let capture = switch case.captureKind {
          | "numbered" =>
            RescriptEmbedLang.Numbered(case.captureValue->Int.fromString->Option.getOrThrow)
          | "named" => RescriptEmbedLang.Named(case.captureValue)
          | kind => panic(`unknown capture kind ${kind}`)
          }
          let cardinality = switch case.cardinality {
          | "exactlyOne" => RescriptEmbedLang.ExactlyOne
          | "first" => RescriptEmbedLang.First
          | value => panic(`unknown cardinality ${value}`)
          }
          RescriptEmbedLang.Regex({
            pattern: case.pattern,
            flags: case.flags,
            capture,
            cardinality,
          })
        | strategy => panic(`unknown naming strategy ${strategy}`)
        }
        let result = try {
          Ok(
            RescriptEmbedLang.GeneratedName.extract(
              ~extension="fixture",
              ~source=case.source,
              config,
            ),
          )
        } catch {
        | JsExn(error) => Error(error->JsExn.message->Option.getOr("unknown error"))
        }

        switch (case.resultKind, result) {
        | ("name", Ok(actual)) => equal(actual, Some(case.resultValue), ~message=case.label)
        | ("error", Error(message)) =>
          ok(message->String.includes(case.resultValue), ~message=`${case.label}: ${message}`)
        | ("error", Ok(_)) =>
          panic(`${case.label}: expected error containing "${case.resultValue}"`)
        | ("name", Error(message)) =>
          panic(`${case.label}: expected "${case.resultValue}", got error: ${message}`)
        | (resultKind, _) => panic(`${case.label}: unknown result kind ${resultKind}`)
        }
      },
    )
  })
})

module NamedGeneratorIntegration = {
  @live type rmOptions = {recursive: bool, force: bool}
  @module("node:fs") external mkdtempSync: string => string = "mkdtempSync"
  @module("node:fs") external rmSync: (string, rmOptions) => unit = "rmSync"
  let write = (path, content) => Fs.writeFileSync(path, NodeJs.Buffer.fromString(content))
  let embed = RescriptEmbedLang.make(
    ~extensionPattern=Generic("fixture"),
    ~generatedName=GraphqlDefinition,
    ~setup=RescriptEmbedLang.defaultSetup,
    ~generate=async ({content, emitExtraFile}) =>
      if content->String.includes("FAIL_GENERATION") {
        Error("intentional generator failure")
      } else {
        ignore(emitExtraFile(~extension="txt", ~content="owned artifact\n", ~moduleName=None))
        Ok(NoModuleName({content: "let default = 42"}))
      },
    ~cliHelpText="fixture generator",
  )
}

describe("named generator integration", () => {
  open NamedGenerationFixture
  open NamedGeneratorIntegration

  testAsync("commits named files transactionally and removes stale owned output", async () => {
    let root = mkdtempSync(Path.join([NodeJs.Os.tmpdir(), "rescript-embed-lang-"]))
    let src = Path.join([root, "src"])
    let output = Path.join([root, "generated"])
    let ppxConfigPath = Path.join([output, "rescript-embed-lang.json"])
    let ownedFilesIndexPath = Path.join([output, ".rescript-embed-lang-fixture.json"])
    Fs.mkdirSync(src)
    Fs.mkdirSync(output)
    let source = Path.join([src, "Operations.res"])
    let run = () =>
      RescriptEmbedLang.runCli(
        embed,
        ~args=["generate", "--src", src, "--output", output],
      )

    try {
      write(source, "module Alpha = %generated.fixture(\x60query Alpha { viewer { id } }\x60)\n")
      await run()
      let alpha = Path.join([output, "Operations__fixture__Alpha.res"])
      ok(Fs.existsSync(alpha), ~message="named output was not generated")
      let alphaArtifact = Path.join([output, "Operations__fixture__Alpha.txt"])
      ok(Fs.existsSync(alphaArtifact), ~message="extra artifact was not generated")
      ok(Fs.existsSync(ownedFilesIndexPath), ~message="owned-files index was not generated")
      let alphaContent = alpha->Fs.readFileSync->NodeJs.Buffer.toString
      equal(
        alphaContent,
        "// @generated by rescript-embed-lang v1\nlet default = 42\n",
        ~message="named generated content should be exposed at the stable module root",
      )
      ok(Fs.existsSync(ppxConfigPath), ~message="generated PPX config was not written")
      ok(
        ppxConfigPath
        ->Fs.readFileSync
        ->NodeJs.Buffer.toString
        ->String.includes("graphqlDefinition"),
        ~message="generated PPX config did not contain the naming strategy",
      )

      write(source, "module Beta = %generated.fixture(\x60query Beta { viewer { id } }\x60)\n")
      await run()
      let beta = Path.join([output, "Operations__fixture__Beta.res"])
      let betaArtifact = Path.join([output, "Operations__fixture__Beta.txt"])
      ok(Fs.existsSync(beta), ~message="renamed output was not generated")
      ok(Fs.existsSync(betaArtifact), ~message="renamed extra artifact was not generated")
      ok(!Fs.existsSync(alpha), ~message="stale owned output was not removed")
      ok(!Fs.existsSync(alphaArtifact), ~message="stale extra artifact was not removed")

      write(
        source,
        "module Beta = %generated.fixture(\x60query Beta { viewer { id } }\x60)\nmodule Broken = %generated.fixture(\x60query FAIL_GENERATION { viewer { id } }\x60)\n",
      )
      let failed = ref(false)
      try {
        await run()
      } catch {
      | JsExn(_) => failed := true
      }
      ok(failed.contents, ~message="generator failure should reject")
      ok(Fs.existsSync(beta), ~message="failed run replaced the last successful output")

      write(
        source,
        "module Upper = %generated.fixture(\x60query Duplicate { viewer { id } }\x60)\nmodule Lower = %generated.fixture(\x60query duplicate { viewer { id } }\x60)\n",
      )
      let collided = ref(false)
      try {
        await run()
      } catch {
      | JsExn(_) => collided := true
      }
      ok(collided.contents, ~message="case-insensitive collision should reject")
      ok(Fs.existsSync(beta), ~message="collision removed the last successful output")

      let userOwned = Path.join([output, "Operations__fixture__userowned.res"])
      write(userOwned, "let userFile = true\n")
      write(
        source,
        "module UserOwned = %generated.fixture(\x60query UserOwned { viewer { id } }\x60)\n",
      )
      let userCollision = ref(false)
      try {
        await run()
      } catch {
      | JsExn(_) => userCollision := true
      }
      ok(userCollision.contents, ~message="case-only user-owned collision should reject")
      equal(
        userOwned->Fs.readFileSync->NodeJs.Buffer.toString,
        "let userFile = true\n",
        ~message="user-owned collision file was modified",
      )

      let userModule = Path.join([src, "Operations__fixture__SourceCollision.res"])
      write(userModule, "let userModule = true\n")
      write(
        source,
        "module SourceCollision = %generated.fixture(\x60query SourceCollision { viewer { id } }\x60)\n",
      )
      let sourceCollision = ref(false)
      try {
        await run()
      } catch {
      | JsExn(_) => sourceCollision := true
      }
      ok(sourceCollision.contents, ~message="user source module collision should reject")
      equal(
        userModule->Fs.readFileSync->NodeJs.Buffer.toString,
        "let userModule = true\n",
        ~message="user source module was modified",
      )

      write(source, "let noEmbeds = true\n")
      await run()
      ok(!Fs.existsSync(beta), ~message="last owned output was not removed")
      ok(!Fs.existsSync(betaArtifact), ~message="last owned artifact was not removed")
      ok(!Fs.existsSync(ownedFilesIndexPath), ~message="empty owned-files index was not removed")
      ok(
        Fs.existsSync(ppxConfigPath),
        ~message="PPX config should remain when an extension currently has no embeds",
      )
      equal(
        userOwned->Fs.readFileSync->NodeJs.Buffer.toString,
        "let userFile = true\n",
        ~message="user-owned file was removed during empty generation",
      )
    } catch {
    | JsExn(error) =>
      rmSync(root, {recursive: true, force: true})
      panic(error->JsExn.message->Option.getOr("integration test failed"))
    }
    rmSync(root, {recursive: true, force: true})
  })
})

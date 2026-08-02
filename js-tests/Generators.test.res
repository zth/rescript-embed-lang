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
    pattern: string,
    flags: string,
    captureKind: string,
    captureValue: string,
    cardinality: string,
    source: string,
    expectedName?: string,
    errorContains?: string,
  }

  @live
  type fixture = {version: int, cases: array<fixtureCase>}

  external decode: JSON.t => fixture = "%identity"

  @module("node:assert/strict")
  external equal: ('a, 'a, ~message: string=?) => unit = "equal"

  @module("node:assert/strict")
  external ok: (bool, ~message: string=?) => unit = "ok"

  let fixturePath = Path.resolve([
    NodeJs.Process.process->NodeJs.Process.cwd,
    "fixtures/named-generation.json",
  ])

  let fixture =
    fixturePath
    ->Fs.readFileSync
    ->NodeJs.Buffer.toString
    ->JSON.parseOrThrow
    ->decode
}

describe("named generation shared corpus", () => {
  open NamedGenerationFixture

  fixture.cases->Array.forEach(case => {
    test(
      case.label,
      () => {
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
        let config = RescriptEmbedLang.Regex({
          pattern: case.pattern,
          flags: case.flags,
          capture,
          cardinality,
        })
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

        switch (case.expectedName, case.errorContains, result) {
        | (Some(expected), _, Ok(actual)) => equal(actual, Some(expected), ~message=case.label)
        | (_, Some(expectedError), Error(message)) =>
          ok(message->String.includes(expectedError), ~message=`${case.label}: ${message}`)
        | (_, Some(expectedError), Ok(_)) =>
          panic(`${case.label}: expected error containing "${expectedError}"`)
        | (Some(expected), _, Error(message)) =>
          panic(`${case.label}: expected "${expected}", got error: ${message}`)
        | _ => panic(`${case.label}: fixture must define expectedName or errorContains`)
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
  let pattern = "^[ \\t]*(?:query|mutation|subscription)[ \\t\\r\\n]+([_A-Za-z][_0-9A-Za-z]*)"
  let embed = RescriptEmbedLang.make(
    ~extensionPattern=Generic("fixture"),
    ~generatedName=Regex({
      pattern,
      flags: "m",
      capture: Numbered(1),
      cardinality: ExactlyOne,
    }),
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
    let config = Path.join([root, "embed-config.json"])
    Fs.mkdirSync(src)
    Fs.mkdirSync(output)
    let source = Path.join([src, "Operations.res"])
    let run = () =>
      RescriptEmbedLang.runCli(
        embed,
        ~args=["generate", "--src", src, "--output", output, "--embed-lang-config", config],
      )

    try {
      write(source, "module Alpha = %generated.fixture(\x60query Alpha { viewer { id } }\x60)\n")
      await run()
      let alpha = Path.join([output, "Operations__fixture__Alpha.res"])
      ok(Fs.existsSync(alpha), ~message="named output was not generated")
      let alphaArtifact = Path.join([output, "Operations__fixture__Alpha.txt"])
      ok(Fs.existsSync(alphaArtifact), ~message="extra artifact was not generated")
      let alphaContent = alpha->Fs.readFileSync->NodeJs.Buffer.toString
      equal(
        alphaContent,
        "// @generated by rescript-embed-lang v1\nlet default = 42\n",
        ~message="named generated content should be exposed at the stable module root",
      )
      ok(Fs.existsSync(config), ~message="PPX config was not written")
      let configContent = config->Fs.readFileSync->NodeJs.Buffer.toString
      ok(
        configContent->String.includes("\"kind\": \"regex\""),
        ~message="named config was not serialized",
      )

      write(source, "module Beta = %generated.fixture(\x60query Beta { viewer { id } }\x60)\n")
      await run()
      let beta = Path.join([output, "Operations__fixture__Beta.res"])
      ok(Fs.existsSync(beta), ~message="renamed output was not generated")
      ok(!Fs.existsSync(alpha), ~message="stale owned output was not removed")
      ok(!Fs.existsSync(alphaArtifact), ~message="stale extra artifact was not removed")

      let validConfig = config->Fs.readFileSync->NodeJs.Buffer.toString
      write(config, "{invalid json\n")
      write(source, "module Gamma = %generated.fixture(\x60query Gamma { viewer { id } }\x60)\n")
      let configFailed = ref(false)
      try {
        await run()
      } catch {
      | JsExn(_) => configFailed := true
      }
      ok(configFailed.contents, ~message="invalid config should reject")
      ok(Fs.existsSync(beta), ~message="config failure replaced the last successful output")
      write(config, validConfig)

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
    } catch {
    | JsExn(error) =>
      rmSync(root, {recursive: true, force: true})
      panic(error->JsExn.message->Option.getOr("integration test failed"))
    }
    rmSync(root, {recursive: true, force: true})
  })
})

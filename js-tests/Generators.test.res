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

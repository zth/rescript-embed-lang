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

  testAsync("finds content", async () => {
    let foundContent = await testFile->findContentInFile(["%edgeql", "%css"])
    expect(foundContent)->Expect.toMatchSnapshot
  })

  testAsync("finds content 2", async () => {
    let foundContent = await testFile2->findContentInFile(["%edgeql"])
    Console.log(foundContent)
    expect(foundContent)->Expect.toMatchSnapshot
  })
})

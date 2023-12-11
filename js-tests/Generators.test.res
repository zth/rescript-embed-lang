open TestFramework
open RescriptEmbedLang__Internal

module Fs = NodeJs.Fs
module Path = NodeJs.Path

describe("findContentInFile", () => {
  let testFile = Path.resolve([NodeJs.Process.process->NodeJs.Process.cwd, "js-tests/TestFile.txt"])

  testAsync("finds content", async () => {
    let foundContent = await testFile->findContentInFile(["%edgeql", "%css"])
    expect(foundContent)->Expect.toMatchSnapshot
  })
})

const assert = require("node:assert/strict");
const fs = require("node:fs");
const os = require("node:os");
const path = require("node:path");
const test = require("node:test");

const {
  copyPlatformBinaries,
  getBinaryPlatform,
  ppxArch,
  removeInitialBinaries,
} = require("./postinstall.js");

test("selects the Linux ARM64 binary", () => {
  assert.equal(ppxArch("linux", "arm64"), "arm64");
  assert.equal(getBinaryPlatform("linux", "arm64"), "linux-arm64");
});

test("selects the existing binaries for supported platforms", () => {
  assert.equal(getBinaryPlatform("linux", "x64"), "linux");
  assert.equal(getBinaryPlatform("darwin", "arm64"), "macos-arm64");
  assert.equal(getBinaryPlatform("darwin", "x64"), "macos-latest");
  assert.equal(getBinaryPlatform("win32", "x64"), "windows-latest");
});

test("installs the selected binary and removes packaged platform binaries", t => {
  const directory = fs.mkdtempSync(path.join(os.tmpdir(), "rescript-embed-lang-"));
  t.after(() => fs.rmSync(directory, {recursive: true, force: true}));

  const binaries = [
    "ppx-linux",
    "ppx-linux-arm64",
    "ppx-macos-arm64",
    "ppx-macos-latest",
    "ppx-windows-latest",
  ];
  for (const binary of binaries) {
    fs.writeFileSync(path.join(directory, binary), binary);
  }

  copyPlatformBinaries("linux-arm64", directory);
  removeInitialBinaries(directory);

  assert.equal(fs.readFileSync(path.join(directory, "ppx"), "utf8"), "ppx-linux-arm64");
  for (const binary of binaries) {
    assert.equal(fs.existsSync(path.join(directory, binary)), false);
  }
});

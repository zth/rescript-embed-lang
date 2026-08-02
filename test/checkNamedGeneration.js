const assert = require("node:assert/strict");
const fs = require("node:fs");
const path = require("node:path");
const {spawnSync} = require("node:child_process");

const rescriptPackageDir = path.dirname(
  require.resolve("rescript/package.json")
);
const rescriptCli = path.join(rescriptPackageDir, "cli", "rescript.js");
const result = spawnSync(process.execPath, [rescriptCli], {
  cwd: __dirname,
  encoding: "utf8",
});

assert.equal(result.status, 0, result.stdout + result.stderr);

const compiled = fs.readFileSync(
  path.join(__dirname, "src", "NamedTransformTest.mjs"),
  "utf8"
);
assert.match(
  compiled,
  /NamedTransformTest__fixture__Inline/,
  "inline value embed should reference its stable generated module"
);
assert.doesNotMatch(
  compiled,
  /SourceHash_/,
  "generated APIs must not contain source-hash modules"
);

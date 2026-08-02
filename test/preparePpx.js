const fs = require("fs");
const path = require("path");

const source = path.resolve(
  __dirname,
  "../_build/default/bin/RescriptEmbedLang.exe"
);
const packageDir = path.resolve(
  __dirname,
  "node_modules/rescript-embed-lang"
);
const target = path.join(
  packageDir,
  process.platform === "win32" ? "ppx.exe" : "ppx"
);

fs.mkdirSync(packageDir, { recursive: true });
fs.copyFileSync(source, target);
fs.chmodSync(target, 0o755);

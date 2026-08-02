#!/usr/bin/env node
const tagName = process.argv[2];
const fs = require("fs");
const path = require("path");

const sourcePkgJson = JSON.parse(
  fs.readFileSync(path.resolve(__dirname, "package.json"), "utf-8")
);
const {
  name,
  version,
  description,
  main,
  engines,
  dependencies,
  peerDependencies,
  repository,
  keywords,
  author,
  license,
  bugs,
  homepage,
} = sourcePkgJson;
const pkgJson = {
  name,
  version,
  description,
  main,
  engines,
  dependencies,
  peerDependencies,
  scripts: {
    postinstall: "node postinstall.js",
  },
  repository,
  keywords,
  author,
  license,
  bugs,
  homepage,
};

// Bypass forcing package name and version for the beta track.
if (tagName && tagName !== "beta") {
  const commit = require("child_process")
    .execSync("git rev-parse HEAD")
    .toString()
    .trim()
    .slice(0, 8);

  pkgJson.version = `0.0.0-${tagName}-${commit}`;
}

fs.writeFileSync(
  path.resolve(path.join("release", "package.json")),
  JSON.stringify(pkgJson, null, 2)
);

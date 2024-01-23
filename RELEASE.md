1. Bump the version in the changelog, `package.json` + `package-release.json`, and run `npm i` so the version is propagated to the lockfile.
2. On an arm64 Mac, run `./build-release.sh`. This will create the release folder, build the arm64 version of the PPX and copy it there, and copy all other required files for release.
3. Download latest Mac/Windows/Linux PPXes from GitHub actions: https://github.com/zth/rescript-embed-lang/actions, place them in the `release` folder. Make sure they're named correctly (`ppx-macos-latest`, `ppx-windows-latest`, `ppx-linux`).
4. Run `npm publish` in the `release` folder.

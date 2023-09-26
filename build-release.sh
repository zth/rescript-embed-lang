mkdir -p release;
echo "Building PPX locally...";
esy;
cp _build/default/bin/RescriptEmbedLang.exe release/ppx-macos-arm64;
echo "Copying assets...";
cp -f README.md postinstall.js release/;
cp -f package-release.json release/package.json;
echo "Done!";
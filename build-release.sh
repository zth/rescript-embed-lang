mkdir -p release;
echo "Copying assets...";
cp -rf src release/;
cp -f README.md postinstall.js bsconfig.json release/;
cp -f package-release.json release/package.json;
./copyPackageJson.js release $INPUT_TAG_NAME
echo "Done!";
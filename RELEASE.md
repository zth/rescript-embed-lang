1. Bump the version in the changelog and `package.json`, then run `npm i` so the version is propagated to the lockfile and release metadata.
2. Run the `Build Release` workflow from GitHub Actions, optionally supplying an npm tag.
3. Verify the platform PPX jobs, generated `release-build` artifact, and npm publication result.

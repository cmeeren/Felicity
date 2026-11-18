# AGENTS.md

- With each user-facing change, add release notes under `### Unreleased` at the top of `RELEASE_NOTES.md`, creating it if needed, and update `DOCUMENTATION.md` when public behavior or API changes.

## Releasing

CI publishes to NuGet only for tags under `v/`. To release version `X`:

1. Bump `<Version>` in `src/Felicity/Felicity.fsproj` and rename `### Unreleased` to `### X (YYYY-MM-DD)`.
2. Make `Release X.` the last paragraph of the commit message before any trailers. A release with a single change goes in that change's commit.
3. After pushing the commit to `main`, push an annotated tag `v/X` with the message `Release X` on that commit.
4. Watch the tag's CI run (`gh run list --repo cmeeren/Felicity --branch v/X`) until it succeeds and its Push step log shows that `Felicity.X.nupkg` was pushed; the step uses `--skip-duplicate`, so it also succeeds without publishing when NuGet already has that version. NuGet never reuses a version, so once the Push step has started, release a new version instead of moving the tag.

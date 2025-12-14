# Repository Guidelines

## Project Structure & Module Organization
- `src/HCsv.hs` is the public entry point re-exporting core types and parsing helpers.
- `src/HCsv/Adt.hs` defines CSV-related aliases; `src/HCsv/Parser.hs` holds Attoparsec-based parsing; `src/HCsv/Util.hs` contains shared utilities.
- Tests live in `test/` with the Hspec runner in `test/Spec.hs` and specs in `test/HCsvSpec.hs`.
- `hcsv.cabal` defines the library and `hcsv-test` suite; metadata sits in `CHANGELOG.md`, `LICENSE`, and `README.md`.

## Build, Test, and Development Commands
- `cabal update && cabal build` — fetch deps and compile with `-Wall` under GHC2024.
- `cabal repl hcsv` — start a REPL focused on the library modules.
- `cabal test hcsv-test` — run the Hspec suite (uses temp CSV files for fixtures).
- `cabal haddock --builddir=dist-newstyle` — generate API docs; open `dist-newstyle/**/doc/html/hcsv/index.html`.

## Coding Style & Naming Conventions
- Use two-space indentation, explicit export lists, and place `LANGUAGE` pragmas at the top.
- Favor pure helpers and total parsing; return `Either String` for recoverable parse errors.
- Prefer `qualified` imports for collections and bytestring/text modules; keep shared helpers in `HCsv.Util`.
- Module names follow `HCsv.*`; mirror module names in test specs (`HCsvSpec.hs` pattern).

## Testing Guidelines
- Framework: Hspec (`cabal test hcsv-test`). Add new specs in `test/` and expose them via `test/Spec.hs`.
- Structure: organize with `describe`/`it`; assert vectors via `shouldBe` (see existing specs).
- Coverage focus: CSV edge cases (missing headers, escaped fields, blank lines, type coercions).
- For I/O paths, follow the `withSystemTempFile` pattern used in current tests.

## Commit & Pull Request Guidelines
- Commits: short, imperative summaries; current history is concise and emoji-friendly if useful.
- Before PR: run `cabal build` and `cabal test`; mention any failing cases if unresolved.
- PR description should outline scope, risks, test results, and link related issues; add sample CSVs or screenshots when behavior changes.
- Call out new dependencies or cabal flag changes in `hcsv.cabal` with rationale.

# daml-cucumber

Behavior-driven-development for Daml script.

## Development

From the project root, run `nix-shell` to get a shell with the `daml` command, `daml sdk`, `ghci`, `cabal`, and necessary haskell packages installed.

Run the canton sandbox:

```bash
nix-shell
cd test
daml sandbox
```

Run the cucumber tests:

```bash
nix-shell
cd hs
cabal repl exe:daml-cucumber
:main --dar "../test/.daml/dist/daml-cucumber-0.1.0.0.dar" --host "localhost" --port 6865 --script "Test:main" --feat "../test/features.feature"
```

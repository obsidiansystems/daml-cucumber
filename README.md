# daml-cucumber 🥒

[Behavior-driven-development](https://cucumber.io/docs/bdd/) for Daml script using Cucumber's [Gherkin](https://cucumber.io/docs/gherkin/reference/) specification language.

## Getting Started

This repository includes both a Daml library and an executable that reads gherkin .feature files and invokes your Daml test script with the feature as input.

### Daml library

Your Daml test suite should import [`Cucumber.daml`](./daml/Cucumber.daml). Your test suite should include a function with the type signature `Feature -> Script [[Message]]`. This can be implemented in terms of the provided `runTests` function. See [`Test:main`](./test/daml/Test.daml) for an example.

### daml-cucumber executable

Launch daml-cucumber to run tests against a running ledger like so:

```bash
daml-cucumber \
  --dar "path/to/your-test-0.1.0.dar" \
  --host "localhost" \
  --port 6865 \
  --feat "path/to/gherkin.feature" \
  --script "Test:main"
```

daml-cucumber will run all of the scenarios in the specified feature file and produce a report in your terminal that looks like the following:

```
Feature: Example
  Scenario: a contract can be created
    Given a party => Passed
    When the party creates contract X => Passed
    Then Contract X is created => Failed: Not implemented
```

## Building daml-cucumber

To build the daml-cucumber executable, run:

```bash
cd hs
nix-build
```

## Working on daml-cucumber

From the project root, run `nix-shell` to get a shell with the `daml` command, `daml sdk`, `ghci`, `cabal`, and necessary haskell packages installed.

Run the canton sandbox:

```bash
nix-shell
cd test
daml build
daml sandbox
```

Build the scripts:

```bash
nix-shell
cd test
daml build
```

Run the cucumber tests:

```bash
nix-shell
cd hs
cabal repl exe:daml-cucumber
:main --dar "../test/.daml/dist/daml-cucumber-0.1.0.0.dar" --host "localhost" --port 6865 --script "Test:main" --feat "../test/features.feature"
```

<div align="center">

# daml-cucumber 🥒

### Behavior-driven development for Daml.

Write your tests in plain-language [Gherkin](https://cucumber.io/docs/gherkin/reference/) feature files, implement each step as a [Daml Script](https://docs.daml.com/daml-script/index.html) action, and get a per-step pass/fail report.

[![Haskell](https://img.shields.io/badge/language-Haskell-blue.svg)](http://www.haskell.org) [![Built with Daml](https://img.shields.io/badge/Daml-1D345D)](https://docs.daml.com) [![Built with Nix](https://img.shields.io/static/v1?logo=nixos&logoColor=white&label=&message=Built%20with%20Nix&color=41439a)](https://nixos.org) [![Obsidian](https://img.shields.io/badge/Obsidian-Systems-white)](https://obsidian.systems) [![License: BSD-3-Clause](https://img.shields.io/badge/License-BSD%203--Clause-blue.svg)](LICENSE)

</div>

https://github.com/obsidiansystems/daml-cucumber/assets/7432518/f13cd205-8342-43df-81fc-ce23b4518679

This repository provides both the Daml library you import to write step implementations and the executable that reads your `.feature` files and drives your Daml test script.

## How to use this library

### Add daml-cucumber to your project
The daml-cucumber Daml library lives in the `./daml` folder of this project. Build it and import the resulting `daml-cucumber-<version>.dar` into your project as one of the [`data-dependencies` in your daml.yaml file](https://docs.daml.com/tools/assistant-build.html#add-a-package-to-a-multi-package-configuration).

You can build the daml-cucumber daml library with the following commands:

```bash
nix-shell
cd daml
daml build
```

### Implement tests for each cucumber step

Your Daml test suite should import [`Cucumber`](./daml/Cucumber.daml), which provides the function `liftScript` and the `Cucumber` [Action](https://docs.daml.com/daml/intro/5_Restrictions.html#actions-and-do-blocks). These can be used to define cucumber scenario implementations. For example, given the following template and feature file:

```haskell
template X
  with
    owner : Party
  where
    signatory owner
```

```cucumber
Feature: Example

  Scenario: a contract can be created
    Given a party
    When the party creates contract X
    Then Contract X is created
```

You can implement each step as a `Cucumber` action. As long as you annotate the step with a comment that matches the step definition in your feature file, it will be detected and run at the appropriate time.

```haskell
data Ctx = Ctx with
  party1 : Optional Party

instance Default Ctx where
  def = Ctx with party1 = None

-- Given a party
givenAParty: Cucumber Ctx ()
givenAParty = do
  p <- liftScript $ allocateParty "alice"
  put $ Ctx with party1 = Some p

-- When the party creates contract X
whenThePartyCreatesContact : Cucumber Ctx (ContractId X)
whenThePartyCreatesContact = do
  malice <- gets party1
  case malice of
    Some alice -> liftScript $ submit alice $ createCmd X with owner = alice
    _ -> error "Missing party1"

-- Then Contract X is created
thenContractIsCreated : Cucumber Ctx ()
thenContractIsCreated = do
  malice <- gets party1
  case malice of
    Some alice -> do
      contracts <- liftScript $ query @X alice
      assertMsg "Must have exactly one contract" $ Prelude.length contracts == 1
    _ -> error "Missing party1"

```

A full project example (using the Daml skeleton app) is available in the [example folder](./example).

### Sharing scenario state

Each scenario has a state or context that is shared by all of the steps that implement that scenario. You can use the functions defined in [DA.Action.State.Class](https://docs.daml.com/daml/stdlib/DA-Action-State-Class.html) to `get`, `put`, and `modify` the scenario state.

## Running scenarios with daml-cucumber

Launch daml-cucumber to run tests like so:

```bash
daml-cucumber \
  --features <path-to-your-feature-files> \
  --source <path-to-your-daml-project>
```

daml-cucumber runs every scenario in the given feature files and prints a report to your terminal that looks like this:

```
Feature: Example
  Scenario: a contract can be created
    Given a party => Passed
    When the party creates contract X => Passed
    Then Contract X is created => Failed: Not implemented
```

Unless you pass `--allow-missing`, a step in a feature file with no matching implementation is treated as an error.

### Command-line options

| Option | Description |
| --- | --- |
| `--features`, `-f` | A `.feature` file, or a directory of them. Repeatable; at least one is required. |
| `--source` | The Daml project directory (the one containing `daml.yaml`). |
| `--watch` | Re-run automatically when feature or Daml files change. |
| `--generate-only` | Write `Generated.daml` without running the tests (see below). |
| `--allow-missing` | Don't fail when a step has no implementation. |
| `--verbose`, `-v` | Verbose output. |

### Inspecting test results with VSCode

daml-cucumber generates a Daml file that can be opened in VSCode or evaluated with `daml test`. It is generated whenever daml-cucumber runs, but you can also generate it at any time with the following command:

```bash
daml-cucumber \
  --features <path-to-your-feature-files> \
  --source <path-to-your-daml-project> \
  --generate-only
```

This will create a file called `Generated.daml` that contains a function for each scenario in your feature files:

```haskell
-- | Scenario: a contract can be created
aContractCanBeCreated: Script ()
aContractCanBeCreated = do
  _ <- runCucumber $ do
    givenAParty
    whenThePartyCreatesContact
    thenContractIsCreated
  pure ()
```

## Building daml-cucumber

To build the daml-cucumber executable, run:

```bash
cd hs
nix-build
```

## Working on daml-cucumber

From the project root, run `nix-shell` to get a shell with the `daml` command, `daml sdk`, `ghci`, `cabal`, and necessary Haskell packages installed.

Now you can run the cucumber tests:

```bash
nix-shell
cd hs
cabal repl exe:daml-cucumber
:main --source ../test --features ../test/features.feature
```

### Setting up the Nix Binary Cache

To speed up the build process, you can fetch pre-built artifacts from our binary cache.

1. [Install Nix](https://nixos.org/nix/). If you already have Nix installed, make sure you have version 2.0 or higher. To check your current version, run nix-env --version.

2. Set up nix caches
    1. If you are running NixOS, add this to `/etc/nixos/configuration.nix`:
        ```nix
        nix.binaryCaches = [ "s3://obsidian-open-source" ];
        nix.binaryCachePublicKeys = [ "obsidian-open-source:KP1UbL7OIibSjFo9/2tiHCYLm/gJMfy8Tim7+7P4o0I=" ];
        ```
        and rebuild your NixOS configuration (e.g. `sudo nixos-rebuild switch`).
    2. If you are using another operating system or Linux distribution, ensure that these lines are present in your Nix configuration file (`/etc/nix/nix.conf` on most systems; [see full list](https://nixos.org/nix/manual/#sec-conf-file)):
        ```nix
        binary-caches = https://cache.nixos.org s3://obsidian-open-source
        binary-cache-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= obsidian-open-source:KP1UbL7OIibSjFo9/2tiHCYLm/gJMfy8Tim7+7P4o0I=
        binary-caches-parallel-connections = 40
        ```

### Building Docker Containers

To build the docker containers you can run
```bash
nix-build -A daml-$sdkversion.container
```

and you'll get a .tar.gz that you can use

```bash
docker load -i $tarfile
```

to import

to push all the containers you can run

```bash
$(nix-build -A pushScript)/bin/docker-push-generated
```

## About Obsidian Systems

daml-cucumber is built and maintained by **[Obsidian Systems](https://obsidian.systems)**. We provide frontier engineering for high-assurance systems: we build production software in Haskell, Daml, Canton, and Nix, and we're long-time stewards of open-source tooling like [Obelisk](https://github.com/obsidiansystems/obelisk), [Reflex](https://reflex-frp.org/), and [nix-thunk](https://github.com/obsidiansystems/nix-thunk).

If you're working with Daml, Canton, Nix, or Haskell and want a partner to help design, build, or ship it, we'd love to hear from you.

- Website: <https://obsidian.systems>
- Blog: <https://blog.obsidian.systems>
- GitHub: <https://github.com/obsidiansystems>

## License

daml-cucumber is released under the [BSD-3-Clause License](LICENSE), © 2024 Obsidian Systems LLC.

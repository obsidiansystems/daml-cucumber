# daml-cucumber 🥒

[Behavior-driven-development](https://cucumber.io/docs/bdd/) for Daml script using Cucumber's [Gherkin](https://cucumber.io/docs/gherkin/reference/) specification language.

## Getting Started

This repository includes both a Daml library and an executable that reads gherkin .feature files and invokes your Daml test script with the feature as input.

### Daml library

The daml-cucumber daml library is found in the `./daml` folder of this project. You can build and import `daml-cucumber-<version>.dar`, into your project as one of the [`data-dependencies` in your daml.yaml file](https://docs.daml.com/tools/assistant-build.html#add-a-package-to-a-multi-package-configuration).

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

### daml-cucumber executable

Launch daml-cucumber to run tests like so:

```bash
daml-cucumber \
  --features <path-to-your-feature-files> \
  --source <path-to-daml-files-implementing-steps>
  ```

daml-cucumber will run all of the scenarios in the specified feature file and produce a report in your terminal that looks like the following:

```
Feature: Example
  Scenario: a contract can be created
    Given a party => Passed
    When the party creates contract X => Passed
    Then Contract X is created => Failed: Not implemented
```

daml-cucumber will also notify you of missing steps, if the `--allow-missing` flag is not set, missing steps is an error.

#### Inspecting test results with VSCode

daml-cucumber generates a daml file that can be opened in VSCode or evaluated with `daml test`. It is generated whenever daml-cucumber runs, but you can also generate it at any time with the following command:

```bash
daml-cucumber
  --features <path-to-your-feature-files> \
  --source <path-to-daml-files-implementing-steps> \
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

From the project root, run `nix-shell` to get a shell with the `daml` command, `daml sdk`, `ghci`, `cabal`, and necessary haskell packages installed.

Now you can run the cucumber tests:

```bash
nix-shell
cd hs
cabal repl exe:daml-cucumber
:main --source ../test --features ../test/features.feature
```

### Nix Binary Cache

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

### Docker Containers

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

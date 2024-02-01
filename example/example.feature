Feature: Asset

  Scenario: An asset can be issued
    Given an issuer party
    When the issuer creates an asset with the issuer as the owner
    Then an asset is created owned by the issuer

  Scenario: An asset can be issued to someone else
    Given an issuer party
    And a second party
    When the issuer creates an asset with the second party as the owner
    Then an asset is created owned by the second party

  Scenario: Asset name cannot be blank
    Given an issuer party
    When the issuer creates an asset with an empty name
    Then the asset cannot be issued

  Scenario: Owners cannot give assets to themselves
    Given an asset
    When the owner of the asset tries to give it to themself
    Then the asset's id does not change

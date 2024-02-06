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

  Scenario Outline: Issue asset <name> and transfer to <party>

    Given an asset called <name>
    When the issuer transfers the asset called <name> to <party>
    Then <party> now has the asset called <name>

    Examples: assets and parties
      | name | party |
      | Car  | alice |
      | Boat | bob   |
      | Hat  | bob   |
      | Shoe | alice |

  # Scenario: An asset can be transferred
  #   Given an asset
  #   Given a third party
  #   When the owner of the asset gives it to the third party
  #   Then the third party has the asset
  #   Then the previous owner does not

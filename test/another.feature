Feature: Example2

  Scenario: a contract can maybe be created
    Given a party
    When the party creates contract X
    Then Contract X is created

  Scenario: This would(usually) fail, but now it shouldn't!
    Given a party
    When the party creates contract X
    Then Contract X is created

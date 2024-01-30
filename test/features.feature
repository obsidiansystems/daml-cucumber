Feature: Example

  Scenario: a contract can be created
    Given a party
    When the party creates contract X
    Then Contract X is created

  Scenario: Something else that can be tested
    Given this other guy I know
    Then he also did something

  Scenario Outline:  Linking a animal to an type
    Given there is a animal and a type
    And the <animal-category> is a <animal type>
    Then the <animal-category> is pretty cool I guess

    Examples: animals
      | animal category                 | animal type  |
      | Dog                                | mammal           |
      | Dog                                | bird           |
      | Dog                                | salad          |
      | Dog                                | toaster           |
      | Parrot                                | mammal           |
      | Parrot                                | bird           |
      | Parrot                                | salad          |
      | Parrot                                | toaster           |
      | Cat           | mammal           |
      | Cat           | bird           |
      | Cat           | salad          |
      | Cat           | toaster           |
      | Toaster    | mammal           |
      | Toaster    | bird           |
      | Toaster    | salad          |
      | Toaster    | toaster           |
      | Caesar                  | mammal           |
      | Caesar                  | bird           |
      | Caesar                  | salad          |
      | Caesar                  | toaster           |

   # We currently don't turn Outlines into multiple scenarios so I am gonna copy-pasta some here
   Scenario:  Linking a animal to an type 1
    Given there is a animal and a type
    And the Dog is a mammal
    Then the Dog is pretty cool I guess

   Scenario:  Linking a animal to an type 2
    Given there is a animal and a type
    And the Cat is a toaster
    Then the Cat is pretty cool I guess

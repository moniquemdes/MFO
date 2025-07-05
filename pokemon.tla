-------------- MODULE pokemon --------------
EXTENDS Integers

VARIABLES pokemons, nextTeam

POKEMONS == {
  [ name |-> "Squirtle", hp |-> 44, speed |-> 43, team |-> "", pokemonType |-> "water" ],
  [ name |-> "Bulbasaur", hp |-> 45, speed |-> 45, team |-> "", pokemonType |-> "grass" ],
  [ name |-> "Charmander", hp |-> 39, speed |-> 65, team |-> "", pokemonType |-> "fire" ]
}

otherTeam(team) == IF team = "A" THEN "B" ELSE "A"

damage(pokemon, d) == [pokemon EXCEPT !.hp = @ - d]

damageModifier(attacker, receiver) ==
  IF attacker.pokemonType = "water" THEN
    IF receiver.pokemonType = "water" THEN "not very effective"
    ELSE IF receiver.pokemonType = "fire" THEN "super effective"
    ELSE "not very effective"
  ELSE IF attacker.pokemonType = "fire" THEN
    IF receiver.pokemonType = "water" THEN "not very effective"
    ELSE IF receiver.pokemonType = "fire" THEN "not very effective"
    ELSE "super effective"
  ELSE
    IF receiver.pokemonType = "water" THEN "super effective"
    ELSE IF receiver.pokemonType = "fire" THEN "not very effective"
    ELSE "not very effective"

Tackle(attacker, receiver) == pokemons' = [pokemons EXCEPT ![receiver.team] = damage(@, 10)]

ElementalAttack(attacker, receiver) ==
  LET baseDamage == 10
      actualDamage == IF damageModifier(attacker, receiver) = "super effective" THEN baseDamage * 2
                      ELSE IF damageModifier(attacker, receiver) = "not very effective" THEN baseDamage \div 2
                      ELSE baseDamage
  IN
    pokemons' = [pokemons EXCEPT ![receiver.team] = damage(@, actualDamage)]

Attack(attacker, receiver) ==
  /\ attacker /= receiver
  /\ attacker.hp > 0
  /\ receiver.hp > 0
  /\ \/ Tackle(attacker, receiver)
     \/ ElementalAttack(attacker, receiver)

Init == \E pokemonsMap \in [ { "A", "B" } -> POKEMONS ]:
  /\ pokemons = [pokemonsMap EXCEPT !["A"].team = "A", !["B"].team = "B"]
  /\ nextTeam = IF pokemonsMap["A"].speed > pokemonsMap["B"].speed THEN "A" ELSE "B"

Next ==
  LET attacker == pokemons[nextTeam]
      receiver == pokemons[otherTeam(nextTeam)]
  IN /\ Attack(attacker, receiver)
     /\ nextTeam' = otherTeam(nextTeam)

Inv == \A team \in DOMAIN pokemons: pokemons[team].hp > 0
================================================================

from battle import *
heroes = [Creature("Phrobald the Halfling", Fixed(50.0), Roll(6.0, 2.0))]
orcs = [Creature("Orc", Roll(6.0, 1.0), Fixed(2.0)), Creature("Orc",
Roll(6.0, 1.0),
Fixed(2.0)), Creature("Orc", Roll(6.0, 1.0), Fixed(2.0))]
minotaurs = [Creature("Minotaur", Roll(8.0, 4.0), Roll(8.0, 1.0))]
battle(heroes, orcs + minotaurs)
from battle import *
heroes = [Creature("Phrobald the Halfling", Fixed(50), Roll(2, 6))]
orcs = [Creature("Orc", Roll(1, 6), Fixed(2)), Creature("Orc", Roll(1, 6), Fixed(2)), Creature("Orc", Roll(1, 6), Fixed(2))]
minotaurs = [Creature("Minotaur", Roll(4, 8), Roll(1, 8))]
battle(heroes, orcs + minotaurs)
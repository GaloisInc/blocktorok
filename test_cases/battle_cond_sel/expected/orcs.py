from battle import *
heroes = [Creature("Phrobald the Halfling", Fixed(50), Roll(2, 6))]
minotaurs = [Creature("Minotaur", Roll(4, 8), Roll(1, 8))]
battle(heroes, minotaurs)
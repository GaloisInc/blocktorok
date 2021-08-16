import random

class Creature:
  def __init__(self, name, hp, dmg):
    self.name = name
    self.hp = hp.sample()
    self.dmg = dmg

class Roll:
  def __init__(self, number, sides):
    self.number = int(number)
    self.sides = int(sides)

  def sample(self):
    return sum([random.randint(1, self.sides) for _ in range(0, self.number)])

class Fixed:
  def __init__(self, amount):
    self.amount = amount

  def sample(self):
    return self.amount

def is_side_alive(side):
  any([s.hp > 0 for s in side])

def attack(creat, side):
  if(len(side) == 0):
    return

  target = random.choice(side)
  dmg = creat.dmg.sample()
  print(f"{creat.name} attacks {target.name} for {dmg}!!")
  target.hp = target.hp - dmg
  if target.hp <= 0:
    side.remove(target)
    print(f"{target.name} dies!")
  else:
    print(f"{target.name} has {target.hp} HP after the attack.")

def battle(good, evil):
  while(len(good) > 0 and len(evil) > 0):
    for g in good:
      attack(g, evil)

    for e in evil:
      attack(e, good)

  if len(evil) > 0:
    print("The forces of evil have triumphed.  All is lost.")
  else:
    print("This day, the forces of good celebrate victory!")



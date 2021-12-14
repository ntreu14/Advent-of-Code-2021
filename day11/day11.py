grid = [[int(i) for i in line] for line in open('input.txt', 'r').read().splitlines()]

def findFlashed(x, y):
  flashed = []
  for i, j in [(x-1, y-1), (x-1, y), (x-1, y+1), (x, y-1), (x, y+1), (x+1, y-1), (x+1, y), (x+1, y+1)]:
    
    # (i, j) between 1 and 9 and it's not flashed yet 
    if 0 <= i < 10 and 0 <= j < 10 and grid[i][j] != 10:
      grid[i][j] += 1

      # If it is now, we found flashed octopus
      if grid[i][j] == 10:
        flashed.append((i, j))

  return flashed

def solve():
  total = 0
  steps = 0

  # We don't know when to stop yet
  while True: 
    flashed = []

    # We know that the grid is 10 x 10
    for y in range(10):
      for x in range(10):
        grid[y][x] += 1
        if grid[y][x] == 10:
          flashed.append((y, x))

    # Find others adjacent that are flashed
    for (x, y) in flashed:
      flashed += findFlashed(x, y)
    
    total += len(flashed)
    steps += 1

    # Part 1 is only looks for the number of flashes after 100 steps
    if steps == 100:
      part1 = total

    # Part 2 is looking for when they are all flashed
    # 10 per row x 10 energy level per octopus means that the whole row is flashed
    if all(sum(row) == 100 for row in grid):
      return part1, steps

    # Have to reset flashed octopuses back to 0 for the next step
    for y, x in flashed:
      grid[y][x] = 0

print(solve())
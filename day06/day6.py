from collections import Counter

lanternfish = [4,1,1,1,5,1,3,1,5,3,4,3,3,1,3,3,1,5,3,2,4,4,3,4,1,4,2,2,1,3,5,1,1,3,2,5,1,1,4,2,5,4,3,2,5,3,3,4,5,4,3,5,4,2,5,5,2,2,2,3,5,5,4,2,1,1,5,1,4,3,2,2,1,2,1,5,3,3,3,5,1,5,4,2,2,2,1,4,2,5,2,3,3,2,3,4,4,1,4,4,3,1,1,1,1,1,4,4,5,4,2,5,1,5,4,4,5,2,3,5,4,1,4,5,2,1,1,2,5,4,5,5,1,1,1,1,1,4,5,3,1,3,4,3,3,1,5,4,2,1,4,4,4,1,1,3,1,3,5,3,1,4,5,3,5,1,1,2,2,4,4,1,4,1,3,1,1,3,1,3,3,5,4,2,1,1,2,1,2,3,3,5,4,1,1,2,1,2,5,3,1,5,4,3,1,5,2,3,4,4,3,1,1,1,2,1,1,2,1,5,4,2,2,1,4,3,1,1,1,1,3,1,5,2,4,1,3,2,3,4,3,4,2,1,2,1,2,4,2,1,5,2,2,5,5,1,1,2,3,1,1,1,3,5,1,3,5,1,3,3,2,4,5,5,3,1,4,1,5,2,4,5,5,5,2,4,2,2,5,2,4,1,3,2,1,1,4,4,1,5]

def solve(days, start):
  counter = Counter(start)
  for _ in range(days):
    new_counter = Counter()
    for fish, count in counter.items():
      new_state = fish - 1
    
      if (new_state == -1):
        new_counter[8] += count
        new_state = 6
    
      new_counter[new_state] += count
      print (new_counter)
    
    counter = new_counter
  
  return sum(counter.values())

print(solve(80, lanternfish))
print(solve(256, lanternfish))
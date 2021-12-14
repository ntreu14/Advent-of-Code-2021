from collections import Counter, defaultdict
from itertools import pairwise

template = 'NBOKHVHOSVKSSBSVVBCS'

rules = {}
for line in open('input.txt', 'r').read().splitlines():
  s = line.split('->')
  rules[s[0]] = s[1]

def countOccurrences(n, template, rules):
  templateCounts = Counter(template)
  polymerCounts = Counter([''.join(c) for c in pairwise(template)])
  for _ in range(n):
    
    # Since we are looping over keys in polymerCounts, we need an intermediate dictionary
    current = defaultdict(int)
    for key in polymerCounts.keys():

      # How many time have we seen this insertions before?
      current[key[0] + rules[key]] += polymerCounts[key]
      current[rules[key] + key[1]] += polymerCounts[key]
      templateCounts[rules[key]] += polymerCounts[key]
    
    polymerCounts = current
    
  return templateCounts.most_common()[0][1] - templateCounts.most_common()[-1][1]

print(countOccurrences(40, template, rules))

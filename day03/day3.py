from collections import Counter

binaryNums = open('input.txt', 'r').read().splitlines()

def part1(input):
  mostCommon = ''
  leastCommon = ''

  for colNum, _ in enumerate(input[0]):
    alongCol = ''
    for row in input:
      alongCol += row[colNum]

    counts = Counter(alongCol).most_common()
    mostCommon += counts[0][0]
    leastCommon += counts[1][0]

  print(int(mostCommon, 2) * int(leastCommon, 2))

def getBit(bitCounts, defaultBit, maxOrMinPosition):
  if len(bitCounts) == 1:
    return bitCounts[0][0]
  else:
    return defaultBit if bitCounts[0][1] == bitCounts[1][1] else bitCounts[maxOrMinPosition][0]

def getMaxBit(bitCounts):
  return getBit(bitCounts, '1', 0)

def getMinBit(bitCounts):
  return getBit(bitCounts, '0', 1)

def findRating(input, ratingFn):
  remaining = input
  for colNum, _ in enumerate(input[0]):
    alongCol = ''
    for row in remaining:
      alongCol += row[colNum]

    if len(remaining) != 1:
      bitCounts = Counter(alongCol).most_common()
      maxBit = ratingFn(bitCounts)
      remaining = list(filter(lambda remainingRow: remainingRow[colNum] == maxBit, remaining))
  
  return remaining[0]

def findOxygenGeneratorRating(input):
  return findRating(input, getMaxBit)

def findCO2ScrubberRating(input):
  return findRating(input, getMinBit)

def part2(input):
  O2GeneratorRating = findOxygenGeneratorRating(input)
  C02GeneratorRating = findCO2ScrubberRating(input)
  print(int(O2GeneratorRating, 2) * int(C02GeneratorRating, 2))

part1(binaryNums)
part2(binaryNums)
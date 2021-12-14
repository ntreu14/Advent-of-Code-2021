input = open('input.txt', 'r').readlines()

vertices = [line.strip().split('-') for line in input]
graph = {}

for v in vertices: 
  for i in range(2):
    graph.setdefault(v[i], []).append(v[i-1])

visits = { node: 0 for node in graph }

def countPaths(stack, node, paths, doubles):
  if visits[node] != 0 and node.islower():
    if not doubles or node == 'start':
      return paths
    
    doubles = False
  
  if node == 'end':
    return paths + 1
  else:
    stack.append(node)
    visits[node] += 1
    
    for neighbor in graph[node]:
      paths = countPaths(stack, neighbor, paths, doubles)
    
    stack.pop()
    visits[node] -= 1 
    
    if visits[node] != 0 and node.islower():
      doubles = True
    
  return paths

def part1():
  return countPaths([], 'start', 0, False)

def part2():
  return countPaths([], 'start', 0, True)

print(part1())
print(part2())
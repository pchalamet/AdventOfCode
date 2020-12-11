
toto = [ 1, 2, 3, 4 ]

with open('Input1.txt') as f:
    nombres = [ int(x) for x in f ]

for i in range(0, len(nombres)):
    x = nombres[i]
    for j in range(i+1, len(nombres)):
        y = nombres[j]
        if x + y == 2020:
            print(x * y)

print("fin")

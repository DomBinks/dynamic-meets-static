x = {1, 2, 3, 4, 5, 6}

for i = 1, 6 do
    x[i] = x[i] * x[i]
end

for i = 1, 6 do
    print(x[i])
end

x = {}
print(x[6])
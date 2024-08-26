x = 4
y = 8

for i = 0, x do
    j = 0
    while j <= y do
        print(i + j)

        if j == 7 then
            break
        else
            j = j + 1
        end
    end
end

z = 0
repeat
    z = z + 1
until z == 10
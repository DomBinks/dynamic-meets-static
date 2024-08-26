x = 0

function testFunc (x, y, z)
    a = 1
    while true do
        a = a * 5
        if a > 20 then
            break
        end
    end
    b = 0
    for i=0,5 do
        for j=0,5 do
            k = 0
            while k < 4 do
                b = b + 1
                k = k + 1
            end
        end
    end
end

function test2()
    return nil
end

function id(x)
    return x
end

function test3(m,n)
    return 100
end

y = testFunc(1, 2, 3)
print(y)
print(a)
print(b)
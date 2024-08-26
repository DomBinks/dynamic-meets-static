a = "abc"

function test()
    a = "set"
end

print(a)
test()
print(a)

x = 0
function recur()
    if x < 3 then
        x = x + 1
        recur()
    else
        return
    end
end
recur()
print(x)

p = 32
function set(p)
    p = 64
    print(p)
end

print(p)
set(p)
print(p)

function fib(n)
    if n == 0 then
        return 0
    elseif n == 1 then
        return 1
    elseif n == 2 then
        return 1
    else
        return fib(n-1) + fib(n-2)
    end

    return -1
end

print("Fibonacci's Sequence:")
fibs = {1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144}
fibs2 = {1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 145}
for i=1,12 do
    x = fib(i)
    print(x)
    print(x == fibs2[i])
end
print("1 1 2 3 5 8 13 21 34 55 89 144")
fibs2 = {}
print(fibs2[2])
hs:nil[4] = {}
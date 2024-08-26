function fib(n)
    if n == 0 then
        return 0
    elseif n == 1 then
        return 1
    else
        return fib(n-1) + fib(n-2)
    end

    return -1
end

n = 35
print(fib(n))
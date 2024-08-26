function max(a, b)
    if a > b then
        return a
    end

    return b
end

function cutRod(p, n, i)
    if n == 0 then
        return 0
    end

    q = -99
    for i = 1, n do
        q = max(q, p[i] + cutRod(p, n-i, 0))
    end

    return q
end

p = {10, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1}
n = 20
print(cutRod(p, n, 0))
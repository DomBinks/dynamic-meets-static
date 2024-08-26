s = "Some string"

function set()
    s = "Some other string"
end

function stuff(x)
    a = 1 + 2
    b = a / 2
    c = "Another string"

    print(c)
    print(x)

    return true
end

print(s)
set()
print(s)
stuff(s)

print(nil == nil)
print(s == "Some other string")
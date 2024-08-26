function fizzbuzz(n)
    if n % 3 == 0 and n % 5 == 0 then
        print("fizzbuzz")
    elseif n % 3 == 0 then
        print("fizz")
    elseif n % 5 == 0 then
        print("buzz")
    else
        print(n)
    end
end

for i = 1, 30 do
    fizzbuzz(i)
end
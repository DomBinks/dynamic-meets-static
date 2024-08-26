bs = {true, true, true}
print(bs[1] == bs[2] == bs[3])

function test(as)
    as = {}
    as = {1, 2, 3, 4, 5}
    print(as[5])
    bs = {false, false, false}
    as[1]:number = 5
    as[2] = 6
    as[3] = 7
    as[4] = 8
    print(as[1])
    print(as[2])
    print(as[3])
    print(as[4])
    print(as[5])
end

xs:number[5] = {1, 1, 1, 1, 1}
test(xs)

big:number[1024] = {}
big2:number[2048] = {}
big3:number[2048] = {}
print(big3[2])

big[512] = 64
big = {}
print(big[512])
print(big[513])

function fcmp()
    return nil
end

h:number[2] = {}
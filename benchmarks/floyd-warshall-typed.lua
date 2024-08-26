dist:number[16384] = {}
v = 128

function getInd(row, col)
    return v * (row - 1) + col
end

for i = 1, v do
    for j = 1, v do
        dist[getInd(i, j)] = 99
    end
end

dist[getInd(11, 22)] = 1
dist[getInd(11, 22)] = 2
dist[getInd(11, 33)] = 3
dist[getInd(11, 44)] = 4
dist[getInd(11, 55)] = 5
dist[getInd(11, 66)] = 6
dist[getInd(11, 77)] = 7
dist[getInd(11, 88)] = 8
dist[getInd(11, 99)] = 9

for i = 1, v do
    dist[getInd(i, i)] = 0
end

function floydWarshall()
    for k = 1, v do
        for i = 1, v do
            for j = 1, v do
                if dist[getInd(i, j)] > dist[getInd(i, k)] + dist[getInd(k, j)] then
                    dist[getInd(i, j)] = dist[getInd(i, k)] + dist[getInd(k, j)]
                end
            end
        end
    end
end

floydWarshall()
print(dist[getInd(11, 22)])
print(dist[getInd(11, 33)])
print(dist[getInd(11, 44)])
print(dist[getInd(11, 55)])
print(dist[getInd(11, 66)])
print(dist[getInd(11, 77)])
print(dist[getInd(11, 88)])
print(dist[getInd(11, 99)])
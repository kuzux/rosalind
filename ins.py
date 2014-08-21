def isort(xs):
    ctr = 0
    #n = len(xs)
    print len(xs)
    for i in range(1,n):
        k = i
        #print k
        while k > 0 and xs[k] < xs[k-1]:
            ctr += 1
            tmp = xs[k-1]
            xs[k-1] = xs[k]
            xs[k] = tmp
            k -= 1
    return (ctr, xs)

n = int(raw_input())
A = map(int, raw_input().split())
#print A

x, std = isort(A)
print x
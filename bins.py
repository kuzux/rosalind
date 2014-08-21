from bisect import bisect_left

def binary_search(a, x, lo=0, hi=None):   # can't use a to specify default for hi
    hi = hi if hi is not None else len(a) # hi defaults to len(a)   
    pos = bisect_left(a,x,lo,hi)          # find insertion position
    return (pos+1 if pos != hi and a[pos] == x else -1) # don't walk off the end

n = int(raw_input())
m = int(raw_input())

A = sorted(map(int, raw_input().split()))
b = map(int, raw_input().split())

for x in b:
    print binary_search(A, x), 

print

import sys
from collections import defaultdict, Counter

infile = sys.argv[1] if len(sys.argv)>1 else '6.in'
X = Counter([int(x) for x in open(infile).read().strip().split(',')])

def solve(S, n):
    X = S
    for day in range(n):
        Y = defaultdict(int)
        for x,cnt in X.items():
            
            if x==0:
                Y[6] += cnt # Y[6] = Y[6] + cnt
                Y[8] += cnt
            else:
                Y[x-1] += cnt
        print(Y)
        print("###########")
        X = Y
    return sum(X.values())

print(solve(X, 10))
#print(solve(X, 256))

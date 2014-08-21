# We have the recurrence relation
# f_i = \Sigma_{j=i-m}^{i-2} f_j    for i \geq m
# f_i = \Sigma_{j=0}^{i-2} f_j
# f_0 = 1, f_1 = 0
# F_i = \Sigma_{j=i-m+1}^{i} f_i

f = [1,0]

n = gets.to_i
m = gets.to_i

2.upto(n-1) do |i|
    tmp = 0
    start = i < m ? 0 : i-m
    start.upto(i-2) do |j| 
        tmp += f[j]
    end
    f.push tmp
end
sum = 0
(n-m).upto(n-1){ |i| sum += f[i] }
p sum


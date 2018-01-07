N = 100
y = rep(0,N)
y[1] = 10
p=0.5
z = rbinom(n=N,size=1,prob=p)
for (i in 2:100)
{
  y[i] = c(0.5 * y[i-1] + z[i])
}
plot(y, type = "l", main = 'Deterministic process', pch = 16)
library(jdeffects)
library(shellpipes)

startGraphics()

set.seed(233)

beta_int = 3
beta_dep = 7
sig = 12
dmean = 5

dep <- runif(100) + dmean
eps <- runif(100)

ind <- beta_dep*dep + beta_int + sig*eps

m <- lm(ind~dep)

pvp <- varpred(m, "dep")
plot(pvp)

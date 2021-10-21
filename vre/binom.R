library(vareffects)
library(ggplot2); theme_set(theme_bw())

library(shellpipes)
startGraphics()

n <- 100
bxz <- 1
bxy <- 0.6 ## Test this one as both 0 and non-zero; 
byz <- 0.8

x <- rnorm(n)
y <- rnorm(n) + bxy*x
zodds <- rnorm(n) + bxz*x + bxy*y

z <- rbinom(unodds(zodds), something like that)


m1 <- glm(z~x+y)

plot(varpred(m1, "x"))
plot(varpred(m1, "x", isolate=TRUE))

plot(varpred(m2, "x"))
plot(varpred(m2, "x", isolate=TRUE))

library(vareffects)
library(ggplot2)

theme_set(theme_bw())

library(shellpipes)
startGraphics()

n <- 100
bxz <- 0
bxy <- 0.8
byz <- 0.4

x <- rnorm(n)
y <- rnorm(n) + bxy*x
z <- rnorm(n) + bxz*x + bxy*y

m1 <- lm(z~x)
m2 <- lm(z~x+y)

plot(varpred(m1, "x"))
plot(varpred(m1, "x", isolate=TRUE))

plot(varpred(m2, "x"))
plot(varpred(m2, "x", isolate=TRUE))

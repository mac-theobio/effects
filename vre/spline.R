library(vareffects)
library(ggplot2); theme_set(theme_bw())
library(splines)
library(dplyr)

library(shellpipes)
startGraphics()

n <- 100
bx <- 1
bxx <- 0.8

dat <- tibble(NULL
	, x = rnorm(n)
	, z = rnorm(n) + bx*x + bxx*x^2
)

m1 <- lm(z~x, data=dat)
m2 <- lm(z~ns(x,3), data=dat)

print(plot(varpred(m2, "x", isolate=TRUE))
	+ geom_point(data=dat, aes(x=x, y=z))
)

## plot(varpred(m1, "x"))
## plot(varpred(m1, "x", isolate=TRUE))
## plot(varpred(m2, "x"))

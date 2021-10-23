library(vareffects)
library(splines)
library(dplyr)
library(ggplot2)

library(shellpipes)
startGraphics()

n <- 101
set.seed(101)

x <- seq(0, 1, length.out=n)
y <- rnorm(n)

mb <- lm(y~bs(x,3))
print(plot(varpred(mb, "x", isolate=TRUE))
)

mn <- lm(y~ns(x,3))
print(plot(varpred(mn, "x", isolate=TRUE))
)


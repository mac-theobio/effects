
n <- 100
r <- 0.8
s <- 0.8

bx <- 0.5
by <- 1

a <- rnorm(n)
b <- rnorm(n)
c <- rnorm(n)

x <- a
y <- r*a + s*b

z <- bx*x + by*y + c

m <- lm(z ~ x+y)

summary(m)

source("makestuff/makeRfuns.R")
commandEnvironments()

## Simple data
n <- 100
x1 <- rnorm(n)
x2 <- sample(rep(c("A", "B"), 1000), n)
y <- 0.5*x1 + rnorm(n)
df <- data.frame(y = y, x1 = x1, x2 = x2)

## Fit lm model
inter_mod <- lm(y ~ x1*x2, data = df)
summary(inter_mod)

saveVars(inter_mod
	, df
)

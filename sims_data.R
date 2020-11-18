# Simulate simple data

library(dplyr)
options(dplyr.width = Inf)

source("makestuff/makeRfuns.R") ## Will eventually be a package
commandEnvironments()

set.seed(7902)

people <- 500 # Number of simulations to run

# Beta values
y1_beta0 <- 0.3
y1_beta1 <- 0.1
y1_beta2 <- -0.6

e1 <- 1

# Predictors
x1 <- seq(1, 9, length.out = people)
summary(x1)
x2 <- sample(runif(people*people, 10, 100), people)
summary(x2)

## Error terms
y1_eps <- e1*rnorm(people)

## Linear predictor
XB <- data.frame(x1 = x1
	, x2 = x2
	, x1s = drop(scale(x1)) # Scaled
	, x2s = drop(scale(x2)) # Scaled
	, pred1 = y1_beta0 + y1_beta1 * x1 + y1_beta2 * x2 + y1_eps
)

## Simulated data
sim_df <- (XB
	%>% mutate(
		y1 = rbinom(people, 1, plogis(pred1))
	)
)

parm_list <- list(y1_beta0 = y1_beta0
	, y1_beta1 = y1_beta1
	, y1_beta2 = y1_beta2
	, e1 = e1
)

saveVars(sim_df, parm_list)


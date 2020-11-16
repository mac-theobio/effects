# Simulate simple multiple outcome data

library(dplyr)
options(dplyr.width = Inf)

source("makestuff/makeRfuns.R") ## Will eventually be a package
commandEnvironments()

set.seed(7902)

people <- 500 # Number of simulations to run

# Beta values
y1_beta0 <- 0.3
y2_beta0 <- 0.2

y1_beta1 <- 0.1
y2_beta1 <- 0.4

y1_beta2 <- 0.5
y2_beta2 <- 0.2

U1_beta <- 0.5
U2_beta <- 0.7

e1 <- 1
e2 <- 1

# Sd
U_sd <- 0.4

## Latent variables
U <- rnorm(people, 0, U_sd)


# Predictors
x1 <- seq(1, 9, length.out = people)
summary(x1)

x2 <- runif(people, 10, 100)
summary(x2)

## Error terms
y1_eps <- e1*rnorm(people)
y2_eps <- e2*rnorm(people)

## Linear predictor
XB <- data.frame(x1 = x1
	, x2 = x2
	, U = U
	, pred1 = y1_beta0 + y1_beta1 * x1 + y1_beta2 * x2 + y1_eps#+ U1_beta * U
	, pred2 = y2_beta0 + y2_beta1 * x1 + y2_beta2 * x2 + y2_eps#+ U2_beta * U
)

## Simulated data
sim_df <- (XB
	%>% mutate(
		y1 = rbinom(people, 1, plogis(pred1))
		, y2 = rbinom(people, 1, plogis(pred2))
	)
)

parm_list <- list(y1_beta0 = y1_beta0
	, y2_beta0 = y2_beta0
	, y1_beta1 = y1_beta1
	, y2_beta1 = y2_beta1
	, y1_beta2 = y1_beta2
	, y2_beta2 = y2_beta2
	, U1_beta = U1_beta
	, U2_beta = U2_beta
	, U_sd = U_sd
	, e1=e1, e2=e2
)

saveVars(sim_df, parm_list)


# Simulate simple data

library(dplyr)
options(dplyr.width = Inf)
library(shellpipes)

set.seed(7902)

people <- 500 # sample size

# Beta values
y1_beta0 <- 0.3
y1_beta1 <- 0.1
y1_beta2 <- -0.6

e1 <- 1

# Predictors
x1 <- seq(1, 9, length.out = people)
summary(x1)
x2 <- runif(people, 10, 100)
summary(x2)

## Error terms
y1_eps <- e1*rnorm(people)

## Make data frame
sim_df <- data.frame(x1 = x1
	, x2 = x2
	, x1s = x1/sd(x1)
	, x2s = x2/sd(x2)
	, x1c = x1 - mean(x1)
	, x2c = x2 - mean(x2)
	, x1std = drop(scale(x1)) # Scaled
	, x2std = drop(scale(x2)) # Scaled
	, y1 = y1_beta0 + y1_beta1 * x1 + y1_beta2 * x2 + y1_eps
)

sim_df <- (sim_df %>% mutate(
	y1bin = rbinom(people, 1, plogis(y1))
))

parm_list <- list(y1_beta0 = y1_beta0
	, y1_beta1 = y1_beta1
	, y1_beta2 = y1_beta2
	, e1 = e1
)

saveVars(sim_df, parm_list)


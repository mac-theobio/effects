library(shellpipes)
library(dplyr)

loadEnvironments()

set.seed(seed)

## Perfectly independent, normalized RVs (to perfectly match desired correlation, and probably partial correlations as well)
r1 <- rnorm(N)
r2 <- residuals(lm(rnorm(N)~r1))
r3 <- rnorm(N)
n1 <- (r1-mean(r1))/sd(r1)
n2 <- (r2-mean(r2))/sd(r2)

sim_df <- tibble(NULL
	, x = n1+xbar
	, y = rho*n1 + sqrt(1-rho^2)*n2+ybar
	, lc = b0 + bx*x + by*y
	, z = lc+r3
)

summary(sim_df)

saveVars(sim_df)

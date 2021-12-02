library(shellpipes)
library(dplyr)

loadEnvironments()
startGraphics()

set.seed(991)

## Simulate mediated variables such that:
### x -> y -> z

N <- 1e4
beta_0 <- 2
beta_xy <- 0
beta_xz <- 1	# Set to 0 -> x: no direct of x on z
beta_yz <- 0.8

df <- data.frame(x=rnorm(N))
sim_df_mediate <- (df
	%>% mutate(y = rnorm(N) + beta_xy*x
		, z = rnorm(N) + beta_xz*x + beta_yz*y + beta_0
		, zbin = rbinom(n(), 1, plogis(z))
	)
)

head(sim_df_mediate)

## Observed marginals
observed_df_med <- (sim_df_mediate
	%>% summarise_all(mean)
)
observed_df_med

saveVars(sim_df_mediate, observed_df_med, comparevarpred, binfun)

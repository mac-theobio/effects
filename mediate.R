library(shellpipes)
library(dplyr)

loadEnvironments()
startGraphics()

set.seed(991)

## Simulate mediated variables such that:
### x -> y -> z

N <- 1e4

b0 <- 5
bxz <- 0
bxy <- 1
byz <- 1.5

df <- data.frame(x=rnorm(N))
sim_df_mediate <- (df
	%>% mutate(y = rnorm(N) + bxy*x
		, z = b0 + bxz*x + byz*y
		, zbin = rbinom(N, 1, plogis(z))
		, z = z + rnorm(N)
	)
)

head(sim_df_mediate)

## Observed marginals
observed_df_med <- (sim_df_mediate
	%>% summarise_all(mean)
)
observed_df_med

saveVars(sim_df_mediate, observed_df_med, comparevarpred, binfun)

library(shellpipes)
library(dplyr)

loadEnvironments()
startGraphics()

set.seed(991)

## Simulate mediated variables such that:
### x -> y -> z

N <- 1e4

rho <- 0.8
b0 <- 5
bxz <- 0.2
byz <- 1.5

df <- data.frame(x=rnorm(N))
#sim_df_mediate <- (df
#	%>% mutate(y = rnorm(N) + bxy*x
#		, z = b0 + bxz*x + byz*y
#		, z = rbinom(N, 1, plogis(z))
#	)
#)
sim_df_mediate <- (df
	%>% mutate(y = rho*x + sqrt(1-rho^2)*rnorm(N)
		, z = b0 + bxz*x + byz*y
	#	, z = rbinom(N, 1, plogis(z))
		, z = rnorm(N, mean=z, sd=1)
	)
)

head(sim_df_mediate)

sim_mediate_betas <- c(b0, bxz, byz)

## Observed marginals
observed_df_med <- (sim_df_mediate
	%>% summarise_all(mean)
	%>% mutate(model="Data mean")
)
observed_df_med

saveVars(sim_df_mediate
	, observed_df_med
	, sim_mediate_betas
	, comparevarpred
	, binfun
)

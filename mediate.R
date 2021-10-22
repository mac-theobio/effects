library(shellpipes)
library(dplyr)

commandEnvironments()
makeGraphics()

set.seed(991)

## Simulate mediated variables such that:
### x -> y -> z

N <- 100
beta_xy <- 0.8
beta_xz <- 0	# Set to 0 -> x: no direct of x on z
beta_yz <- 0.8

df <- data.frame(x=rnorm(N))
sim_df_mediate <- (df
	%>% mutate(y = rnorm(N) + beta_xy*x
		, z = rnorm(N) + beta_xz*x + beta_yz*y
	)
)

head(sim_df_mediate)

saveVars(sim_df_mediate, comparevarpred)

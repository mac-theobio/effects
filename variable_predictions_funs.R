
## Simulate data for simple lm or glm
### If link_scale=TRUE -> lm otherwise glm
simplesim <- function(N=1e4, beta0=1.5, beta1=1.0, beta2=2
		, x1_sd=1, x1_mean=0.2
		, x2_sd=1, x2_mean=0
		, link_scale = FALSE
	) {
	x1 <- rnorm(N, x1_mean, x1_sd)
	x2 <- rnorm(N, x2_mean, x2_sd)
	eta <- beta0 + beta1 * x1 + beta2 * x2
	if (link_scale) {
		eta <- eta + rnorm(N) 
	}
	sim_df <- data.frame(x1=x1, x2=x2, y=eta)
	if (!link_scale) {
		sim_df <- (sim_df
			%>% mutate(y = rbinom(N, 1, plogis(y)))
		)
	}
	return(sim_df)
}

saveEnvironment()

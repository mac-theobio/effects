
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

## Simulate data for lm and glm with interactions
interactionsim <- function(N=1e4, beta0=1.5, beta1=1.0
		, beta2=2, beta3=1.5, beta23=1
		, x1_sd=1, x1_mean=0.2
		, x2_sd=1, x2_mean=0
		, x3_sd=1, x3_mean=0
		, link_scale = FALSE
		, form = ~x1 + x2 + x3 + x2:x3
	) {
	x1 <- rnorm(N, x1_mean, x1_sd)
	x2 <- rnorm(N, x2_mean, x2_sd)
	x3 <- rnorm(N, x3_mean, x3_sd)
	betas <- c(beta0, beta1, beta2, beta3, beta23)
	df <- data.frame(x1=x1, x2=x2, x3=x3)
	X <- model.matrix(form, df)
	eta <- as.vector(X %*% betas)
	if (link_scale) {
		eta <- eta + rnorm(N) 
	}
	df$y <- eta 
	if (!link_scale) {
		df <- (df
			%>% mutate(y = rbinom(N, 1, plogis(y)))
		)
	}
	return(df)
}


## Generate binned observations
binfun <- function(mod, focal, non.focal, bins=50, ...) {
	mf <- model.frame(mod)
	mm <- (mf
		%>% select_at(c(focal, non.focal))
	)
	check_df <- (mf
		%>% arrange_at(focal)
		%>% mutate(bin=ceiling(row_number()*bins/nrow(.)))
		%>% group_by(bin)
		%>% summarise_all(mean)
		%>% mutate(model="binned")
	)
	return(check_df)
}

saveEnvironment()

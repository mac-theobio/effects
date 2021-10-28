library(shellpipes)
library(dplyr)

commandEnvironments()
makeGraphics()

set.seed(991)

## Simulate binary response with correlated predictors:
### Switch beta_xy to 0 and non-zero 

simcorr <- function(N = 100
	, beta_xy = 0
	, beta_xz = 1
	, beta_yz = 0.8) {

	df <- data.frame(x=rnorm(N))
	df <- (df
		%>% mutate(y = rnorm(N) + beta_xy*x
			, z_eta = rnorm(N) + beta_xz*x + beta_yz*y
			, z = rbinom(N, 1, plogis(z_eta))
		)
		%>% select(-z_eta)
	)
	return(df)
}

N <- 100
sim_df_bin_corr <- simcorr(N = N, beta_xy=2)
head(sim_df_bin_corr)
sim_df_bin_noncorr <- simcorr(N = N, beta_xy=0)
head(sim_df_bin_noncorr)

## Observed marginals
observed_df_corr <- (sim_df_bin_corr
	%>% summarise_all(mean)
)
observed_df_corr

observed_df_noncorr <- (sim_df_bin_noncorr
	%>% summarise_all(mean)
)
observed_df_noncorr

saveVars(sim_df_bin_corr
	, sim_df_bin_noncorr
	, comparevarpred
	, observed_df_corr
	, observed_df_noncorr
)

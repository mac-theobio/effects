library(shellpipes)
library(dplyr)

set.seed(991)

N <- 1e4

beta_0 <- 2

nitro_beta <- 0.5
nitro_mean <- 2
nitro_sd <- 0.8
nitro_max <- 4

phos_beta <- 1.5
phos_sd <- 3

pot_beta <- 0.9

## Function to generate bins
binfun <- function(mod, focal, bins=50) {
	if (!inherits(mod, "data.frame")) {
		mf <- model.frame(mod)
	} else {
		mf <- mod
	}
	check_df <- (mf
		%>% arrange_at(focal)
		%>% mutate(bin=ceiling(row_number()*bins/nrow(.)))
		%>% group_by(bin)
		%>% summarise_all(mean)
		%>% mutate(model="binned")
	)
	return(check_df)
}


## Simulate data
dat <- (data.frame(nitro=rlnorm(N,meanlog=log(nitro_mean), sdlog=nitro_sd))
	%>% mutate(nitro=pmin(nitro, nitro_max)
		, nitro = nitro - min(nitro)
		, phos=rnorm(N, mean=1, sd=phos_sd)
		, pot = rnorm(N)
		, eta=beta_0 + nitro_beta*nitro + phos_beta*phos + pot_beta*pot
		, status=rbinom(N, 1, plogis(eta))
	)
)

## Generate bins
binned_df <- binfun(dat, "nitro")
head(binned_df)

## Fit model
mod <- glm(status ~ nitro + phos + pot, dat, family="binomial")

saveVars(dat, mod, binned_df)

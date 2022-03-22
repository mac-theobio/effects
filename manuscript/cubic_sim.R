library(shellpipes)
library(tidyr)
library(dplyr)

set.seed(9991)

# Global variables
N <- 1000
b0 <- 20
age_b11 <- 0.1
age_b12 <- 0.8
age_b13 <- 0.3
wealth_b2 <- 0.7
age_sd <- 1
wealthindex_sd <- 1
hhsize_sd <- 3

true_beta_bi <- c(b0, age_b11, age_b12, age_b13)

## Simulation
#### Bi-variate
bi_df <- (
	data.frame(age=rnorm(N, 0, age_sd)
		, wealthindex=rnorm(N, 0, wealthindex_sd)
	)
	%>% mutate(eta = b0 + age_b11*age + age_b12*age^2 + age_b13*age^3
		, hhsize=rnorm(N, mean=eta, sd=hhsize_sd)
	)
)
head(bi_df)

#### Multi-variate
multi_df <- (bi_df
	%>% mutate(eta = eta + wealth_b2*wealthindex
		, hhsize=rnorm(N, mean=eta, sd=hhsize_sd)
	)
	%>% select(-eta)
)
head(multi_df)
true_beta_multi <- c(true_beta_bi, wealth_b2)

saveVars(true_beta_bi
	, true_beta_multi
	, bi_df
	, multi_df
)

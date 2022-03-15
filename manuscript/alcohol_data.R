library(dplyr)
library(tibble)

library(shellpipes)

N <- 25 
seed <- 33
income_mean <- 3300
income_spread <- 1
edu_mean <- 5
edu_spread <- 0.5
edu_max <- 15
con_mean <- 20
con_spread <- 0.5
beta_ei <- 300
beta_ic <- 0.02
beta_ec <- -2

set.seed(seed)

## meanlog = log(0) works as desired in rlnorm!
dat <- (data.frame(edu = rlnorm(N, meanlog=log(edu_mean), sdlog=edu_spread))
	%>% mutate(NULL
		, edu = pmin(edu, edu_max)
		, income_pred = income_mean+(edu-edu_mean)*beta_ei
		, income = rlnorm(N, meanlog=log(income_pred), sdlog=income_spread)
		, con_pred=con_mean+(edu-edu_mean)*beta_ei+(income-income_mean)* income
		, con_pred=pmax(0, con_pred)
		, con = rlnorm(N, meanlog=log(con_pred), sdlog=con_spread)
	)
)

saveVars(dat)


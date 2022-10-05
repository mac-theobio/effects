library(shellpipes)
library(dplyr)

set.seed(991)

loadEnvironments()

df <- (data.frame(age=rlnorm(N, log(mean_age), age_sd)
		, income=rlnorm(N, log(mean_income), income_sd)
		, wealthindex=rnorm(N, 0, 1)
	)
	%>% mutate(NULL
		, age=pmin(age, age_max)
		, age=pmax(age, age_min)
		, income=pmin(income, income_max)
		, income = income - min(income)
		, eta = beta0 + betaA*age + betaI*income + betaW*wealthindex + betaIW*income*wealthindex
		, hhsize=rnorm(N, eta, hhsize_sd)
	)
	%>% select(-eta)
)
head(df)

saveVars(df)




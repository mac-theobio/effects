library(shellpipes)
library(dplyr)

loadEnvironments()

## Choose the same focal values
quants <- seq(0, 1, length.out=200)
focal <- (quantile(df$age, quants, names=FALSE)
	|> unique()
)

saveVars(focal
	, fit
	, df
	, hhsize_mean
)

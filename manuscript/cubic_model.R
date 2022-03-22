library(shellpipes)

loadEnvironments()

## Bi-variate model
bi_mod <- lm(hhsize ~ poly(age, degree=3, raw=TRUE), data=bi_df)
summary(bi_mod)

## Multi-variate model
multi_mod <- lm(hhsize ~ poly(age, degree=3, raw=TRUE) + wealthindex, data=multi_df)
summary(multi_mod)


saveVars(bi_mod
	, multi_mod
	, true_beta_bi
	, true_beta_multi
	, bi_df
	, multi_df
)

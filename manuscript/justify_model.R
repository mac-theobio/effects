library(shellpipes)

loadEnvironments()
 
#### Model
## No interaction
justify_mod <- lm(y~x1+x2+x3
	, data=justify_sim_df
)
summary(justify_mod)
coef(justify_mod)
justify_sim_betas

## With interaction
justify_inter_mod <- lm(y~x1+x2*x3
	, data=justify_inter_sim_df
)
summary(justify_inter_mod)

saveEnvironment()


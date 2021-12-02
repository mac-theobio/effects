library(shellpipes)

loadEnvironments()
 

mod_bin_corr <- glm(z~x+y, sim_df_bin_corr, family=binomial)
summary(mod_bin_corr)

mod_bin_noncorr <- glm(z~x+y, sim_df_bin_noncorr, family=binomial)
summary(mod_bin_noncorr)

saveEnvironment()

library(shellpipes)

loadEnvironments()
startGraphics()
 
set.seed(9991)

############################################################################################
## Mediated models
############################################################################################

### Linear model

mod_notmediated <- lm(z~x, sim_df_mediate)
summary(mod_notmediated)

mod_mediated <- lm(z~x+y, sim_df_mediate)
summary(mod_mediated)

### glm model

mod_notmediated_bin <- glm(zbin~x, sim_df_mediate, family=binomial())
summary(mod_notmediated_bin)

mod_mediated_bin <- glm(zbin~x+y, sim_df_mediate, family=binomial())
summary(mod_mediated_bin)

saveEnvironment()

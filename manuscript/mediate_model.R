library(shellpipes)

loadEnvironments()
startGraphics()
 
set.seed(9991)

########################################################################
## Mediated models
########################################################################

### glm model

mod_notmediated <- glm(z~x, sim_df_mediate, family=gaussian())
summary(mod_notmediated)

mod_mediated <- glm(z~x+y, sim_df_mediate, family=gaussian())
summary(mod_mediated)

saveEnvironment()

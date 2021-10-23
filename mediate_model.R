library(shellpipes)

commandEnvironments()
makeGraphics()
 
set.seed(9991)

############################################################################################
## Mediated models
############################################################################################

mod_notmediated <- lm(z~x, sim_df_mediate)
summary(mod_notmediated)

mod_mediated <- lm(z~x+y, sim_df_mediate)
summary(mod_mediated)

saveEnvironment()

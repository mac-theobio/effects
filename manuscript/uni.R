library(shellpipes)
loadEnvironments()
startGraphics()

m <- lm(z ~ x, data=sim_df)

summary(m)
plot(m)

saveVars(m, sim_df)

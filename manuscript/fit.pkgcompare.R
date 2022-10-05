library(shellpipes)

loadEnvironments()

fit <- lm(hhsize ~ age + income + wealthindex + income:wealthindex, df)
summary(fit)

saveVars(fit, df)

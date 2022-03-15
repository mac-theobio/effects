library(shellpipes)

dat <- rdsRead()

summary(dat)

mod <- lm(con~edu+income, data=dat)

saveVars(mod, dat)

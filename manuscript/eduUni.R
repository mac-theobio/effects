library(shellpipes)

dat <- rdsRead()

summary(dat)

mod <- lm(con~edu, data=dat)

summary(mod)

saveVars(mod, dat)


library(shellpipes)

dat <- rdsRead()

summary(dat)

mod <- lm(con~poly(edu, 2) + income, data=dat)
mod <- lm(con~poly(edu, 2), data=dat)

summary(mod)

saveVars(mod, dat)


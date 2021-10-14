library(shellpipes)
library(vareffects)
library(lme4)

loadEnvironments()

summary(poism)

varpred(poism, "group")
varpred(poism, "wave")


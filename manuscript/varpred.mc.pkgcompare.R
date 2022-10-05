library(shellpipes)
library(varpred)

loadEnvironments()

varpred_mc <- (varpred(fit, "age", at=list(age=focal), modelname="vapred")
	|> as.data.frame()
)
head(varpred_mc)

saveVars(varpred_mc)

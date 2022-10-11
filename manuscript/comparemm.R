library(shellpipes)
library(varpred)
loadEnvironments()

mm <- model.matrix(fit)
head(mm)
bb <- coef(fit)
bb

v1 <- mm %*% bb |> as.vector()

v2 <- predict(fit)
dd <- data.frame(v1=v1, v2=v2)
dd
mean(v2)
hhsize_mean
mean(df$hhsize)

age_mean <- mean(df$age)

v3 <- varpred(fit, "age", at=list(age=age_mean))
v3
v4 <- varpred(fit, "age", at=list(age=df$age))
all.equal(v4$preds$age, df$age)
v4$preds$fit |> mean()
names(v4$preds)

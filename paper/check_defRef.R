library(shellpipes)
library(varpred)

loadEnvironments()
startGraphics()

summary(mod)

pred_anch <- varpred(mod, "nitro")
plot(pred_anch)

mean(pred_anch$preds$fit)
mean(dat$robustProp)

pred_anch <- varpred(mod, "nitro", bias.adjust="observed")
plot(pred_anch)

mean(pred_anch$preds$fit)
mean(dat$robustProp)

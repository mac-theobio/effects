library(vareffects)
library(dplyr)
library(ggplot2); theme_set(theme_bw(base_size=16))
## library(ggpubfigs)

library(shellpipes)
startGraphics()

dat <- rdsRead()

summary(dat)

mod <- lm(con~edu+income, data=dat)
mod <- lm(con~edu, data=dat)

points <- (ggplot(dat)
	+ aes(edu, con)
	+ geom_point()
	+ xlab("Education")
	+ ylab("Consumption")
)

vpred <- as.data.frame(varpred(mod, "edu", isolate=FALSE))
vpzero <- as.data.frame(varpred(mod, "edu", isolate=FALSE, isolate.value=0))
veff <- as.data.frame(varpred(mod, "edu"))
vzero <- as.data.frame(varpred(mod, "edu", isolate.value=0))

plain <- (points
	+ geom_line(data=vpred, aes(y=fit))
)

pred <- (plain
	+ geom_line(data=vpred, lty=2, aes(y=lwr))
	+ geom_line(data=vpred, lty=2, aes(y=upr))
)
teeGG(pred, desc="pred")

eff <- (plain
	+ geom_line(data=veff, lty=2, aes(y=lwr))
	+ geom_line(data=veff, lty=2, aes(y=upr))
)
teeGG(eff, desc="eff")

(pred
	+ geom_line(data=vpzero, lty=2, color="blue", aes(y=lwr))
	+ geom_line(data=vpzero, lty=2, color="blue", aes(y=upr))
) %>% teeGG(desc="pzero")

predeff <- (pred
	+ geom_line(data=veff, lty=2, aes(y=lwr))
	+ geom_line(data=veff, lty=2, aes(y=upr))
)
teeGG(predeff, desc="predeff")

(eff
	+ geom_line(data=vzero, lty=2, color="blue", aes(y=lwr))
	+ geom_line(data=vzero, lty=2, color="blue", aes(y=upr))
) %>% teeGG(desc="zero")

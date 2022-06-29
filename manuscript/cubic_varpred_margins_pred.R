library(shellpipes)
library(vareffects); varefftheme()
library(ggpubr)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(margins)
library(emmeans)
library(effects)

loadEnvironments()
startGraphics()


## marginal effect
m <- lm(hhsize ~I(age)+I(age^2)+I(age^3)+wealthindex, data=sim_df_cubic)
d1 <- cplot(m, "age", what="effect")
d2 <- data.frame(d1)
d2
ggplot(d2, aes(x=xvals, y=yvals)) +  geom_line() + geom_point(data=sim_df_cubic, aes(x=age, y=hhsize))
p1 <- varpred(m, "age", isolate=FALSE, at=list(age=d2$xvals), bias.adjust="none", returnall=TRUE)
plot(p1) + geom_point(data=sim_df_cubic, aes(x=age, y=hhsize))
m1 <- emmeans(m, specs=~age, at=list(age=d2$xvals), nesting=NULL)
as.data.frame(m1)
ef <- Effect("age", m, xlevels=list(age=d2$xvals))
p1
as.data.frame(ef)
head(p1$raw$model.matrix)
head(ef$model.matrix)
quit()
meffect_df <- cplot(mod_cubic, "age", what = "effect", draw=FALSE)
meffect_df

library(varpred)
library(dplyr)
library(ggplot2); theme_set(theme_bw(base_size=16))

library(shellpipes)

loadEnvironments()

objects()

vpred <- as.data.frame(varpred(mod, "nitro"))
## summary(vpred)

dat <- na.omit(dat)
dat$pred <- predict(mod)

## summary(dat)

pv <- data.frame(nitro=vpred$nitro
	, est=vpred$fit
)

pp <- data.frame(nitro=dat$nitro
	, est=dat$pred
)

pf <- bind_rows(varpred=pv, predict=pp, .id="method")

summary(pf)

p <- (ggplot(pf)
	+ aes(nitro, est, color=method)
	+ geom_point()
)

print(p)
print(p + facet_wrap(~method))

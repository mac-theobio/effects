library(shellpipes)
library(vareffects); varefftheme()
library(lme4)
library(effects)

loadEnvironments()

summary(poism)

## Predictor effects
isolate <- TRUE	# Set to FALSE to non-focal variance
offset <- NULL		# Set to 0 to ignore model offset
### group
group <- varpred(poism, "group", isolate=isolate, offset=offset)
print(group)
plot(group)

### group
wave <- varpred(poism, "wave", isolate=isolate, offset=offset)
plot(wave)

### interaction
wave_group <- varpred(poism, c("group", "wave"), x.var="wave", isolate=isolate, offset=offset)
plot(wave_group)

## What does effects give? Just a quick check
group_eff <- Effect("group", poism)
print(as.data.frame(group_eff))

plot(group_eff)

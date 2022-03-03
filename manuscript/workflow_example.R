library(shellpipes)
library(dplyr)
library(ggplot2)
library(vareffects)

loadEnvironments()
startGraphics()

## Data
N <- 100
b0 <- 10
b1 <- 0.6
b2 <- -0.8
df <- (data.frame(x1=rnorm(N), x2=rnorm(N))
	%>% mutate(NULL
		, y=b0 + b1*x1 + b2*x2 + rnorm(N)
	)
)

## Model
mod <- lm(y ~ x1 + x2, df)

## Binned observations
binned_df <- binfun(mod, focal="x1")
head(binned_df)

## basic varpred
v1 <- varpred(mod, "x1")
names(v1) # use v1$preds to extract df containing the predictions

## basic plot: ggplot object and add binned observations
p1 <- (plot(v1)
	+ geom_point(data=binned_df, aes(x=x1, y=y), colour="grey")
)
print(p1)

## Compare varpreds
v2_iso <- varpred(mod, "x2", isolate=TRUE, modelname="isolated")
v2_trad <- varpred(mod, "x2", isolate=FALSE, modelname="traditional")

## Generate binned obs for x2
binned_df <- binfun(mod, focal="x2")
head(binned_df)

## use comparevarpred with varpreds in a list; plotit=TRUE (default)
v2_all <- (comparevarpred(list(v2_iso, v2_trad), plotit=TRUE)
	+ geom_point(data=binned_df, aes(x=x2, y=y), colour="grey")
	+ labs(colour="Model", linetype="Model")
)
print(v2_all)

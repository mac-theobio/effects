library(shellpipes)
library(dplyr)
library(ggplot2)

rpcall("observed_approach.Rout observed_approach.pipestar observed_approach.R eco.interglm.glmfit.rda")

theme_set(theme_bw())

loadEnvironments()

startGraphics()

summary(mod)

##


## get RHS of formula

form <- formula(mod)[c(1,3)]

focal_var <- "nitro"
outcome_var <- "robustProp"
bins <- 10

## Setting focal values
probs <- seq(0, 1, length.out = 50)
focal_vals <- quantile(dat[[focal_var]], probs, names=FALSE)
print(focal_vals)
focal_df <- data.frame(focal_vals)
colnames(focal_df) <- focal_var

## Non-focal predictors
non_focal_df <- dat[, all.vars(form)[!all.vars(form) %in% focal_var]]
head(non_focal_df)

## For every focal value, assign a non-focal value
nM <- NROW(non_focal_df)
#focal_df <- focal_df[rep(1:NROW(focal_df), each=nM),,drop=FALSE]

## Generate new model frame
mf <- expand.grid(as.list(c(focal_df, non_focal_df)))
#mf <- cbind.data.frame(focal_df, non_focal_df)
head(mf)

## Generate model matrix
mm <- model.matrix(form, mf)

typical <- mean
col_mean <- apply(mm, 2, typical)
center_mean <- colMeans(mm)
mm <- sweep(mm, 2, col_mean, FUN="/")
mm <- sweep(mm, 2, center_mean, FUN="*")

## Generate predictions
beta <- coef(mod)
pred <- as.vector(mm %*% beta)

## Pass through the inverse link function and then average
pred_df <- (mf
	%>% mutate(fit = plogis(pred))
	%>% group_by_at(focal_var)
	%>% summarise(fit = mean(fit))
)
head(pred_df)
mean(pred_df$fit)
mean(dat[[outcome_var]])

## Binned data
bin_df <- (dat
	%>% arrange_at(focal_var)
	%>% mutate(bin=ceiling(row_number()*bins/nrow(.)))
	%>% group_by(bin)
	%>% summarise_all(mean)
)


p <- (ggplot(pred_df, aes(x=nitro, y=fit))
	+ geom_line()
	+ geom_point(aes(x=mean(nitro), y=mean(fit)), size=3, colour="red")
	+ geom_point(data=bin_df, aes(x=nitro, y=robustProp), colour="grey")
	+ geom_point(data=dat, aes(x=mean(nitro), y=mean(robustProp)), size=3, colour="black")
)
print(p)


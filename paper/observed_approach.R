library(shellpipes)
library(dplyr)

loadEnvironments()

summary(mod)

## get RHS of formula

form <- formula(mod)[c(1,3)]

focal_var <- "nitro"
outcome_var <- "robustProp"

## Setting focal values
probs <- seq(0, 1, length.out = 25)
focal_vals <- quantile(dat[[focal_var]], probs, names=FALSE)
print(focal_vals)
focal_df <- data.frame(focal_vals)
colnames(focal_df) <- focal_var

## Non-focal predictors
non_focal_df <- dat[, colnames(dat)[!colnames(dat) %in% c(focal_var, outcome_var)]]
head(non_focal_df)

## For every focal value, assign a non-focal value
nM <- NROW(non_focal_df)
focal_df <- focal_df[rep(1:NROW(focal_df), each=nM),,drop=FALSE]

## Generate new model frame
mf <- cbind.data.frame(focal_df, non_focal_df)
head(mf)

## Generate model matrix
mm <- model.matrix(form, mf)

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


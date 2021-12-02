library(dplyr)
library(jdeffects)
library(shellpipes)

loadEnvironments()

## Predict function. Calls customized base R predict
predfun <- function(mod, var, newdata, model, level = 0.95, vvfun=NULL){
	pred <- cpredict(mod, newdata, level, vvfun)
	pred$x <- newdata[[var]]
	pred$model <- model
	return(pred)
}

# Marginal effect of x1

## Unscaled predictions
### Unzeroed vv
x1u_base <- predfun(mod_unscaled
	, var = "x1", newdata = pred_df_x1
	, mode = "base_u"
)
head(x1u_base)

### vv for non-focal predictors zeroed out
x1u_base_zero <- predfun(mod_unscaled
	, var = "x1", newdata = pred_df_x1
	, mode = "base_u_zero"
	, vvfun = zero_vcov(mod_unscaled, "x1")
)
head(x1u_base_zero)

## Scaled
### Unzeroed vv
x1std_base <- predfun(mod_scaled
	, var = "x1std", newdata = pred_df_x1std
	, mode = "base_s"
)
head(x1std_base)

### Non-focal vv zeroed out
x1std_base_zero <- predfun(mod_scaled
	, var = "x1std", newdata = pred_df_x1std
	, mode = "base_s_zero"
	, vvfun = zero_vcov(mod_scaled, "x1std")
)
head(x1std_base_zero)

## Merge the predicitons for each predictor
predict_x1_base <- bind_rows(list(x1u_base, x1u_base_zero, x1std_base, x1std_base_zero))
head(predict_x1_base)
rownames(predict_x1_base) <- NULL
saveVars(predict_x1_base)



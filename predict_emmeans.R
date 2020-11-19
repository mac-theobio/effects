library(dplyr)
library(data.table)
library(emmeans)

source("makestuff/makeRfuns.R")
commandEnvironments()
sourceFiles()

# Unzeroed vcov
######################################################################

# Marginal effect of x1
## Unscaled predictions
predict_x1u_em <- emmeans(mod_unscaled, ~x1, at = list(x1 = pred_df_x1$x1), cov.keep = "x1", CIs = TRUE)
predict_x1u_em <- as.data.frame(predict_x1u_em)
predict_x1u_em$model <- "em_u"
predict_x1u_em$x <-  pred_df_x1$x1
head(predict_x1u_em)

## Scaled
predict_x1s_em <- emmeans(mod_scaled, ~x1s, at = list(x1s = pred_df_x1s$x1s), cov.keep = "x1s", CIs = TRUE)
predict_x1s_em <- as.data.frame(predict_x1s_em)
predict_x1s_em$model <- "em_s"
predict_x1s_em$x <-  pred_df_x1s$x1s
head(predict_x1s_em)

# Marginal effect of x2
## Unscaled predictions
predict_x2u_em <- emmeans(mod_unscaled, ~x2, at = list(x2 = pred_df_x2$x2), cov.keep = "x2", CIs = TRUE)
predict_x2u_em <- as.data.frame(predict_x2u_em)
predict_x2u_em$model <- "em_u"
predict_x2u_em$x <-  pred_df_x2$x2
head(predict_x2u_em)

## Scaled
predict_x2s_em <- emmeans(mod_scaled, ~x2s, at = list(x2s = pred_df_x2s$x2s), cov.keep = "x2s", CIs = TRUE)
predict_x2s_em <- as.data.frame(predict_x2s_em)
predict_x2s_em$model <- "em_s"
predict_x2s_em$x <-  pred_df_x2s$x2s
head(predict_x2s_em)

# Zero out all vcov except focal effects
######################################################################

# Marginal effect of x1
## Unscaled predictions
predict_x1u_em_zero <- emmeans(mod_unscaled, ~x1, at = list(x1 = pred_df_x1$x1)
	, cov.keep = "x1", CIs = TRUE, vcov. = zero_vcov(mod_scaled, "x1")
)
predict_x1u_em_zero <- as.data.frame(predict_x1u_em_zero)
predict_x1u_em_zero$model <- "em_u_zero"
predict_x1u_em_zero$x <-  pred_df_x1$x1
head(predict_x1u_em_zero)

## Scaled predictions
predict_x1s_em_zero <- emmeans(mod_scaled, ~x1s, at = list(x1s = pred_df_x1s$x1s)
	, cov.keep = "x1s", CIs = TRUE, vcov. = zero_vcov(mod_scaled, "x1s")
)
predict_x1s_em_zero <- as.data.frame(predict_x1s_em_zero)
predict_x1s_em_zero$model <- "em_s_zero"
predict_x1s_em_zero$x <-  pred_df_x1s$x1s
head(predict_x1s_em_zero)

# Marginal effect of x2
## Unscaled predictions
predict_x2u_em_zero <- emmeans(mod_unscaled, ~x2, at = list(x2 = pred_df_x2$x2)
	, cov.keep = "x2", CIs = TRUE, vcov. = zero_vcov(mod_scaled, "x2")
)
predict_x2u_em_zero <- as.data.frame(predict_x2u_em_zero)
predict_x2u_em_zero$model <- "em_u_zero"
predict_x2u_em_zero$x <-  pred_df_x2$x2
head(predict_x2u_em_zero)

## Scaled predictions
predict_x2s_em_zero <- emmeans(mod_scaled, ~x2s, at = list(x2s = pred_df_x2s$x2s)
	, cov.keep = "x2s", CIs = TRUE, vcov. = zero_vcov(mod_scaled, "x2s")
)
predict_x2s_em_zero <- as.data.frame(predict_x2s_em_zero)
predict_x2s_em_zero$model <- "em_s_zero"
predict_x2s_em_zero$x <-  pred_df_x2s$x2s
head(predict_x2s_em_zero)

######################################################################

## Combine the predictions
predict_x1_em <- (list(predict_x1u_em, predict_x1s_em, predict_x1u_em_zero, predict_x1s_em_zero)
	%>% bind_rows()
	%>% setnames(old = c("emmean", "lower.CL", "upper.CL"), new = c("fit", "lwr", "upr"))
)
head(predict_x1_em)

predict_x2_em <- (list(predict_x2u_em, predict_x2s_em, predict_x2u_em_zero, predict_x2s_em_zero)
	%>% bind_rows()
	%>% setnames(old = c("emmean", "lower.CL", "upper.CL"), new = c("fit", "lwr", "upr"))
)
head(predict_x2_em)

saveVars(predict_x1_em
	, predict_x2_em
)



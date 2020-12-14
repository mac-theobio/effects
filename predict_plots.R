library(dplyr)
library(ggplot2); theme_set(theme_bw())

source("makestuff/makeRfuns.R")
commandEnvironments()
sourceFiles()
makeGraphics()

# x1 marginal effects
pred_x1 <- (bind_rows(predict_x1_base, predict_x1_em, predict_x1_jd)
	%>% mutate(trans = ifelse(grepl("_u", model), "unscaled", "scaled")
		, model = gsub("_u|_s", "", model)
	)
)
head(pred_x1)
table(pred_x1$trans)
table(pred_x1$model)

x1_plot <- (ggplot(pred_x1, aes(x = x, group = trans))
	+ geom_line(aes(y = fit, colour = trans), alpha = 0.7)
	+ geom_ribbon(aes(ymin = lwr, ymax = upr, fill = trans)
		, colour = NA, alpha = 0.7
	)
	+ scale_colour_manual(values = c("unscaled" = "red", "scaled" = "blue"))
	+ scale_fill_manual(values = c("unscaled" = "red", "scaled" = "blue"))
	+ labs(x = "x1", y = "Predictions", fill = "Data", colour = "Data")
	+ facet_wrap(~model, scales = "free_x")
)
print(x1_plot)


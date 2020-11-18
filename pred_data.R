source("makestuff/makeRfuns.R")
commandEnvironments()
sourceFiles()

## Marginal for x1
### Unscaled
x1_range <- range(sim_df$x1)
pred_df_x1 <- expand.grid(
	x1 = seq(x1_range[1], x1_range[2], length.out = 50)
	, x2 = mean(sim_df$x2)
)
head(pred_df_x1)

### Scaled
x1s_range <- range(sim_df$x1s)
pred_df_x1s <- expand.grid(
	x1s = seq(x1s_range[1], x1s_range[2], length.out = 50)
	, x2s = mean(sim_df$x2s)
)
head(pred_df_x1s)

## Marginal for x2
### Unscaled
x2_range <- range(sim_df$x2)
pred_df_x2 <- expand.grid(
	x1 = mean(sim_df$x1)
	, x2 = seq(x2_range[1], x2_range[2], length.out = 50)
)
head(pred_df_x2)

### Scaled
x2s_range <- range(sim_df$x2s)
pred_df_x2s <- expand.grid(
	x1s = mean(sim_df$x1s)
	, x2s = seq(x2s_range[1], x2s_range[2], length.out = 50)
)
head(pred_df_x2s)

saveVars(pred_df_x1, pred_df_x1s, pred_df_x2, pred_df_x2s)

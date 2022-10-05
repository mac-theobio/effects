library(shellpipes)
library(dplyr)

loadEnvironments()

bin_df <- binfun(fit, "age", bins=10)
head(bin_df)

saveVars(bin_df)


library(dplyr)
library(data.table)
library(emmeans)

source("makestuff/makeRfuns.R")
commandEnvironments()
sourceFiles()

# Marginal effect of x1
## emmeans
x1_em <- data.frame(emmeans(inter_mod, spec = ~x1:x2, cov.keep="x1"))
head(x1_em)

## varpred
x1_jd <- varpred(inter_mod, "x1")
head(x1_jd)

# Marginal effect of x2
## emmeans
x2_em <- data.frame(emmeans(inter_mod, spec = ~x1:x2, cov.keep="x2"))
head(x2_em)

## varpred
x2_jd <- varpred(inter_mod, "x2")
head(x2_jd)


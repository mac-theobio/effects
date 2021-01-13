library(dplyr)
library(data.table)
library(emmeans)

source("makestuff/makeRfuns.R")
callArgs <- "inter_predict.Rout inter_predict.R effectsfuns.R inter_mod_lm.rda"
commandEnvironments()
sourceFiles()


# Marginal effect of x1
## emmeans
x1_em <- data.frame(emmeans(inter_mod, spec = ~x1:x2, cov.keep="x1"))
head(x1_em)
library(ggplot2)
ggplot(x1_em, aes(x1, emmean, colour=x2)) + geom_line()

em0 <- emmeans(inter_mod,
               specs = "x1",
               cov.keep="x1",
               ## average across factor effects
               nesting=NULL 
               )

emmip(em0, ~x1)
x1_em0 <- data.frame(em0)
ggplot(x1_em0, aes(x1, emmean)) + geom_line()

## varpred
x1_jd <- varpred(inter_mod, "x1")
head(x1_jd)

## want prediction

## make the reference grid
mf <- model.frame(inter_mod)
dd <- expand.grid(x2=levels(mf$x2),
                  x1=seq(min(mf$x1), max(mf$x1),
                         length=11))

## predict over the reference grid
pp <- predict(inter_mod, data=dd)

## average over the reference grid
## (unweighted arithmetic mean)
marg_means <- tapply(pp, mf$x1, mean)


## special cases:
## if you're already using sum-to-zero contrasts,
## then setting the coefficient for the factor to zero
## (and then applying X %*% beta) will marginalize
## properly: by definition, the baseline value is
## the unweighted mean across levels

## https://bbolker.github.io/stat4c03/notes/contrasts.pdf
## https://bbolker.github.io/mixedmodels-misc/notes/contrasts.pdf
## Schad, Daniel J., Sven Hohenstein, Shravan Vasishth, and Reinhold Kliegl. “How to Capitalize on a Priori Contrasts in Linear (Mixed) Models: A Tutorial.” ArXiv:1807.10451 [Stat], July 27, 2018. http://arxiv.org/abs/1807.10451.

## model fitted with scaled predictors:
##  we want to be able to specify an *unscaled* reference grid and get the right predictions?
##  (we want to have it automatically pick a sensible reference grid, unscale it, and give us the
##  unscaled ref grid and the relevant predictions)

## suppose we have a model whose predictors have been scaled by F()
## if we are happy with emmeans' choice of reference grid (which will be based on the ranges and levels
## of the predictors)
## then all we need to be able to do it to apply F^{-1} to the reference grid (predictors) that emmeans returns

set.seed(101)
dd_unsc <- expand.grid(x1=rnorm(20,mean=5,sd=10), x2 = factor(c("A","B")))
X <- model.matrix(~x1*x2, dd_unsc)
beta0 <- c(-5, 0.1, 1, -0.05)
dd_unsc$y <- rnorm(nrow(dd_unsc), mean= X %*% beta0, sd=1)
## c() drops all attributes
## drop() drops length-1 array dimensions
dd_sc <- transform(dd_unsc, x1=drop(scale(x1)))
lm_unsc <- lm(y~x1*x2, dd_unsc)
lm_sc <- lm(y~x1*x2, dd_sc)

cbind(unsc=coef(lm_unsc), sc=coef(lm_sc))
dd2 <- as.data.frame(emmeans(lm_sc, ~x1,
                             nesting=NULL,
                             cov.keep="x1"))

scp <- unlist(attributes(dd_sc$x1))
dd2_resc <- transform(dd2,
                      x1=x1*scp[["scaled:scale"]]+
                          scp[["scaled:center"]])
dd3 <- as.data.frame(emmeans(lm_unsc, ~x1,
                             nesting=NULL,
                             cov.keep="x1"))
all.equal(c(dd2_resc),c(dd3))

# Marginal effect of x2
## emmeans
x2_em <- data.frame(emmeans(inter_mod, spec = ~x1:x2, cov.keep="x2"))
head(x2_em)
ggplot(x2_em, aes(x2, emmean)) + geom_point()

## varpred
x2_jd <- varpred(inter_mod, "x2")
head(x2_jd)


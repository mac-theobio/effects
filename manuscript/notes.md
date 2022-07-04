What have we decided? 

BC> I think we chose to go with the conventional definition by Gelman until we convince Ben otherwise

It would be great to be able to save predictor for the logical sense. Maybe use model variable for the columns of the model matrix?

I sort of think that "input" and "model" variable is a distinction that could in theory be applied to an outcome variable, or to a variable we are ordinating, which could be another reason to avoid "predictor" to mean the column-associated variable

BC> I agree with you that associating predictor with columns of model matrix is confusing but maybe it is not confusing for others. The challenge would be having to convince people like Ben and Gelman why we want to introduce a new terminology, I think.

I am rewriting the intro because I was unable to completely follow the logic. For example, the previous draft says that non-focal choices are needed _because_ glms can be non-linear. In fact non-focal choices are always needed. It then talks unnecessarily (and confusingly, I think, or at least prematurely) about "conclusions" depending on non-focal choices.

We need a word to talk generically about the plots (outcome plots?), and one to talk about the central estimate (outcome estimate?)

MS says: If the link function is nonlinear and/or the model has complex higher order interactions, the average of the predictions evaluated at the mean of non-focal predictors and the average of predictions evaluated at the population level (and then averaged) of non-focal predictor are not equivalent… This seems wrong. I don't know what higher-order interactions have to do with it in our framework.

Do we have an uncertainty method for the whole-sample based estimate?

I think it's very common to choose a non-focal reference point as the average of values across levels, rather than observations, for a categorical variable. In other words, to use a focal point of 1/3 Jews, 1/3 Muslims, 1/3 Christian (the "sum-to-zero" approach). The model-center approach would use the observed proportions.

focal predictors

multi-parameter variables?

higher-order (interactions vs. MPVs)

----------------------------------------------------------------------

2022 Jul 04 (Mon)
=================

We will use "input variable" sensu Gelman, and "model variable" for a variable that corresponds to a column of a model matrix. We will use predictor loosely and carefully avoid use of the term "predictor variable".

We need to worry about justifying our whole-population CIs (empirically, theoretically, or through a Gaussian conceptualization).

We need a word to talk generically about the plots (outcome plots), and one to talk about the central estimate (central estimate).

Bicko is in fact using a range of quantiles, not a range of values -- is it better? Let's use the quantiles for now; maybe later we could add equal spacing. Bicko will take a look at ppoints (but it may not be necessary to exclude min and max in this case).
* See also https://cran.r-project.org/web/packages/emmeans/vignettes/messy-data.html#weights

Avoid "higher order"
* interactions are interactions
* multi-parameter variables are multi-parameter variables

Talk about MPVs first
* non-focal interactions are then similar to non-focal MPVs
* interactions that involve the focal predictor are hard (maybe deal with them later).

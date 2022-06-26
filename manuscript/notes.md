What have we decided? It would be great to be able to save predictor for the logical sense. Maybe use model variable for the columns of the model matrix?

I sort of think that "input" and "model" variable is a distinction that could in theory be applied to an outcome variable, or to a variable we are ordinating, which could be another reason to avoid "predictor" to mean the column-associated variable

I am rewriting the intro because I was unable to completely follow the logic. For example, the previous draft says that non-focal choices are needed _because_ glms can be non-linear. In fact non-focal choices are always needed. It then talks unnecessarily (and confusingly, I think, or at least prematurely) about "conclusions" depending on non-focal choices.

We need a word to talk generically about the plots (outcome plots?), and one to talk about the central estimate (outcome estimate?)

MS says: If the link function is nonlinear and/or the model has complex higher order interactions, the average of the predictions evaluated at the mean of non-focal predictors and the average of predictions evaluated at the population level (and then averaged) of non-focal predictor are not equivalent… This seems wrong. I don't know what higher-order interactions have to do with it in our framework.

Do we have an uncertainty method for the whole-sample based estimate?

I think it's very common to choose a non-focal reference point as the average of values across levels, rather than observations, for a categorical variable. In other words, to use a focal point of 1/3 Jews, 1/3 Muslims, 1/3 Christian. The model-center approach would use the observed proportions.

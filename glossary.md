

**Marginal predictions** - Predictions based on models that directly predict the population average (marginal mean). Usually produced from fixed-effect models.

**Conditional predictions** - These predictions are usually derived from conditional models (mixed-effect) with some values of the random parameters [de-Miguel et. al](https://doi.org/10.1139/x2012-090).

**Marginal vs conditional predictions** - The main distinction between marginal and conditional predictions depends on whether the resulting predictions describe the individual's or the population-average response to changing covariates. In particular, conditional predictions results from conditional models which describe how the change in one covariate affects the outcome within a particular group, while holding the other covariates and all random effects constant. On the other hand, marginal predictions result from population-averaged models and are appropriate when predictions on the population-level is desired, irrespective of potential intergroup differences. For example marginal prediction for gender compares the mean of women to that of female, while conditional prediction compares the mean among female to that of women holding the same value that corresponds to each individual (random effect).

**Simple linear models case** - For a simple linear model, the choice between a marginal and conditional formulation is equivalent [Stefanie Muff  Leonhard Held  Lukas F. Keller](https://doi.org/10.1111/2041-210X.12623).

**Focal predictor** - A predictor whose predictions are computed, otherwise _non-focal_.

**Zeroing-out variance-covariance matrix** - Eliminating uncertainties due to non-focal predictors.

**Variables vs model matrix** - Transformations (centering, scaling or both, or averaging) can be applied to either the observed variables or to the columns of the generated model matrix. If all the variables are continuous, transforming the observed variables is straightforward otherwise, in the presence of categorical variables, we need to first compute class proportions (yet to figure this out). Current implementation uses model matrix.


## References 

- [Sergio de-Miguel, Lauri Mehtätalo, Zuheir Shater, Bassel Kraid, and Timo Pukkala](https://doi.org/10.1139/x2012-090)

- [Stefanie Muff  Leonhard Held  Lukas F. Keller](https://doi.org/10.1111/2041-210X.12623)

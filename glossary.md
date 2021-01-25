

**Marginal predictions** - Predictions based on models that directly predict the population average (marginal mean). Usually produced from fixed-effect models.
**Conditional predictions** - These predictions are usually derived from conditional models (mixed-effect) with some values of the random parameters \cite{de2012evaluating}.
**Marginal vs conditional predictions** - The main distinction between marginal and conditional predictions depends on whether the resulting predictions describe the individual's or the population-average response to changing covariates. In particular, conditional predictions results from conditional models which describe how the change in one covariate affects the outcome within a particular group, while holding the other covariates and all random effects constant. On the other hand, marginal predictions result from population-averaged models and are appropriate when predictions on the population-level is desired, irrespective of potential intergroup differences.


For example marginal prediction for gender compares the mean of women to that of female, while conditional prediction compares the mean among female to that of women holding the same value that corresponds to each individual (random effect).

**Case in simple linear model** - For a simple linear model, the choice between a marginal and conditional formulation is equivalent \cite{muff2016marginal}.




## References 

@article{de2012evaluating,
  title={Evaluating marginal and conditional predictions of taper models in the absence of calibration data},
  author={de-Miguel, Sergio and Meht{\"a}talo, Lauri and Shater, Zuheir and Kraid, Bassel and Pukkala, Timo},
  journal={Canadian Journal of Forest Research},
  volume={42},
  number={7},
  pages={1383--1394},
  year={2012},
  publisher={NRC Research Press}
}

@article{muff2016marginal,
  title={Marginal or conditional regression models for correlated non-normal data?},
  author={Muff, Stefanie and Held, Leonhard and Keller, Lukas F},
  journal={Methods in Ecology and Evolution},
  volume={7},
  number={12},
  pages={1514--1524},
  year={2016},
  publisher={Wiley Online Library}
}

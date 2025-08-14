
<!-- README.md is generated from README.Rmd. Please edit that file -->

# senseweight

<!-- badges: start -->

[![R-CMD-check](https://github.com/melodyyhuang/senseweight/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/melodyyhuang/senseweight/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

`senseweight` implements a set of sensitivity functions and tools to
help researchers transparently conduct sensitivity analyses for weighted
estimators. `senseweight` allows researchers to assess the sensitivity
present in their weighted estimates to omitted confounders. Specific
methods provided in `senseweight` include the following: (1)
visualization tools to summarize sensitivity; (2) summary tables
containing necessary sensitivity statistics; (3) formal benchmarking
methods which allow researchers to use observed covariates to assess the
plausibility of different confounders.

## Installation

You can install the development version of senseweight from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("melodyyhuang/senseweight")
```

## References

The package implements a series of methods developed in the following
papers.

For the technical introduction of the sensitivity tools:

- [Huang, Melody. “Sensitivity Analysis in the Generalization of
  Experimental Results.” Journal of the Royal Statistical Society Series
  A: Statistics in Society
  (2024)](https://academic.oup.com/jrsssa/advance-article-abstract/doi/10.1093/jrsssa/qnae012/7626119)
- [Hartman, Erin and Huang, Melody. “Sensitivity Analysis for Survey
  Weights.” Political Analysis
  (2024)](https://www.cambridge.org/core/journals/political-analysis/article/sensitivity-analysis-for-survey-weights/0A13E3843155099F169CF195B8D7604F)

For less technical introductions with interesting applications and best
practice:

- Huang, Melody and Hartman, Erin. “Assessing Nonignorable Response:
  Sensitivity Analysis for Survey Weighting, with Applications to Survey
  Estimates of COVID-19 Vaccination Uptake.” Working paper.
- Bailey, Michael. “Polling at a Crossroads.” (Chapter 7)

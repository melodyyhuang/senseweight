
<!-- README.md is generated from README.Rmd. Please edit that file -->

# senseweight

<!-- badges: start -->
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

## Citation

[Huang, Melody. “Sensitivity Analysis in the Generalization of
Experimental Results.” arXiv preprint arXiv:2202.03408
(2022).](https://arxiv.org/abs/2202.03408)

## Basic Usage

The example below illustrates how to use the `senseweight` package for
external validity. Examples of how to use `senseweight` for internal
validity or survey weighting are forthcoming.

``` r
library(senseweight)

#Load in JTPA data: 
data(jtpa_women)
```

``` r
#Summarize sites
jtpa_women %>%
    group_by(site) %>%
        summarize(
            length(prevearn),
            mean(prevearn),
            mean(age),
            mean(married),
            mean(hrwage),
            mean(black),
            mean(hispanic),
            mean(hsorged),
            mean(yrs_educ)
        )
#> # A tibble: 16 x 10
#>    site  `length(prevearn)` `mean(prevearn)` `mean(age)` `mean(married)`
#>    <chr>              <int>            <dbl>       <dbl>           <dbl>
#>  1 CC                   524            1855.        32.1          0.219 
#>  2 CI                   190            2250.        33.5          0.253 
#>  3 CV                   788            2192.        33.6          0.278 
#>  4 HF                   234            1997.        31.6          0.184 
#>  5 IN                  1392            3172.        34.9          0.193 
#>  6 JC                    81            2564.        30.6          0.136 
#>  7 JK                   353            1928.        30.0          0.113 
#>  8 LC                   485            3039.        33.9          0.258 
#>  9 MD                   177            2915.        34.6          0.181 
#> 10 MN                   179            2215.        37.6          0.352 
#> 11 MT                    38            1680.        33.8          0.395 
#> 12 NE                   636            2161.        31.7          0.0975
#> 13 OH                    74            2568.        34.6          0.324 
#> 14 OK                    87            2320.        37.3          0.126 
#> 15 PR                   463            1783.        32.8          0.0842
#> 16 SM                   401            2997.        32.2          0.284 
#> # … with 5 more variables: mean(hrwage) <dbl>, mean(black) <dbl>,
#> #   mean(hispanic) <dbl>, mean(hsorged) <dbl>, mean(yrs_educ) <dbl>
```

Assume researchers are interested in generalizing the results from the
site of Omaha, Nebraska to the other 15 experimental sites:

``` r
site_name="NE"
df_site = jtpa_women[which(jtpa_women$site == site_name),]
df_else = jtpa_women[which(jtpa_women$site != site_name),]

#Estimate unweighted estimator: 
model_dim = estimatr::lm_robust(Y~T, data = df_site)
PATE = coef(lm(Y~T, data = df_else))[2]
DiM = coef(model_dim)[2]

#Generate weights using observed covariates:
df_all = jtpa_women
df_all$S = ifelse(jtpa_women$site == "NE", 1, 0)
model_ps = WeightIt::weightit((1-S)~.-site-T-Y, data = df_all, method='ebal', estimand="ATT")
weights = model_ps$weights[df_all$S==1]

#Estimate IPW model: 
model_ipw = estimatr::lm_robust(Y~T, data = df_site, weights=weights)
ipw=coef(model_ipw)[2]

#Estimate bound for var(tau):
m = sqrt(var(df_site$Y[df_site$T==1])/var(df_site$Y[df_site$T==0]))
#Since m > 1:
vartau = var(df_site$Y[df_site$T==1])-var(df_site$Y[df_site$T==0])
```

### Sensitivity Summary Measures

We can generate the sensitivity summary measures using the
`summarize_sensitivity` function:

``` r
summarize_sensitivity(weights=weights, Y=df_site$Y, Z=df_site$T, sigma2 = vartau, estimand='PATE')
#>   Unweighted Estimate      SE   RV sigma_tau_bound cor_w
#> Z    1107.35  1356.89 1417.21 0.36          2897.9  0.07
```

The `summarize_sensitivity` function defaults to evaluating the
robustness value at `q=1`, indicating a robustness value, relative to a
bias equal to the point estimate. Researchers can specify different
values for `q` in the function. In the generalization setting,
researchers can modify the `sigma2` bound and posit their own values for
a plausible bound (given substantive justification). With no
specification, `sigma2` will be automatically calculated to be bound by
`var(Y(1)) + var(Y(0))`.

Individual components of the sensitivity summaries can be computed as
well:

``` r
#Calculate robustness value:
RV = robustness_value(q=1, ipw, vartau, weights)
print(RV)
#> [1] 0.4114544
```

### Formal Benchmarking:

``` r
#Select weighting variables: 
weighting_vars = names(df_all)[which(!names(df_all) %in% c("site", "S", "Y", "T"))]

#Run bechmarking: 
df_benchmark = run_benchmarking(weighting_vars, data = df_all[,-1], 
                 treatment="T", outcome = "Y",selection = "S", 
                 estimate=ipw, 
                 RV = RV, sigma2=vartau, 
                 estimand = "PATE")

print(df_benchmark)
#>   variable R2_benchmark rho_benchmark    bias   MRCS k_sigma_min k_rho_min
#> 1 prevearn         0.04          0.59  310.90   4.36       10.01      1.08
#> 2      age         0.06          0.75  479.06   2.83        6.91      0.85
#> 3  married         0.11          0.19  170.96   7.94        3.83      3.29
#> 4   hrwage         0.05         -0.42 -244.46  -5.55        8.34     -1.51
#> 5    black         0.20         -0.49 -628.02  -2.16        2.03     -1.30
#> 6 hispanic         0.14         -0.10  -96.86 -14.01        3.02     -6.66
#> 7  hsorged         0.12          0.08   74.17  18.29        3.50      7.98
#> 8 yrs_educ         0.00          0.27   21.60  62.83      403.43      2.40
```

### Generating the Bias Contour Plots

``` r
contour_plot(var(weights), vartau, ipw, df_benchmark, benchmark=TRUE, shade=TRUE,
    shade_var = c("age", "prevearn"))+
    geom_point(aes(x = RV, y=sqrt(RV))) +
    annotate("text", x = RV-0.01, y = sqrt(RV)+0.02, 
        label=expression(RV[1]*"= 0.41"), hjust=0, vjust=0, size=3)
```

<img src="man/figures/README-contour_plot-1.png" width="100%" />

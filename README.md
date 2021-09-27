# fmmr6
Finite Mixture Modelling (FMM) based on the R6 object system.

## Installation

Install and test this ongoing package from Github:

``` r
remotes::install_github("wudongjie/fmmr6")
```

## Example
### Use `fmglm` to Fit Finite Mixture of Generalized Linear Models:

```
model1 <- fmglm$new(formula1, data, family="gaussian", latent=2)
result <- model1$fit()
output <- model1$summarize()
```

or

```
result <- fmglm$new(formula1, data, family="gaussian", latent=2)$fit()$summarize()
```

### Use `fmglm` to Fit Finite Mixture of Multinomial Regression Models:

```
model_mn <- fmglm$new(formula, data, family="multinom",
                      latent=2, method="em", glm_fit=F, use_llc=T)
```

or

```
model_mn <- fmglm$new(formula, data, family="multinom",
                      latent=2, method="em", glm_fit=T, use_llc=F)
```




## TODO
`fmcat`: Finite Mixture of Categorical Data 
(e.g. the log-multiplicative uniform difference model (unidiff), the log-multiplicative layer effect model)

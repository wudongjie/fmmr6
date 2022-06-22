# fmmr6
Finite Mixture Modelling (FMM) based on the R6 object system.

## Introduction

The `fmmr6` package is a R package for implementing modeling based on finite mixture distributions. It adopts R6 – an Object-Oriented system in R which encapsulates data and method into an object like other OO programming language such as C and Java. Although there are alternative packages and software available for FMM, these packages have limited choices on models (e.g., rarely used models or self-defined models) and estimation methods.
This package is originally designed to fit the finite mixture of "log-multiplicative layer effect" models. It is designed in a way that it can be easily extended to other models. It adopts a flexible and extendable structure with more choices on models and estimation methods. The main features are listed below:

### Flexibility:  

Finite mixture modelling can be used with different models and analyses (e.g., Generalized linear model, Categorical data analysis, etc.). It could also provide a way to estimate a user-defined model.  

### Extendibility: 

On the current version, the fmmr6 package uses the EM-algorithms to estimate the mixture models. On the R6 system, these methods are encapsulated in separated objects so that adding another method (e.g., MCMC, Bayesian methods) would not affect old methods.

### User Friendly Interface: 

Using and implementing fmmr6 with different choices of models and data structures is as easy as a single line of code. See the example below.



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
1. Add the concomitant variable models. 


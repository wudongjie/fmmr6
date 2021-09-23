#' @section Introduction of `fmglm` class:
#' The `fmglm` class is designed to fit the finite mixture of generalized linear 
#' models including the multinomial regression models. 
#' It contains three major methods:
#' * `$new()` creates a `fmglm` object given `data` and `formula`
#' * `$fit()` fits the `fmglm` model. The algorithm used to fit and 
#' the starting method can also be specified in the method. In addition,
#' this method returns a result list containing coefficients, loglikelihood value,
#' and information criterion such as AIC and BIC.
#' * `$summarize()` generates the table output of the results including
#' the standard errors and p-values.
#' 
#' @section The Normal Likelihood Function Vs. The Complete-Data Likelihood Function: 
#' The Finite Mixture Model (FMM) with \eqn{K} components can be written in the following form:
#' \deqn{f(y|x, \phi) = \sum_{k=1}^K{\pi_k f_k(y|x, \theta_k)}}
#' 
#' Based on the above equation, the log-likelihood function is:
#' \deqn{\log(L) = \sum_{i=1}^N \log{f(y_i|x_i, \phi)} = \sum_{i=1}^N \log {\sum_{k=1}^K{\pi_k f_k(y_i|x_i, \theta_k)}}}
#' 
#' The log-likelihood function can be maximized separately for each component in the M-step, 
#' given the posterior probabilities as weights: 
#' \deqn{\max_{\theta_k} \sum_{i=1}^N \hat{p_{ik}} \log f_k(y_i|x_i, \theta_k)}
#' 
#' where \eqn{p_{ik}} is the posterior probabilty estimated in the E-step.
#' This approach is used in other packages, such as \CRANpkg{flexmix}. 
#' 
#' Similar to \CRANpkg{flexmix}, we use `glm.fit()` and `lm.wfit()`, to fit the log-likelihood function by components.
#' To use it, set `glm_fit` to `TRUE` and `use_llc` to `FALSE`.
#' 
#' Alternative, FMM can be viewed as a model with incomplete data where the variable to determine individual's class is missing.
#' The imputed variable \eqn{z_ik = 1} or {0} captures the classification of each sample. 
#' Therefore, the complete-data log-likelihood is:
#' \deqn{\log L_c = \sum_{k=1}^K \sum_{i=1}^N z_ik \{ \log{\pi} + \log{f_k(y_i|x_i, \theta_k)} \} }
#' 
#' In this package, we use `optim` with the L-BFGS-B algorithm to maximize the complete-data log-likelihood. 
#' And this method is the default method to fit FMM. 
#' 
#' @section Alternative Algorithm to EM-algorithm:
#' 
#' In addition to the normal EM-algorithm, `$fit()` can also choose two extended algorithms of the EM-algorithm.
#' 
#' * The Classification EM (`cem`) assigns each sample to a component based on the maximum value of its posterior probabilities.
#' * The Stocastic EM (`sem`) randomly assigns each sample to a component based on its posterior probabilities. 
#' 
#' 
#' 
#' @section Use `fmglm` to Fit Finite Mixture of Generalized Linear Models:
#' The main practice of `fmglm` is to fit finite mixture of generalized linear models 
#' such as linear regressions with Gaussian distributions or Poisson distribution. 
#' To fit a linear regression with Gaussian distribution, run the code similar to the following:
#' 
#' ```
#' model1 <- fmglm$new(formula1, data, family="gaussian", latent=2)
#' result <- model1$fit()
#' output <- model1$summarize()
#' ```
#' 
#' Benefit from the chain feature of R6, the code can be written in one line like the following:
#' ```
#' result <-fmglm$new(formula1, data, family="gaussian", latent=2)$fit()$summarize()
#' ```
#' 
#' @section Use `fmglm` to Fit Finite Mixture of Multinomial Regression Models:
#' 
#' `fmglm` can fit finite mixture of multinomial regression models as well.
#' In general the code is similar to fitting the mixture of generalized linear models.
#' 
#' ```
#' model_mn <- fmglm$new(formula, data, family="multinom",
#'                       latent=2, method="em", glm_fit=F, use_llc=T)
#' ```
#' 
#' The difference is that one should prepare the dependent variable as a `factor` variable.
#' In addition, the parameter `mn_base` is available in the constructor for identifying 
#' the base group of the dependent variable. The default value is `1`.
#' 
#' Same as the fitting the mixture of GLM, one can also use the normal log-likelihood and
#' set the `glm_fit` to `TRUE`. Instead of using `glm.fit()`, `nnet::nnet()` is used to
#' fit the mixture of multinomial regression model. The following code is an example:
#' 
#' ```
#' model_mn <- fmglm$new(formula, data, family="multinom",
#'                       latent=2, method="em", glm_fit=T, use_llc=F)
#' ```
#'    


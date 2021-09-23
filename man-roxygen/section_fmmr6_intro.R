#' @section The R6 Structure of the `fmmr6` Package:
#' The `fmmr6` package adopts an Object-Oriented (OO) design with the help of \CRANpkg{R6}.
#' It contains three main blocks of constructing and fitting a finite mixture model (FMM):
#' * Finite Mixture Modelling:
#' Finite Mixture Modelling can the way we construct our finite mixture models.
#' These modelling techniques are organized under `fmmr6` class. 
#' In the current version, we have only developed the mixture of generalized linear models,
#' which is the `fmglm` class. 
#' We are going to add more modelling techniques such as the mixture of 
#' generalized linear models using a panel data structure `xtfmglm`. 
#' 
#' * Methods to fit a FMM models:
#' The fitting methods such as EM-algorithm (`em`) and Maximum Likelihood Estimation (`mle`)
#' are organized under the `AbstractMethod` class. In this way, it is convenient to pickup 
#' a different method to fit the FMM models.
#' 
#' * Families of distributions:
#' Different families of distributions are organized under the `Family` class.
#' Currently, the package only supports Gaussian distributions (`FamilyNormal`), 
#' Poisson distributions (`FamilyPoisson`),
#' Logit Models with Gaussian distributions (`FamilyLogit`),
#' and Multinomial Logit Models with Gaussian distributions (`FamilyMultiNomial`). 
#' These distributions will be sent to a `Mixer` class to generate a mixture of 
#' distributions based on the setting of the finite mixture model.
#'
#' 

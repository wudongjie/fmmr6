#' @title Family Object for Mixture of Normal Distributions
#' 
#' @author Dongjie Wu
#'
#' @description An family object for mixture of 
#' normal distributions.
#'
#' @name FamilyMNormal
#'
#' @return A FamilyMNormal Object
#'
#' @export

FamilyMNormal <- R6Class("Family",
                          inherit = Family,
                          public = list(
                            initialize = function(Y, X, latent) {
                              private$Y = Y
                              private$X = X
                              private$latent = latent
                              private$ll = self$gen_ll()
                            },
                            get_ll = function() {
                              return(private$ll)
                            },
                            
                            #' @description 
                            #' Generate the likelihood function for a mixture of
                            #' normal distributions.
                            #' @return a function `ll` with two parameters,
                            #' `theta` is a vector with parameters whose number 
                            #' is number of variables in X + 2 times 
                            #' number of latent classes. 
                            #' `d` is the indicator matrix whose size is:
                            #' rows of X by number of latent classes   
                            gen_ll = function(){
                              ll <- function(theta, d) {
                                X <- private$X
                                Y <- private$Y
                                latent <- private$latent
                                npar <- (ncol(X) + 2) * latent - 1
                                p = ncol(X)
                                n = nrow(X)
                                l = 0
                                counter = 1
                                #theta <- matrix(theta, nrow=p, ncol=latent)
                                pi_vector = theta[1:(latent-1)]
                                sigma_vector = theta[seq((latent),npar,p+1)]
                                theta_matrix = theta[-seq((latent),npar,p+1)][-(1:(latent-1))]
                                #pi_vector[latent] = 1 - sum(pi_vector[-latent])
                                #theta[1:p] = colSums(d)/nrow(d)
                                for (i in 1:latent) {
                                  if (i==latent) {
                                    l <- l + d[,i]*(log(1-sum(pi_vector[-i])) + dnorm(Y, 
                                      mean = X %*% theta_matrix[(p*(i-1)+1):(p*(i-1)+p)], 
                                      sd = sqrt(sigma_vector[i]^2), log = TRUE))
                                  } else {
                                  l <- l + d[,i]*(log(pi_vector[i]) + dnorm(Y, 
                                             mean = X %*% theta_matrix[(p*(i-1)+1):(p*(i-1)+p)], 
                                             sd = sqrt(sigma_vector[i]^2), log = TRUE))
                                  }
                                }
                                l <- sum(l)
                                return(-l)
                                }
                              return(ll)
                            }
                          ),
                          private = list(
                            Y = NULL,
                            X = NULL,
                            latent = 2,
                            ll= NULL,
                            gr= NULL
                          )
)
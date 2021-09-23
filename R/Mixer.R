#' @title A Mixer Object
#'
#' @author Dongjie Wu
#'
#' @description A Mixer object which can generate a mix distribution from given
#' distributions. 
#'
#' @name Mixer
#'
#' @return Returns R6 object of class Mixer.
#'
#' @examples 
#' Mixer$new(family="gaussian", latent=2)
#' 
#' @export

Mixer <- R6Class("Mixer",
               public = list(
                 #' @description 
                 #' Create a new instance of this [R6] [R6::R6Class] class.
                 #' @param family (`character(1)`) \cr
                 #' The family/families of the mixture model to fit in the fmglm.
                 #' The distribution family can be either a string like 
                 #' "gaussian" or a vector like `c("gaussian", "poisson")`.
                 #' The default value is `gaussian`.
                 #' @param latent (`integer(1)`) \cr
                 #' The number of latent classes. The default is 2.
                 #' @param use_llc (`boolean(1)`) \cr
                 #' Whether to use the complete log-likelihood to fit the model.
                 #' The default is `TRUE`.
                 initialize = function(family="gaussian", latent=2, use_llc=TRUE){
                   stopifnot(length(family)>=1)
                   private$family_init = family
                   private$family = self$set_family(family, latent)
                   private$latent = length(private$family)
                   private$ll = self$mix_ll(use_llc=use_llc)
                 },
                 #' @description 
                 #' The function to return the number of the latent class. 
                 get_latent = function() {
                   return(private$latent)
                 },
                 #' @description 
                 #' The function to return the log-likelihood. 
                 get_ll = function() {
                   return(private$ll)
                 },
                 #' @description 
                 #' The function to return the `family` variable.
                 get_family_init = function() {
                   if ((length(private$family_init) == 1) & (private$latent==1)) {
                     stop("Only one latent class!")
                   }
                   if ((length(private$family_init) == 1) & (private$latent>1)) {
                     family_init <- rep(private$family_init, private$latent)
                   } else {
                     family_init <- private$family_init
                   }
                   return(family_init)
                 },
                 #' @description 
                 #' The function to generate the mixed log-likelihood.
                 #' @param use_llc (`boolean(1)`) \cr
                 #' Whether to use the complete log-likelihood to fit the model.
                 #' The default is `TRUE`.
                 mix_ll = function(use_llc=TRUE){
                   mll <- function(theta, Y, X, d) {
                     latent <- private$latent
                     if (is.matrix(Y)|is.data.frame(Y)){
                       theta <- matrix(theta, ncol=ncol(Y))
                     } else {
                       theta <- matrix(theta, ncol=1)
                     }
                     npar <- nrow(theta)/latent
                     p = ncol(X)
                     n = nrow(X)
                     l = 0
                     pi_vector = colSums(d)/n
                     if (use_llc) {
                       for (i in 1:latent) {
                           l <- l + d[,i]*(log(pi_vector[i]) 
                                              + private$family[[i]]$gen_density()(theta[(npar*(i-1)+1):(npar*(i-1)+npar),], Y, X))
                       }
                     } else {
                       for (i in 1:latent) {
                        l = l + pi_vector[i] * exp(private$family[[i]]$gen_density()(theta[(npar*(i-1)+1):(npar*(i-1)+npar),], Y, X))
                       }
                       l <- log(l)
                     }
                     l <- sum(l)
                     return(-l)
                   }
                   return(mll)
                 },
                 #' @description 
                 #' Generate the posterior probability given estimated coefficients and data.
                 #' @param theta (`matrix()`) \cr
                 #' The estimated coefficients matrix.
                 #' @param pi_vector (`numeric()`) \cr
                 #' The prior probabilities of being in each class.
                 #' @param Y (`matrix()`) \cr
                 #' The dependent variable.
                 #' @param X (`matrix()`) \cr
                 #' The independent variables.
                 post_pr = function(theta, pi_vector, Y, X){
                   latent <- private$latent
                   npar <- nrow(theta)/latent
                   p = ncol(X)
                   n = nrow(X)
                   l = 0
                   counter = 1
                   pi_matrix = diag(pi_vector)
                   z_matrix = matrix(0, nrow=n,ncol=latent)
                   for (i in 1:latent) {
                     z_matrix[,i] = exp(private$family[[i]]$gen_density()(theta[(npar*(i-1)+1):(npar*(i-1)+npar),],Y,X))
                   }
                   z_matrix = z_matrix %*% pi_matrix
                   z_sum = rowSums(z_matrix)
                   z = z_matrix/z_sum
                   z[is.na(z)] <- 0.5
                   return(z)
                 },
                 #' @description 
                 #' Generate the gradiant function given the log-likelihood function.
                 #' @param ll (`function(1)`) \cr
                 #' The log-likelihood function.
                 gen_gr = function(ll){                                        
                   gr <- function(theta) {
                     g <- grad(ll,theta)
                     return(g)
                   }
                   return(gr)
                 },
                 #' @description 
                 #' Given the `family` variable and number of classes,
                 #' generate a set of `Family` objects.
                 #' @param family (`character(1)`) \cr
                 #' The family variable.
                 #' @param latent (`integer(1)`) \cr
                 #' The latent class variable.
                 #' @examples 
                 #' Mixer$new()$set_family("gaussian", 2)
                 #' # Return `c(FamilyNormal$new(), FamilyNormal$new())`.
                 #' Mixer$new()$set_family(c("gaussian", "poisson"), 2)
                 #' # Return `c(FamilyNormal$new(), FamilyPoisson$new())`. 
                 set_family = function(family, latent){
                   if ((length(family) == 1) & (latent==1)) {
                     stop("Only one latent class!")
                   }
                   if ((length(family) == 1) & (latent>1)) {
                     family <- rep(family,latent)
                   }
                   family_group = NULL
                   for (f in family) {
                     # TODO: ADD MORE FAMILY DISTRIBUTION
                     K <- private$dist_list[[f]]
                     family_group <- c(family_group, K)
                   }
                   if (length(family_group) != length(family)){
                     stop("Some families of distributions are not found!")
                   }
                   return(family_group)
                 },
                 #' @description generate the start value: if the start value is provided 
                 #' make sure it has the correct length. If the start value is not 
                 #' provided, generate a start value with the correct length.
                 gen_start = function() {
                   start = function(Y, X){
                     constr = self$gen_constraint()(Y, X)
                     start_v = rep(1, length(constr$lower))

                     gen_start_value <- function(vl,vh) {
                       if (is.infinite(vl)) {
                         vl <- -2
                       }
                       if (is.infinite(vh)) {
                         vh <- 2
                       }
                       start_v <- runif(1, min=vl, max=vh)
                       return(start_v)
                     }
                     start = matrix(mapply(gen_start_value, 
                                           constr$lower, 
                                           constr$upper), 
                                    ncol=ncol(constr$lower))
                   return(start)
                   }
                 },
                 #' @description 
                 #' Generate the constraint of the parameters.
                 gen_constraint = function(){
                   latent = private$latent
                   if (latent==1) {
                     constr = private$family$gen_constraint
                     return(constr)
                   } else {
                     constr = function(Y, X){
                       mix_l = c()
                       mix_h = c()
                       for (f in private$family) {
                         mix_l = rbind(mix_l,f$gen_constraint(Y, X)$lower)
                         mix_h = rbind(mix_h,f$gen_constraint(Y, X)$upper)
                       }
                       return(
                         list(
                           lower = mix_l,
                           upper = mix_h
                         )
                         )
                     }
                     return(constr)
                   }
                   }
               ),
               private=list(
                 family=NULL,
                 family_init=NULL,
                 latent=NULL,
                 ll=NULL,
                 gr=NULL,
                 dist_list = list(
                   "gaussian" = FamilyNormal$new(),
                   "poisson" = FamilyPoisson$new(),
                   "logit" = FamilyLogit$new(),
                   "multinom" = FamilyMultiNomial$new()
                 )
               ))

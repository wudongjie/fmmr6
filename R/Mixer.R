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
#' @example Mixer$new(formula, data, family="gaussian", method="em",
#'                    latent=2)
#' @export

Mixer <- R6Class("Mixer",
               public = list(
                 #' Create a new instance of this [R6] [R6::R6Class] class.
                 #' @param formula `formula(1)` \cr
                 #' The formula/expression of the model to fit in the fmglm
                 #' @param data `data(1)` \cr
                 #' The Data used in the fmglm
                 #' The distribution family which can be either a string like 
                 #' "gaussian" or a vector like c("gaussian", "poisson")
                 #' @param latent `numeric(1)` \cr
                 #' The number of latent classes
                 initialize = function(family="gaussian", latent=2){
                   stopifnot(length(family)>=1)
                   private$family_init = family
                   private$family = self$set_family(family, latent)
                   private$latent = length(private$family)
                   private$ll = self$mix_ll()
                 },
                 
                 get_latent = function() {
                   return(private$latent)
                 },
                 get_ll = function() {
                   return(private$ll)
                 },
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
                 mix_ll = function(){
                   mll <- function(theta, Y, X, d) {
                     latent <- private$latent
                     npar <- (length(theta)-latent)/latent
                     p = ncol(X)
                     n = nrow(X)
                     l = 0
                     q_vector = theta[1:latent]
                     pi_vector = q_vector^2/sum(q_vector^2)
                     theta_matrix = theta[-(1:latent)]
                     for (i in 1:latent) {
                         l <- l + d[,i]*(log(pi_vector[i]) 
                                         + private$family[[i]]$gen_density()(theta_matrix[(npar*(i-1)+1):(npar*(i-1)+npar)], Y, X))
                     }
                     l <- sum(l)
                     return(-l)
                   }
                   return(mll)
                 },
                 post_pr = function(theta, Y, X){
                   latent <- private$latent
                   npar <- (length(theta)-latent)/latent
                   p = ncol(X)
                   n = nrow(X)
                   l = 0
                   counter = 1
                   q_vector = theta[1:latent]
                   pi_vector = q_vector^2/sum(q_vector^2)
                   theta_matrix = theta[-(1:latent)]
                   pi_matrix = diag(pi_vector)
                   z_matrix = matrix(0, nrow=n,ncol=latent)
                   for (i in 1:latent) {
                     z_matrix[,i] = exp(private$family[[i]]$gen_density()(theta_matrix[(npar*(i-1)+1):(npar*(i-1)+npar)],Y,X))
                   }
                   z_matrix = z_matrix %*% pi_matrix
                   z_sum = rowSums(z_matrix)
                   z = z_matrix/z_sum
                   z[is.na(z)] <- 0.5
                   return(z)
                 },
                 gen_gr = function(ll){                                        
                   gr <- function(theta) {
                     g <- grad(ll,theta)
                     return(g)
                   }
                   return(gr)
                 },
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
                 }
                 ,
                 #' @description generate the start value: if the start value is provided 
                 #' make sure it has the correct length. If the start value is not 
                 #' provided, generate a start value with the correct length.
                 #' @param start `matrix(1)` \cr
                 #' The starting values for the model
                 gen_start = function() {
                   start = function(X){
                     constr = self$gen_constraint()(X)
                     start_v = rep(1, length(constr$lower))
                     for (i in 1:length(constr$lower))
                     {
                       cmin <- constr$lower[i]
                       cmax <- constr$upper[i]
                       if (is.infinite(cmin)) {
                         cmin <- -2
                       }
                       if (is.infinite(cmax)) {
                         cmax <- 2
                       }
                       start_v[i] <- runif(1, min=cmin, max=cmax)
                     }
                       return(start_v)
                   }
                   return(start)
                   # latent = private$latent
                   # if (latent==1) {
                   #   start = private$family$gen_start
                   #   return(start)
                   # } else {
                   #   start_pi = rep(1/latent, (latent-1))
                   #   start = function(X){
                   #     mix_start = start_pi
                   #     for (f in private$family) {
                   #       mix_start = c(mix_start, f$gen_start(X)) 
                   #     }
                   #     return(mix_start)
                   #   }
                   #   return(start)
                   # }
                 },
                 gen_constraint = function(){
                   latent = private$latent
                   if (latent==1) {
                     constr = private$family$gen_constraint
                     return(constr)
                   } else {
                     constr_pi_l = rep(-Inf, latent)
                     constr_pi_h = rep(+Inf, latent)
                     constr = function(X){
                       mix_l = constr_pi_l
                       mix_h = constr_pi_h
                       for (f in private$family) {
                         mix_l = c(mix_l, f$gen_constraint(X)$lower)
                         mix_h = c(mix_h, f$gen_constraint(X)$upper)
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
                   "logit" = FamilyLogit$new()
                 )
               ))

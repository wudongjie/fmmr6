#' @title EM Algorithm (EM) Object
#'
#' @author Dongjie Wu
#'
#' @description A concrete R6 object for EM Algorithm.
#'
#' @name em
#'
#' @return Returns R6 object of class em.
#'
#' @export

em <- R6Class("em",
               inherit = EstimMethod,
               public = list(        
                 #' @description
                 #' Fit the model using the EM algorithm
                 #' @param algo (`character(1)`) \cr
                 #' Specify the type of EM algorithm to use. Can choose `em` stands for the
                 #' conventional EM algorithm, `cem` stands for the classification EM algorithm,
                 #' and `sem` stands for the stochastic EM algorithm.
                 #' The default algorithm is `em`.
                 #' @param max_iter (`numeric(1)`) \cr
                 #' Specify the maximum number of iterations for the E-step-M-step loop.
                 #' The default number is 500.
                 #' @param start (`character(1)`) \cr
                 #' Specify the starting method of the EM algorithm.
                 #' Can either start from `kmeans` or `random`.
                 #' `kmeans` use the K-mean methods to put samples into latent classes.
                 #' `random` randomly assigns samples into latent classes.
                 #' The default method is `kmeans`.
                 #' @param rep (`numeric(1)`) \cr
                 #' Specify the number of reps EM-algorithm runs.
                 #' This parameter is designed for preventing the local maximum.
                 #' Each rep, the EM_algorithm generates a start.
                 #' It is only useful when `start` is `random`. 
                 #' After all reps, the algorithm will pick the rep with maximum log likelihood.
                 #' The default value is 1
                 #' @param div_tol (`numeric(1)`) \cr
                 #' Divergence tolerence: the convergence process stops if there are `div_tol` number of 
                 #' divergence ,i.e. the log-likelihood getting bigger.
                 fit = function(algo="em", max_iter=500, start="random", rep=1, div_tol=10){
                   if (start=="kmeans") {
                     z <- self$em_start(start)
                     result <- self$em_algo(z, algo, max_iter, div_tol)
                     if (result$message == "Convergence succeed") {
                       return(result)
                     } else {
                       print("Change to a random start with 1 rep.")
                       start <- "random"
                       rep <- 1
                       }
                   }
                   if (start=="random") {
                     ll_list = c()
                     result_list = list()
                     if (rep==1) {
                       z <- self$em_start(start)
                       result <- self$em_algo(z, algo, max_iter, div_tol)
                       if (result$message == "Convergence succeed") {
                         return(result)
                       } else {
                         print("Change to a random start with 3 rep.")
                         start <- "random"
                         rep <- 3
                       }
                     }
                     for (r in 1:rep) {
                       z <- self$em_start(start)
                       result <- self$em_algo(z, algo, max_iter, div_tol)
                       result_list[[r]] <-  result
                       ll_list <- c(ll_list, result$value)
                     }
                     return(result_list[[which.min(ll_list)]])
                   }
                   
                  },
                 #' @description Generate the start value for the EM algorithm.
                 #' @param start (`character(1)`) \cr
                 #' Specify the starting method of the EM algorithm.
                 #' Can either start from `kmeans` or `random`.
                 #' `kmeans` use the K-mean methods to put samples into latent classes.
                 #' `random` randomly assigns samples into latent classes.
                 #' The default method is `kmeans`.
                 em_start = function(start = "kmeans"){
                   if (start == "random") {
                     z <- vectorize_dummy(sample(1:self$latent, size=nrow(self$data_model$X), replace=T))
                   } else if (start == "kmeans") {
                     z <- vectorize_dummy(kmeans(cbind(self$data_model$Y,self$data_model$X), 
                                                 self$latent)$cluster)
                   } else {
                     stop("Please specify the correct starting method!")
                   }
                   return(z)
                 },
                 #' @description Running the EM algorithm, given the start values, the data and number of classes.
                 #' @param z (`matrix()`) \cr
                 #' The matrix of the posterior probability. 
                 #' @param algo (`character(1)`) \cr
                 #' Specify the type of EM algorithm to use. Can choose `em` stands for the
                 #' conventional EM algorithm, `cem` stands for the classification EM algorithm,
                 #' and `sem` stands for the stochastic EM algorithm.
                 #' The default algorithm is `em`.
                 #' @param max_iter (`numeric(1)`) \cr
                 #' Specify the maximum number of iterations for the E-step-M-step loop.
                 #' The default number is 500.
                 #' @param div_tol (`numeric(1)`) \cr
                 #' Divergence tolerence: the convergence process stops if there are `div_tol` number of 
                 #' divergence ,i.e. the log-likelihood getting bigger.
                 em_algo = function(z, algo, max_iter, div_tol){
                   result <- self$mstep(z, private$.start, optim_method = self$optim_method)
                   pi_vector <- result$pi_vector
                   theta_update <- result$par
                   convergence <- 1
                   ll_value <- 0
                   counter <- 0
                   divergence <- 0
                   while((abs(convergence) > 1e-4) & (max_iter > counter) & (divergence<div_tol)) {
                     z <- self$estep(theta_update, pi_vector)
                     if (algo=='cem') {
                       z <- self$cstep(z)
                     } else if (algo=='sem') {
                       z <- self$sstep(z)
                     }
                     result <- self$mstep(z, private$.start, optim_method = self$optim_method)
                     theta_update <- result$par
                     pi_vector <- result$pi_vector
                     convergence <- result$value - ll_value
                     ll_value <- result$value
                     counter <- counter + 1
                     if (convergence>0) {
                        divergence <- divergence + 1
                     }
                   }
                   if (divergence < div_tol) {
                     print(paste0("convergence after ", counter, " iterations"))
                     print(paste0("logLik: ", ll_value))
                     result$message <- "Convergence succeed"
                   } else {
                     print("Convergence failed! ")
                     result$message <- "Convergence failed"
                   }
                   return(result)
                 },
                 #' @description Given the data, start values, the number
                 #' of latent classes, compute the expected value of 
                 #' the indicator variable.
                 #' @param theta_update (`matrix()`) \cr
                 #' Updated coefficients
                 #' @param pi_vector (`matrix()`) \cr
                 #' The matrix with the diagonal values representing the prior probability `\pi`.
                 #' @return return the probability matrix indicating the posterior probability of individual belonging to each class.      
                 estep = function(theta_update, pi_vector){
                   hidden <- post_pr(theta_update, pi_vector, 
                                                   self$data_model$Y, self$data_model$X, self$latent, self$data_model$family, constraint=self$constraint)
                   return(hidden)
                 },
                 #' @description Given the posterior probability, generate a matrix to assign
                 #' each individual to a class. The assignment based on which probability is the largest.
                 #' @param hidden (`matrix()`) \cr
                 #' The matrix of the posterior probability 
                 cstep = function(hidden) {
                   assign_func = function(postpr) {
                     vec <- rep(0,length(postpr))
                     vec[[which.max(postpr)]] <- 1
                     return(vec)
                   }
                   return(t(apply(hidden,1,assign_func)))
                 },
                 #' @description Given the posterior probability, generate a matrix to assign
                 #' each individual to a class. The assignment is randomly sampled based on the posterior probability.
                 #' @param hidden (`matrix()`) \cr
                 #' The matrix of the posterior probability 
                 sstep = function(hidden) {
                   assign_func = function(postpr) {
                     vec <- rmultinom(1:length(postpr), size=1,prob=postpr)
                     return(vec)
                   }
                   return(t(apply(hidden,1,assign_func)))
                 },
                 #' @description Given the data, start values, the number
                 #' of latent classes, the indicator variable, the likelihood
                 #' function, solve the maximum value of the likelihood 
                 #' function and update the parameter vectors theta.
                 #' @param hidden (`matrix()`) \cr
                 #' The matrix of the posterior probability.
                 #' @param theta (`matrix()`) \cr
                 #' The matrix of the coefficients to fit.
                 #' @param optim_method (`character(1)`) \cr
                 #' The optimization method to use to fit the model.
                 #' The default is `base`.
                 mstep = function(hidden, theta, optim_method="base"){
                   npar <- self$latent + nrow(theta) * ncol(theta) - 1
                   ll <- partial(private$.likelihood_func, d=hidden, 
                                 Y=self$data_model$Y, X=self$data_model$X,
                                 latent=self$latent, family=self$data_model$family, isLog=private$.use_llc, constraint=self$constraint)
                   gr <- gen_gr(ll)
                   pi_vector = colSums(hidden)/nrow(hidden)
                   sel_optim <- private$dist_list[[optim_method]]
                   init_optim <- eval(sel_optim)$new()
                   result <- init_optim$fit(self$data_model, theta, ll, gr, hidden, pi_vector,
                                            npar, self$latent, self$data_model$family)
            
                   return(result)
                 }
                 

                 )
               )


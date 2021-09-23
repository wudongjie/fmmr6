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
               inherit = AbstractMethod,
               public = list(        
                 #' @description
                 #' Create a new instance of this [R6] [R6::R6Class] class.
                 #' @param mixer (`Mixer(1)`) \cr
                 #' A mixer object contains the mixed density function of the model.
                 #' @param Y (`matrix()`) \cr
                 #' A matrix with 1 or k columns contains the dependent variable/variables.
                 #' @param X (`matrix()`) \cr
                 #' A matrix with m column contains m independent variables.
                 #' @param start (`matrix()`) \cr
                 #' The matrix of start values for the EM algorithm.
                 #' @param constraint (`matrix()`) \cr
                 #' The matrix of constraint values for the EM algorithm
                 #' @param glm_fit (`boolean(1)`) \cr
                 #' Whether use the `glm.fit()`, `ols.wfit()` or `nnet()` to fit the model.
                 #' @return Return a R6 object of class em.
                 initialize = function(mixer, Y, X, start=NULL, constraint=NULL, glm_fit=F){
                   private$X <- X
                   private$Y <- Y
                   private$latent <- mixer$get_latent()
                   private$mixer <- mixer
                   private$glm_fit <- glm_fit
                   if (!is.null(start)) {
                     private$start <- start
                   } else {
                     private$start <- private$mixer$gen_start()(private$Y, private$X)
                   }
                   if (!is.null(constraint)) {
                     private$constraint <- constraint
                   } else {
                     private$constraint <- private$mixer$gen_constraint()(private$Y, private$X)
                   }
                 },
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
                 fit = function(algo="em", max_iter=500, start="kmeans", rep=1){
                   ll_list = c()
                   result_list = list()
                   for (r in 1:rep) {
                     likelihood_func <- private$mixer$get_ll()
                     if (start == "random") {
                       z <- vectorize_dummy(sample(1:private$latent, size=nrow(private$X), replace=T))
                     } else if (start == "kmeans") {
                       z <- vectorize_dummy(kmeans(cbind(private$Y,private$X), 
                                                   private$latent)$cluster)
                     } else {
                       stop("Please specify the correct starting method!")
                     }
  
                     result <- self$mstep(likelihood_func, z, private$start, glm_fit = private$glm_fit)
                     pi_vector <- result$pi_vector
                     theta_update <- result$par
                     convergence <- 1
                     ll_value <- 0
                     counter <- 0
                     while((convergence > 1e-4) & (max_iter > counter) ) {
                       z <- self$estep(theta_update, pi_vector)
                       if (algo=='cem') {
                         z <- self$cstep(z)
                       } else if (algo=='sem') {
                         z <- self$sstep(z)
                       }
                       result <- self$mstep(likelihood_func, z, theta_update, glm_fit = private$glm_fit)
                       theta_update <- result$par
                       pi_vector <- result$pi_vector
                       convergence <- abs(result$value - ll_value)
                       ll_value <- result$value
                       counter <- counter + 1
                     }
                     print(paste0("convergence after ", counter, " iterations"))
                     print(paste0("logLik: ", ll_value))
                     result_list[[r]] <-  result
                     ll_list <- c(ll_list, ll_value)
                   }
                   return(result_list[[which.min(ll_list)]])
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
                   hidden <- private$mixer$post_pr(theta_update, pi_vector, private$Y, private$X)
                   return(hidden)
                 },
                 #' @description Given the posterior probability, generate a matrix to assign
                 #' each individual to a class. The assignment based on which probability is the largest.
                 #' @param hidden (`matrix()`) \cr
                 #' The matrix of the posterior probability 
                 cstep = function(hidden) {
                   assign_func = function(postpr) {
                     vec <- rep(0,length(postpr))
                     vec[which.max(postpr)] <- 1
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
                 #' @param likelihood_func (`function(1)`) \cr
                 #' The likelihood function to maximize.
                 #' @param hidden (`matrix()`) \cr
                 #' The matrix of the posterior probability.
                 #' @param theta (`matrix()`) \cr
                 #' The matrix of the coefficients to fit.
                 #' @param glm_fit (`boolean(1)`) \cr
                 #' Whether use the `glm.fit()`, `ols.wfit()` or `nnet()` to fit the model.
                 mstep = function(likelihood_func, hidden, theta, glm_fit=FALSE){
                   partial <- function(f, ...) {
                     l <- list(...)
                     function(...) {
                       do.call(f, c(l, list(...)))
                     }
                   }
                   npar <- private$latent + nrow(theta) * ncol(theta) - 1
                   ll <- partial(likelihood_func, d=hidden, Y=private$Y, X=private$X)
                   gr1 <- gen_gr(ll)
                   pi_vector = colSums(hidden)/nrow(hidden)
                   lower <-  private$constraint$lower
                   upper <-  private$constraint$upper

                   if (glm_fit) {
                     result <- list()
                     fam <- private$mixer$get_family_init()
                     result$par <- c()
                     
                     for (k in 1:private$latent) {
                       if (fam[k] == "gaussian") {
                         train <- lm.wfit(private$X, private$Y, w=hidden[,k])
                         coefs <- train$coefficients
                         df <- sum(coefs != 0)  + 1
                         sigma <- sqrt(sum(hidden[,k] * (private$Y - private$X %*% coefs)^2/mean(hidden[,k]))/(nrow(private$X) - df))
                         result$par <- c(result$par, sigma, coefs)
                       } else if (fam[k] == "poisson") {
                         train <- glm.fit(private$X, private$Y, family=poisson(), weights=hidden[,k])
                         coefs <- train$coefficients
                         result$par <- c(result$par, coefs)
                       } else if (fam[k] == "logit") {
                         train <- suppressWarnings(glm.fit(private$X, private$Y, family=quasibinomial(), weights=hidden[,k]))
                         coefs <- train$coefficients
                         result$par <- c(result$par, coefs)
                       } else if (fam[k] == "multinom") {
                         train <- suppressWarnings(nnet::nnet(private$X[,-1], private$Y, weights=hidden[,k], size=0,
                                                              skip = T, entropy = TRUE, trace=F))
                         coefs <- matrix(coef(train), ncol=ncol(private$Y))
                         if (k == 1) {
                           result$par <- coefs
                         } else {
                           result$par <- rbind(result$par, coefs)
                         }
                       }
                     }
                     if (!( "multinom" %in% fam )) {
                       result$par <- matrix(result$par, ncol=1)
                     } 
                     result$pi_vector <- pi_vector
                     result$value <- ll(result$par)
                   } else {
                     result <- suppressWarnings(optim(theta, ll, gr=gr1, method="L-BFGS-B",
                                                      hessian=T)) 
                     result$pi_vector <- pi_vector
                   }

                   result$AIC <- 2 * npar + 2 * result$value
                   result$BIC <- npar * log(nrow(private$X)) + 2 * result$value

                   return(result)
                 }
                 ),
               private = list(
                 Y = NULL,
                 X = NULL,
                 latent = 1,
                 constraint = NULL,
                 likelihood_func = NULL,
                 start = NULL,
                 mixer = NULL,
                 glm_fit = FALSE
               )
               )


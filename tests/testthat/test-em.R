test_that("test e-step", {
  browser()
  x1 <- rnorm(100, mean=1, sd=1)
  x2 <- rnorm(100, mean=1, sd=1)
  theta <- matrix(c(1,1,2,1,2,3), ncol=1)
  pi_v <- matrix(c(0.36, 0.64), ncol=100, nrow=2)
  pi_v <- t(pi_v)
  Y <- matrix(1, nrow=100, ncol=1)
  Y[1:36] <- theta[2,1]*x1[1:36] + theta[3,1]*x2[1:36]
  Y[37:100] <- theta[5,1]*x1[37:100]+theta[6,1]*x2[37:100]
  X <- matrix(c(x1,x2),ncol = 2)
  latent <- 2
  formula <- Y ~ 0 + x1 + x2
  data <- data.frame(Y=Y, x1=x1, x2=x2)
  dm1 <- DataModel$new(data, formula, family="gaussian")  
  y_result <- em$new(latent, dm1)$estep(theta, pi_v)
  z <- vectorize_dummy(kmeans(cbind(Y,X), 2)$cluster)
  y_expect <- c(36, 64)
  expect_equal(colSums(y_result), y_expect, tolerance=1e-1)
  sum_to_1 = rep(c(1),100)
  expect_equal(rowSums(y_result), sum_to_1)
})


test_that("test m-step", {
  x1 <- rnorm(1000, mean=0, sd=1)
  x2 <- rnorm(1000, mean=0, sd=1)
  theta <- matrix(c(1,2,5,1,4,3), ncol=1)
  pi_v <- matrix(c(0.36, 0.64), ncol=1000, nrow=2)
  pi_v <- t(pi_v)
  Y <- matrix(1, nrow=1000, ncol=1)
  Y[1:360] <- theta[2,1]*x1[1:360] + theta[3,1]*x2[1:360]
  Y[361:1000] <- theta[5,1]*x1[361:1000]+theta[6,1]*x2[361:1000]
  X <- matrix(c(x1,x2),ncol = 2)
  latent <- 2
  formula <- Y ~ 0 + x1 + x2
  data <- data.frame(Y=Y, x1=x1, x2=x2)
  dm1 <- DataModel$new(data, formula, family="gaussian")  
  em1 <- em$new(latent, dm1)
  y_result <- em1$estep(theta, pi_v)
  result <- em1$mstep(y_result, theta)
  #expect_equal(result$par, matrix(theta, ncol=1), tolerance = 0.1)
})


# test_that("test fit", {
#   data("NPreg")
#   latent <- 2
#   NPreg$x2 <- (NPreg$x)^2
#   formula <- yn~x+x2
#   z <- kmeans(NPreg$yn, 2)
#   Y <-  model.frame(formula, NPreg)[,1]
#   X <- model.matrix(formula, NPreg)
#   start1 <- matrix(c(3,0.2,1,3,5,5,2,2), ncol=1)
#   start2 <- c(1,1,1,1,1,1,1,1)
#   pi_v <- c(0.5, 0.5)
#   dm1 <- DataModel$new(NPreg, formula, family="gaussian")  
#   
#   em2 <- em$new(latent, dm1, start=start1, optim_method="glm")
#   result2 <- em2$fit(algo="em")
#   em1 <- em$new(latent, dm1, start=start1)
#   result <- em1$fit(algo="em")
#   mod2 <- flexmix::flexmix(yn~x+x2, data=NPreg, k=2) 
#   result_mod2 <- rbind(parameters(mod2, component=1), 
#                        parameters(mod2, component=2))
# 
#   expect_equal(result$value, result2$value, tolerance = 0.1)
# })


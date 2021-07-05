test_that("test e-step", {
  x1 <- rnorm(100, mean=1, sd=1)
  x2 <- rnorm(100, mean=1, sd=1)
  theta <- c(0.8,1,1,2,1,2,3)
  Y <- matrix(1, nrow=100, ncol=1)
  Y[1:80] <- theta[3]*x1[1:80] + theta[4]*x2[1:80]
  Y[81:100] <- theta[6]*x1[81:100]+theta[7]*x2[81:100]
  X <- matrix(c(x1,x2),ncol = 2)
  latent <- 2
  #mod1 <- FamilyMNormal$new(Y, X, latent)
  mix1 <-  Mixer$new(family="gaussian", latent=2)
  y_result <- em$new(mix1, Y, X)$estep(theta)
  y_expect <- c(80, 20)
  expect_equal(colSums(y_result), y_expect, tolerance=1e-1)
  sum_to_1 = rep(c(1),100)
  expect_equal(rowSums(y_result), sum_to_1)
})


test_that("test m-step", {
  x1 <- rnorm(1000, mean=0, sd=1)
  x2 <- rnorm(1000, mean=0, sd=1)
  theta <- c(0.8,1,2,5,1,4,3)
  Y <- matrix(1, nrow=1000, ncol=1)
  Y[1:800] <- theta[3]*x1[1:800] + theta[4]*x2[1:800]
  Y[801:1000] <- theta[6]*x1[801:1000]+theta[7]*x2[801:1000]
  X <- matrix(c(x1,x2),ncol = 2)
  latent <- 2
  mix1 <- Mixer$new(family="gaussian", latent=2)
  em1 <- em$new(mix1, Y, X)
  y_result <- em1$estep(theta)
  print(y_result)
  print(colSums(y_result))
  ll <- mix1$mix_ll()
  result <- em1$mstep(ll, y_result, theta)
  print(result)
})


test_that("test fit", {
  data("NPreg")
  latent <- 2
  NPreg$x2 <- (NPreg$x)^2
  formula <- yn~x+x2
  z <- kmeans(NPreg$yn, 2)
  print(z)
  Y <-  model.frame(formula, NPreg)[,1]
  X <- model.matrix(formula, NPreg)
  start1 <- c(0.5,3,0.2,1,3,5,5,2,2)
  start2 <- c(0.5,1,1,1,1,1,1,1,1)
  mix1 <- Mixer$new(family="gaussian", latent=2)
  em1 <- em$new(mix1, Y, X, start=NULL)
  result <- em1$fit()
  mod2 <- flexmix::flexmix(yn~x+x2, data=NPreg, k=2)
  print(result)
  print(mod2)
  print(parameters(mod2, component=1))
  print(parameters(mod2, component=2))
})


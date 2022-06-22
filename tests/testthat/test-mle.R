test_that("test fit", {
  # Generate dataset
  x1 <- rnorm(1000, mean=3, sd=1)
  x2 <- rnorm(1000, mean=5, sd=2)
  e <- rnorm(1000, mean=0, sd=1)
  y <- 2 * x1 + 3 * x2 + e
  data1 <- data.frame(list(y=y,x1=x1,x2=x2))
  formula1 <- y ~ 0 + x1 + x2
  data_model1 <- DataModel$new(data1, formula1, family="gaussian")
  X <- matrix(data = c(x1,x2), ncol=2)
  y2 = 4 * x1 + 5 * x2 + 5 + e
  X2 <- matrix(data = c(x1,x2, rep(1,length(x1))), ncol=3)
  data2 <- data.frame(list(y=y2,x1=x1,x2=x2))
  formula2 <- y~x1+x2
  data_model2 <- DataModel$new(data2, formula2, family="gaussian")
  y3 = 4 * x1 + 1 * x2 + 2 * x1 * x2 + 1 + e
  data3 <- data.frame(list(y=y3,x1=x1,x2=x2))
  formula3 <- y3 ~ x1 + x2 + x1:x2
  x3 <- x1*x2
  X3 <- matrix(data = c(x1,x2,x3, rep(1,length(x1))), ncol=4)
  data_model3 <- DataModel$new(data3, formula3, family="gaussian")
  lm1 <- lm(y~0+x1+x2)
  # Manually do the mle
  minusll <- function(sigma, theta1, theta2) {
    -sum(dnorm(y, mean = x1*theta1 + x2*theta2, 
                                           sd = sigma, log = TRUE))}
  mle_result <- suppressWarnings(stats4::mle(minusll, start=list(sigma=1,theta1=2,theta2=3)))
  mle_coef <- stats4::coef(mle_result)[-1]
  names(mle_coef) <- c("x1", "x2")
  expect_equal(mle_coef, lm1$coefficients, tolerance=1e-5)
  mod1 <- mle$new(1,data_model1)
  result1 <- mod1$fit(start="random", rep=5)
  mod1_n <- mle$new(1,data_model1, optim_method="glm")
  result2 <- mod1_n$fit(start="random", rep=5)
  #expect_equal(result2$value, result1$value, tolerance=1e-3)

  lm2 <- lm(y2~x1+x2)
  minusll <- function(sigma, theta1, theta2, theta3) {
    -sum(dnorm(y2, mean = theta1 + x1*theta2 + x2*theta3, 
               sd = sigma, log = TRUE))}
  mle_result <- suppressWarnings(stats4::mle(minusll, start=list(sigma=1,theta1=4,theta2=2,theta3=3)))
  mle_coef <- stats4::coef(mle_result)[-1]
  names(mle_coef) <- c("(Intercept)", "x1", "x2")
  mod2 <- mle$new(1, data_model2, start=c(1,2,3,4))$fit()
  mod2_n <- mle$new(1, data_model2, start=c(1,2,3,4), optim_method="glm")$fit()
  expect_equal(mod2_n$value, mod2$value, tolerance=1e-3)

  lm3 <- lm(y3~x1+x2+x3)
  minusll2 <- function(sigma, theta1, theta2, theta3,theta4) {
    -sum(dnorm(y3, mean = theta1 + x1*theta2 + x2*theta3+x3*theta4,
               sd = sigma, log = TRUE))}
  mle_result2 <- suppressWarnings(stats4::mle(minusll2, start=list(sigma=1,theta1=4,theta2=2,theta3=3,theta4=4),
                                              control = list(parscale = c(1, 1, mean(x1), mean(x2), mean(x3)))))
  mle_coef2 <- stats4::coef(mle_result2)[-1]
  names(mle_coef2) <- c("(Intercept)", "x1", "x2", "x3")
  mod3 <- mle$new(1, data_model3, start=c(1,4,2,3,4))$fit()

  expect_equal(mle_coef2, lm3$coefficients, tolerance=1e-1)
  expect_equal(mle_result2@details$value, mod3$value, tolerance=1e-3)

})


# test_that("test mix fit", {
#   data("NPreg")
#   print(NPreg)
#   NPreg$x2 <- (NPreg$x)^2
#   formula <- yn~x+x2
#   Y <-  model.frame(formula, NPreg)[,1]
#   X <- model.matrix(formula, NPreg)
#   lfn <- FamilyNormal$new(Y, X, latent=2)
#   theta <- c(0.5,3,0.2,1,3,5,5,2,2)
#   mle1 <- mle$new(lfn, start=theta)
#   browser()
#   mod1 <- mle1$fit(latent=2)
#   mod2 <- flexmix::flexmix(yn~x+x2, data=NPreg, k=2)
#   print(mod1)
#   print(parameters(mod2, component=1))
#   print(parameters(mod2, component=2))
# })
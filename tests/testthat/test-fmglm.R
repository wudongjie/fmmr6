test_that("multiplication works", {
  data("NPreg")
  latent <- 2
  NPreg$x2 <- (NPreg$x)^2
  formula <- yn~x+x2
  formula2 <- yp~x
  formula3 <- yb~yn
  print(lm(formula, NPreg))
  # TODO: TEST WITHOUT THE START
  model1 <- fmglm$new(formula, NPreg, family="gaussian", latent=2,
            method="em", start=c(0.5,3,0.2,1,3,5,5,2,2))
  model2 <- fmglm$new(formula2, NPreg, family="poisson", latent=2,
                      method="em", start=c(0.5,2,-0.1,1,0.1))
  model3 <- fmglm$new(formula3, NPreg, family="logit", latent=2,
                      method="em", start=c(0.5,0.2,1,2,2))
  result <- model3$fit()
  print(result)
})

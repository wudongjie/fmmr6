test_that("multiplication works", {
  data("NPreg")
  latent <- 2
  NPreg$x2 <- (NPreg$x)^2
  formula <- yn~x+x2
  print(coef(lm(formula, NPreg)))
  browser()
  model_ols <- fmglm$new(formula, NPreg, family = "gaussian", method="mle", latent=1)
  print(model_ols$fit())
  
  model_ols2 <- fmglm$new(formula, NPreg, family = "gaussian", method="mle", latent=1, glm_fit=T)
  print(model_ols2$fit())
  
  # the model using the complete loglik and buildin optim
  # model1 <- fmglm$new(formula, NPreg, family="gaussian", latent=2,
  #           method="em", glm_fit=F, use_llc=T)
  # # the model using the normal loglik and glm.fit
  # model2 <- fmglm$new(formula, NPreg, family="gaussian", latent=2,
  #                     method="em", glm_fit=T, use_llc=F)
  # X <- matrix(c(NPreg$x,NPreg$x2),ncol = 2)
  # result1 <- model1$fit(algo="em")
  # result2 <- model2$fit(algo="em")
  # print(result1)
  # print(result2)
  # # test the cem algorithm
  # result1c <- model1$fit(algo="cem")
  # result2c <- model2$fit(algo="cem")
  # # test the sem algorithm
  # browser()
  # result1s <- model1$fit(algo="sem", max_iter=100)
  # result2s <- model2$fit(algo="sem", max_iter=100)
  # 
  # expect_equal(result1$par, result2$par, tolerance = 0.001)
  # expect_equal(result1s$par, result2s$par, tolerance = 0.001)
  # expect_equal(result1c$par, result2c$par, tolerance = 0.001)


  print(model1$summarize())
})

test_that("test poisson", {
  data("NPreg")
  latent <- 2
  NPreg$x2 <- (NPreg$x)^2
  formula2 <- yp~x
  model2 <- fmglm$new(formula2, NPreg, family="poisson", latent=2,
                      method="em", glm_fit=F, use_llc=T)

  flex_result2 <- flexmix::flexmix(formula2, NPreg, k=2,
                                   model = FLXMRglm(family = "poisson"))

  print(flex_result2@logLik)
  flex_coef <- matrix(rbind(parameters(flex_result2, component=1),parameters(flex_result2, component=2)))
  print(flex_coef[,1])

  print(summary(flex_result2))
  result2 <- model2$fit(start="random", rep=5)
  expect_equal(result2$value, 541.2, tolerance = 0.1)
  #expect_equal(result2$par[,1], flex_coef[,1], tolerance=0.1)
})


# 
# test_that("test poisson", {
#   latent <- 2
#   formula <- drvisits~private+medicaid+chronic
#   flex_result <- flexmix::flexmix(formula, gsem_mixture, k=2,
#                                   model = FLXMRglm(family = "poisson"))
#   print(summary(flex_result))
#   print(parameters(flex_result, component=1))
#   print(parameters(flex_result, component=2))
# 
#   model <- fmglm$new(formula, gsem_mixture, family="poisson", latent=2,
#                      method="em")
#   result1 <- model$fit()
#   print(result1)
# 
# 
# })
# 
# test_that("test logit", {
#   formula <- private~age+educ+income+female
#   formula2 <- cbind(private, 1 - private)~age+educ+income+female
#   formula_t <- Survived~factor(Pclass)+Fare+Sex
#   formula_t2 <- cbind(Survived, 1 - Survived)~Pclass+Fare+Sex
#   latent <- 2
#   flex_result <- flexmix::flexmix(formula2, gsem_mixture, k=2,
#                                   model = FLXMRglm(family = "binomial"))
#   print(flex_result)
#   print(flex_result@logLik)
#   print(parameters(flex_result, component=1))
#   print(parameters(flex_result, component=2))
#   Y <- model.frame(formula, gsem_mixture)[,1]
#   X <- model.matrix(formula, gsem_mixture)
#   print(size(X))
#   #print(X)
#   mixt_result <- mixtools::logisregmixEM(Y, X, beta=matrix(0.9,5,2), k=2, addintercept = F)
#   print(summary(mixt_result))
#   model <- fmglm$new(formula, gsem_mixture, family="logit", latent=2,
#                      method="em")
#   result <- model$fit()
#   print(result)
# })
# 
# 
# test_that("test multinomial", {
#   gss_test <- gss94_educ[c("sib","ed3cat","ses2cat")]
#   gss_test$sib <- factor(gss_test$sib)
#   gss_test$ed3cat <- factor(gss_test$ed3cat)
#   gss_test$ses2cat <- factor(gss_test$ses2cat)
#   gss_test$hs_sib <- ifelse(((gss_test$ed3cat==2) & (gss_test$sib==1)), 1, 0)
#   gss_test$col_sib <- ifelse(((gss_test$ed3cat==3) & (gss_test$sib==1)), 1, 0)
#   gss_test$hs_ses <- ifelse(((gss_test$ed3cat==2) & (gss_test$ses2cat==1)), 1, 0)
#   gss_test$col_ses <- ifelse(((gss_test$ed3cat==3) & (gss_test$ses2cat==1)), 1, 0)
#   # Y <- fastDummies::dummy_columns(gss94$egp, remove_first_dummy = T)
#   # Y <- Y[,2:ncol(Y)]
#   # X <- fastDummies::dummy_columns(gss94$fa_egp, remove_first_dummy = T)
#   # X <- X[, 2:ncol(X)]
#   # new_data <- cbind(Y, X)
#   formula <- ed3cat ~ hs_sib + col_sib + hs_ses + col_ses
#   formula2 <- ed3cat ~ sib + ses2cat
#   # print(model.frame(formula, gss94)[,1])
#   # print(class(model.frame(formula, gss94)[,1]))
#   # print(model.matrix(formula, gss94))
#   model_mn <- fmglm$new(formula2, gss_test, family="multinom",
#                         latent=2, method="em", glm_fit=F, use_llc=T)
# 
#   result <- model_mn$fit(algo="em")
#   print(model_mn$summarize()
# })



# test_that("multiplication works", {
#   data("NPreg")
#   latent <- 2
#   NPreg$x2 <- (NPreg$x)^2
#   formula <- yn~x+x2
#   model_ols <- fmglm$new(formula, NPreg, family = "gaussian", method="mle", latent=1)
#   result_ols1 <- model_ols$fit()
#   # print(model_ols$summarize())
#   # model_ols2 <- fmglm$new(formula, NPreg, family = "gaussian", method="mle", latent=1, optim_method="glm")
#   # result_ols2 <- model_ols2$fit()
#   # model_ols3 <- fmglm$new(formula, NPreg, family = "gaussian", method="mle", latent=1, optim_method="lm")
#   # result_ols3 <- model_ols3$fit()
#   # expect_equal(result_ols1$value, result_ols2$value, tolerance = 0.001)
#   # expect_equal(result_ols1$value, result_ols3$value, tolerance = 0.001)
#   # the model using the complete loglik and buildin optim
#   model1 <- fmglm$new(formula, NPreg, family="gaussian", latent=2,
#             method="em", use_llc=T, optim_method="base")
#   # the model using the normal loglik and glm.fit
#   model2 <- fmglm$new(formula, NPreg, family="gaussian", latent=2,
#                       method="em", use_llc=F, optim_method="glm")
#   model3 <- fmglm$new(formula, NPreg, family="gaussian", latent=2,
#                        method="em", use_llc=T)
#   X <- matrix(c(NPreg$x,NPreg$x2),ncol = 2)
#   result1 <- model1$fit(algo="em")
#   print(model1$summarize())
#   result2 <- model2$fit(algo="em")
#   result3 <- model3$fit(start="random", algo="em", rep=5)
#   #print(model1$summarize())
#   #print(model2$summarize())
#   # test the cem algorithm
# 
#   #result1c <- model1$fit(algo="cem")
#   #result2c <- model2$fit(algo="cem")
#   # test the sem algorithm
#   #result1s <- model1$fit(algo="sem", max_iter=100)
#   #result2s <- model2$fit(algo="sem", max_iter=100)
# 
# 
#   # expect_equal(result1$value, result2$value, tolerance = 0.001)
#   # expect_equal(result2$value, result3$value, tolerance = 0.001)
#   # expect_equal(result1s$value, result2s$value, tolerance = 0.001)
#   # expect_equal(result1c$value, result2c$value, tolerance = 0.001)
# 
# })
# 
# test_that("test poisson", {
#   data("NPreg")
#   latent <- 2
#   NPreg$x2 <- (NPreg$x)^2
#   formula2 <- yp~x
#   model1 <- fmglm$new(formula2, NPreg, family="poisson", latent=2,
#                       method="em", use_llc=T)
#   model2 <- fmglm$new(formula2, NPreg, family="poisson", latent=2,
#                       method="em", use_llc=F, optim_method="glm")
#   # Test random start with 5 rep
#   result1 <- model1$fit(start="random", rep=5)
#   result2 <- model2$fit()
#   print(model1$summarize())
#   print(result1)
#   print(result2)
#   expect_equal(result1$value, result2$value, tolerance = 1)
# })
# 

# test_that("test logit", {
#   load(list.files(system.file('extdata', package = 'fmmr6'), full.names = T)[1])
#   data(gsem_mixture)
#   formula <- private~age+educ+income+female
#   formula2 <- cbind(private, 1 - private)~age+educ+income+female
#   latent <- 2
#   # flex_result <- flexmix::flexmix(formula2, gsem_mixture, k=2,
#   #                                 model = FLXMRglm(family = "binomial"))
#   # print(flex_result)
#   # print(flex_result@logLik)
#   # print(parameters(flex_result, component=1))
#   # print(parameters(flex_result, component=2))
#   Y <- model.frame(formula, gsem_mixture)[,1]
#   X <- model.matrix(formula, gsem_mixture)
#   #print(X)
#   #browser()
#   model1m <- fmglm$new(formula, gsem_mixture, family="logit", latent=1,
#                       method="mle", optim_method="glm")
#   model2m <- fmglm$new(formula, gsem_mixture, family="logit", latent=1,
#                       method="mle")
#   result1m <- model1m$fit()
#   result2m <- model2m$fit()
# 
#   model1 <- fmglm$new(formula, gsem_mixture, family="logit", latent=2,
#                       method="em", optim_method="glm")
#   model2 <- fmglm$new(formula, gsem_mixture, family="logit", latent=2,
#                      method="em")
#   result1 <- model1$fit()
#   result2 <- model2$fit()
#   print(result1)
# })
# 
test_that("test clogit with simulation", {
  browser()
  #library(flexmix)
  #library(survival)
  set.seed(100)
  beta1 <- matrix(c(.6,.1, 1.1, -.1), 2, 2)
  beta2 <- matrix(c(.3,.2, 0.5, 0.7), 2, 2)
  x <- sample.int(3, 1000, replace =T)
  x <- vectorize_dummy(x)
  x2 <- x[,2]
  x3 <- x[,3]
  Xt <- cbind(x2, x3)
  xbeta1 <- exp(Xt%*%beta1)
  xbeta2 <- exp(Xt%*%beta2)
  prb1 <- cbind(1, xbeta1)
  prb2 <- cbind(1, xbeta2)
  w <- rbinom(1000, 1, .3)
  # One class case
  y1 <- t(apply(prb1, 1, rmultinom, n = 1, size = 1))
  # Two classes
  y2 <- w*t(apply(prb1, 1, rmultinom, n = 1, size = 1))+
    (1-w)*t(apply(prb2, 1, rmultinom, n = 1, size = 1))
  df <- cbind.data.frame(y1=apply(y1, 1, function(x) which(x==1)), 
                         y2=apply(y2, 1, function(x) which(x==1)), x2=x2, x3=x3)
  df$y1 <- factor(df$y1)
  df$y2 <- factor(df$y2)
  formula_nom <- y1 ~ 0 + x2 +x3
  formula_nom2 <- y2 ~ 0 + x2 +x3
  nfit <- summary(nnet::multinom(formula_nom, df))
  print(nfit)
  model_s <- fmglm$new(formula_nom, df, family="multinom",
                     latent=1, method="mle", use_llc=T, mn_base=1)
  model1 <- fmglm$new(formula_nom2, df, family="multinom",
                       latent=2, method="em", use_llc=T)
  result_s <- model_s$fit()
  result1 <- model1$fit()
  browser()
  #print(result_s$summerize())
  #print(result1$summerize())
  })
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
#   formula <- ed3cat ~ hs_sib + col_sib + hs_ses + col_ses
#   formula2 <- ed3cat ~ sib + ses2cat
#   cmat <- matrix(c(0,1,1,1,0,0),nrow=3, ncol=2)
#   print(dim(cmat))
#   model_s <- fmglm$new(formula2, gss_test, family="multinom",
#                       latent=1, method="mle", use_llc=T, mn_base=1)
#   model_s1 <- fmglm$new(formula2, gss_test, family="multinom",
#                        latent=1, method="mle", use_llc=T, constraint=cmat)
#   model1 <- fmglm$new(formula, gss_test, family="multinom",
#                       latent=2, method="em", use_llc=T)
#   model2 <- fmglm$new(formula, gss_test, family="multinom",
#                         latent=2, method="em", use_llc=T, constraint=cmat)
#   result_s <- model_s$fit()
#   result_s1 <- model_s1$fit()
#   result1 <- model1$fit()
#   result2 <- model2$fit(algo="em")
# 
#   #print(model1$summarize())
#   #print(model2$summarize())
# })

test_that("test clogit with simulation", {
  browser()
  #library(flexmix)
  #library(survival)
  set.seed(100)
  beta1 <- matrix(c(.6,.1, 1.1, -.1), 2, 2)
  beta2 <- matrix(c(.3,.2, 0.5, 0.7), 2, 2)
  x <- sample.int(3, 1000, replace =T) 
  x <- vectorize_dummy(x)
  x2 <- x[,2]
  x3 <- x[,3] 
  Xt <- cbind(x2, x3)
  xbeta1 <- exp(Xt%*%beta1)
  xbeta2 <- exp(Xt%*%beta2)
  prb1 <- cbind(1, xbeta1)
  prb2 <- cbind(1, xbeta2)
  w <- rbinom(1000, 1, .3)
  # One class case
  y1 <- t(apply(prb1, 1, rmultinom, n = 1, size = 1))
  # Two classes
  y2 <- w*t(apply(prb1, 1, rmultinom, n = 1, size = 1))+
    (1-w)*t(apply(prb2, 1, rmultinom, n = 1, size = 1))
  df <- cbind.data.frame(y=apply(y1, 1, function(x) which(x==1)), x2=x2, x3=x3)
  formula_nom <- y1 ~ 0 + x2 +x3
  nfit <- summary(nnet::multinom(formula_nom, df))
  print(nfit)
  # extend to clogit form
  y1x <- as.vector(t(y1))
  y2x <- as.vector(t(y2))
  x2x <- as.vector(sapply(x2, rep, times=3))
  x3x <- as.vector(sapply(x3, rep, times=3))
  a2x <- rep(c(0,1,0), 1000)
  a3x <- rep(c(0,0,1), 1000)
  idx <- as.vector(sapply(1:1000, rep, times=3))
  dat <- data.frame(chosen1=y1x, chosen2=y2x, x2=x2x, x3=x3x, a2=a2x, a3=a3x, id=idx)  
  # Use integer. No factor.
  dat$a2_x2 <- as.integer((dat$a2==1) & (dat$x2==1))
  dat$a2_x3 <- as.integer((dat$a2==1) & (dat$x3==1))
  dat$a3_x2 <- as.integer((dat$a3==1) & (dat$x2==1))
  dat$a3_x3 <- as.integer((dat$a3==1) & (dat$x3==1))
  dat$chosen <- dat$chosen2
  dat$alt <- rep(1:3, 1000)
  dat$obs <- dat$id
  
  #write.csv(dat, "sim_clogit.csv")
  formula1 <- chosen1 ~ 0 + a2_x2 + a2_x3 + a3_x2 + a3_x3+ strata(id)
  formula2 <- chosen2 ~ 0 + a2_x2 + a2_x3 + a3_x2 + a3_x3+ strata(id)
  formula3 <- chosen ~ a2_x2 + a2_x3 + a3_x2 + a3_x3 + 0
  #cfit1 <- clogit(formula1, dat)
  #print(summary(cfit1))
  #flexfit1 <- flexmix(cbind(chosen2, 1-chosen2) ~ 0 + a2_x2 + a2_x3 + a3_x2 + a3_x3 | id, data=dat, 
  #                    k=2, model = FLXMRglm(family = "binomial"))
  #cfit2 <- clogit(formula2, dat)
  #emfit <- em(cfit2, latent=2, algo="sem", verbose=T)
  #browser()
  model_c3 <- fmmr6::fmglm$new(formula3, dat, family="clogit", data_str="longc",
                               latent=2, method="em")
  result_c3 <- model_c3$fit(algo="sem")
  model_c3$summarize()
  print(result_c3)
  browser()
})

# test_that("test conditional logit", {
#   browser()
#   usdata <- read.csv(list.files(system.file('extdata', package = 'fmmr6'), full.names = T)[3])
#   formula_c <- chosen ~ 0 + a2 + a3 + a1_x1 + a2_x2 + a3_x3
#   model_c <- fmmr6::fmglm$new(formula_c, usdata, family="clogit", data_str="longc",
#                               latent=1, method="mle")
#   result_c <- model_c$fit()
#   cmat <- matrix(c(0,1,2,2,2),nrow=5, ncol=1)
#   model_c2 <- fmmr6::fmglm$new(formula_c, usdata, family="clogit", data_str="longc",
#                                latent=1, method="mle", constraint=cmat)
#   result_c2 <- model_c2$fit()
#   model_c3 <- fmmr6::fmglm$new(formula_c, usdata, family="clogit", data_str="longc",
#                                latent=2, method="em")
#   result_c3 <- model_c3$fit()
#   print(result_c3)
# }
# )
# 
# test_that("test unidiff", {
#   browser()
#   usdata <- read.csv(list.files(system.file('extdata', package = 'fmmr6'), full.names = T)[3])
#   usdata <- usdata[usdata$chosen==1,c("y", "x", "g")]
#   formula1 <- y ~ 0 + x + g
#   model1 <- fmmr6::fmglm$new(formula1, usdata, family="unidiff", data_str="default", 
#                               latent=1, method="mle")
#   result1 <- model1$fit()
#   print(result1)
#   model2 <- fmmr6::fmglm$new(formula1, usdata, family="unidiff", data_str="default", 
#                              latent=2, method="em")
#   result2 <- model2$fit(verbose=T)
#   print(result2)
# })
# 
# 
# 
# test_that("test concomitant", {
#   browser()
#   x1 <- rnorm(100, mean=1, sd=1)
#   x2 <- rnorm(100, mean=1, sd=2)
#   z1 <- rnorm(100, mean=1, sd=1)
#   z2 <- rnorm(100, mean=1, sd=2)
#   alpha <- matrix(c(2,3,4,3,2,3), ncol=2)
#   theta <- matrix(c(1,1,2,1,2,3), ncol=1)
#   v1 <- exp(alpha[2,1]*z1 + alpha[3,1]*z2 + alpha[1,1])
#   v2 <- exp(alpha[2,2]*z1 + alpha[3,2]*z2 + alpha[1,2])
#   pi_v <- cbind(v1/(v1+v2), v2/(v1+v2))
#   Y <- matrix(1, nrow=100, ncol=1)
#   Y1 <- theta[2,1]*x1 + theta[3,1]*x2
#   Y2 <- theta[5,1]*x1+theta[6,1]*x2
#   Y <- Y1*pi_v[,1] + Y2*pi_v[,2]
#   X <- matrix(c(x1,x2), ncol=2)
#   Z <- matrix(c(z1,z2), ncol=2)
#   latent <- 2
#   formula <- Y ~ 0 + x1 + x2
#   pi_formula <- ~ z1 + z2
#   data <- data.frame(Y=Y, x1=x1, x2=x2, z1=z1, z2=z2)
#   model1 <- fmglm$new(formula, data, latent=2, concomitant=pi_formula,
#                       method="em", use_llc=T)
#   result <- model1$fit()
#   print(result)
# })


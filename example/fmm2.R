

x1 <- rnorm(1000, mean=3, sd=1)
x2 <- rnorm(1000, mean=5, sd=2)
e <- rnorm(1000, mean=0, sd=1)

y <- 0.3*(3*x1+10*x2) + 0.7*(x1+2*x2) + e

df_fmm2 <- data.frame(y=y,x1=x1,x2=x2)

mod2 <- fmglm$new(y~x1+x2, rand_lm_data, latent=2)$fit()
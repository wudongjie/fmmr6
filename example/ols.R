x1 <- rnorm(1000, mean=3, sd=1)
x2 <- rnorm(1000, mean=5, sd=2)
e <- rnorm(1000, mean=0, sd=1)

y <- 2 * x1 + 3 * x2 + e
data1 <- data.frame(list(y=y,x1=x1,x2=x2))

y2 = 4 * x1 + 5 * x2 + 5 + e
data2 <- data.frame(list(y=y2,x1=x1,x2=x2))

y3 = 4 * x1 + 1 * x2 + 2 * x1 * x2 + 4 + e
data3 <- data.frame(list(y=y3,x1=x1,x2=x2))


lm1 <- lm(y~x1+x2,rand_lm_data)
summary(lm1)

mod1 <- fmglm$new(y~x1+x2, data1, intercept=FALSE)$fit()
mod2 <- fmglm$new(y~x1+x2, data2)$fit()
mod3 <- fmglm$new(y~x1+x2+x1:x2, data3)$fit()
#k <- mod1



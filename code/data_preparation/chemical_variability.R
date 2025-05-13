library(MASS)
library(lme4)
library(data.table)

mySigma    <- matrix(c(10,3,3,2),2,2)
two_points <- mvrnorm(1000, c(15,12), mySigma)
two_points <- as.data.frame(two_points)
names(two_points) <- c("p1", "p2")
two_points$id <- as.factor(1:dim(two_points)[1])

mod1 <- lm(p1 ~ 1, data = two_points)

names(mod1)
sqrt(sum(mod1$residuals^2) / mod1$df)
sd(two_points$p1)

## here these two are exactly the same!

cv_p1 <- sd(two_points$p1)/mean(two_points$p1)
cv_p2 <- sd(two_points$p2)/mean(two_points$p2)

two_points_lng <- melt(setDT(two_points), id.vars = c("id"), variable.name = "both_p")

mod2 <- lmer(value ~ 1 + (1 | id), data = two_points_lng)
summary(mod2)

resid_sd <- sigma(mod2)
sd(two_points_lng$value) ## in this case not the same as total sd b/c repeated measures
mean(two_points_lng$value) ## checking that indeed same as intercept

cv_repeated <- resid_sd/mean(two_points_lng$value)
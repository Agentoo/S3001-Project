cpss0 <- read.table("CPSS.txt", header = TRUE)
y <- cpss0$wage
x1 <- cpss0$edu
x2 <- cpss0$exp
x3 <- cpss0$eth
x4 <- cpss0$smsa
x5 <- cpss0$region
x6 <- cpss0$parttime
cpss <- data.frame(cbind(y,x1,x2,x3,x4,x5,x6))
plot(cpss)
plot(cpss[1:3])

# current model
mod <- lm (y ~ x1 + x2 + x3 + x4 + x5 + x6)
summary(mod)
abline(mod)
par(mfrow=c(2,2))
plot(mod)
# mod check for constant var
par(mfrow=c(1,1))
plot(mod$residuals)
# mod check for normality
par(mfrow=c(1,2))
hist(mod$residuals)
qqnorm(mod$residuals)
qqline(mod$residuals, col="red")

# Variable Selection -- Automatic method -- Foward; x1, x2, x3, x4 and x6

null <- lm(y ~ 1, data = cpss)
full <- lm(y ~ ., data = cpss)
lm(formula = y ~ ., data = cpss)
step(null,scope=list(lower=null, upper=full), direction="forward")

# new model

mod1 <- lm(y ~ x1 + x2 + x6 + x4 + x3)
summary(mod1)
abline(mod1)
par(mfrow=c(2,2))
plot(mod1)

# mod1 check for constant var
par(mfrow=c(1,1))
plot(mod1$residuals)
# mod1 check for normality
par(mfrow=c(1,2))
hist(mod1$residuals)
qqnorm(mod1$residuals)
qqline(mod1$residuals, col="red")

# transform mod1 using box-cox

library(MASS)
par(mfrow=c(1,1))
b <- boxcox(y~x1 + x2 + x6 + x4 + x3)
lambda <- b$x
like <- b$y
bc <- cbind(lambda, like)
bc[order(-like),] # here, we get lambda = 0.10101010
# new model
mod2 <- lm(y^(0.10101010) ~ x1 + x2 + x6 + x4 + x3)
summary(mod2)
abline(mod2)
par(mfrow=c(2,2))
plot(mod2)

# mod2 check for constant var
par(mfrow=c(1,1))
plot(mod2$residuals)
library(car) 
ncvTest(mod2)
spreadLevelPlot(mod2)
# mod1 check for normality
par(mfrow=c(1,2))
hist(mod2$residuals)
qqnorm(mod2$residuals)
qqline(mod2$residuals, col="red")

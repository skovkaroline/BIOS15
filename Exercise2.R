## Lecture notes example
# generate data
set.seed(85)
x <- rnorm(n=200, mean=10, sd=2)
y <- 0.4*x + rnorm(200, 0, 1)

plot(x, y, las=1,
     xlab="Leaf length (mm)",
     ylab="Leaf width (mm)")

# fit model
m <- lm(y~x)

# derive parameters
cf <- m$coef
cf

# draw out predicted values
predvals <- cf[1] + cf[2]*x

par(mfrow=c(1,2))
plot(x, y, las=1,
     xlab="Leaf length (mm)",
     ylab="Leaf width (mm)")
abline(m)
segments(x, y, x, predvals)
hist(residuals(m), xlab="", las=1)

# regression line
newx = seq(min(x), max(x), length.out=200)
predy = cf[1] + cf[2]*newx

plot(x, y, las=1,
     xlab="Leaf length (mm)",
     ylab="Leaf width (mm)")
lines(newx, predy)

summary(m)

# non-parametric bootstrapping to derive a S.E. for slope of regression line
df <- data.frame(x, y)
head(df)

cov(y,x)/var(x) # = slope of linear regression

(cf[2]*(mean(x) + sd(x))) - (cf[2]*mean(x))
# leaf width increased by 0.86 mm per s.d. increase in leaf length

cor(x,y)^2
# square of Pearson correlation coefficient = R^2

## Exercise: fitting a linear regression to real data ##
birds <- read.csv("C:/Users/skovk/OneDrive/Desktop/bird_allometry.csv")
head(birds)
str(birds)

birds$Genus_Species <- as.factor(birds$Genus_Species)
birds$Sex <- as.factor(birds$Sex)

## split data into males and females
males <- birds[birds$Sex=="m", ]
females <- birds[birds$Sex=="f", ]

## fit linear model to log-transformed brain and body sizes
mm <- lm(log(brain_mass) ~ log(body_mass), data=males)
mf <- lm(log(brain_mass) ~ log(body_mass), data=females)

## check residuals
hist(residuals(mm))
hist(residuals(mf))

## model fits
summary(mm)
# intercept = -1.906
# slope = 0.55 --> brain size increases by 5.5% per 10% increase in body size
# R-squared = 0.8345 --> 83.45% of variance in brain size explained by body mass 

summary(mf)
# intercept = -1.998
# slope = 0.575 --> brain size increases by 5.8% per 10% increase in body size
# R-squared = 0.8307

## plotting
library(ggplot2)

# males
ggplot(males, aes(log(body_mass), log(brain_mass))) + 
  geom_point() + 
  theme_bw() + 
  labs(
    x = "Log(body mass)",
    y = "Log(brain mass)"
  ) + 
  geom_smooth(method = "lm")

# females
ggplot(females, aes(log(body_mass), log(brain_mass))) + 
  geom_point() + 
  theme_bw() + 
  labs(
    x = "Log(body mass)",
    y = "Log(brain mass)"
  ) +
  geom_smooth(method = "lm")

plot(log(males$body_mass), log(males$brain_mass),
     xlab="Log(body mass)",
     ylab="Log(brain mass",
     main = "Males")
abline(mm)

plot(log(females$body_mass), log(females$brain_mass),
     xlab="Log(body mass)",
     ylab="Log(brain mass",
     main = "Females")
abline(mf)

plot(log(birds$body_mass), log(birds$brain_mass), col=birds$Sex)
abline(mm)
abline(mf)



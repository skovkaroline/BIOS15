## generate data
set.seed(100)
groups <- as.factor(rep(c("Low", "Medium", "High"), each = 50))
x <- c(rnorm(50, 10, 3), rnorm(50, 13, 3), rnorm(50,14,3))

## plot data
plot(groups, jitter(x), last=1, 
     xlab="",
     ylab="Body size (g)")
points(groups, x)

## build model 
m <- lm(x ~ groups)
anova(m)

## calculate sums of squares
SS_T <- 319.97 + 1200.43
# total sums of squares (group + residuals)

SS_T / (150-1)
# [1] 10.20403
# SST / n - 1 gives the total variance of the sample

319.97 / SS_T
# [1] 0.2104512 
# proportion of variance explained by the groups variable
# groups Sum Sq / SST = r^2

## Mean sq: variance attributable to each variable 
## F-ratio: mean sum of squares / mean residuals sum of squares
  # represents ratio of among-group variance to within-group variance
summary(m)

## Suppress intercept of model 
m2 <- lm(x ~ groups-1)
summary(m2)$coef
confint(m2)

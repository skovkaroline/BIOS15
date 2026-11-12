rm(list = ls())

## Import data  
birds <- read.csv("C:/Users/skovk/OneDrive/Desktop/bird_allometry.csv")
head(birds)
str(birds)

## Remove samples with unknown sex
birds <- birds[birds$Sex != "unknown", ]

## Set sex as factor 
birds$Sex <- as.factor(birds$Sex)

## Split data into males and females
males <- birds[birds$Sex=="m", ]
females <- birds[birds$Sex=="f", ]

## Fit linear model to log-transformed brain and body sizes
mm <- lm(log(brain_mass) ~ log(body_mass), data=males)
mf <- lm(log(brain_mass) ~ log(body_mass), data=females)

## Check residuals
hist(residuals(mm))
hist(residuals(mf))

## Check model for males 
summary(mm)
# intercept = -1.906
# slope = 0.55 --> brain size increases by 5.5% per 10% increase in body size
# R-squared = 0.8345 --> 83.45% of variance in brain size explained by body mass 

## Check model for females 
summary(mf)
# intercept = -1.998
# slope = 0.575 --> brain size increases by 5.8% per 10% increase in body size
# R-squared = 0.8307

## Plot data
library(ggplot2)

ggplot(birds, aes(log(body_mass), log(brain_mass), color = Sex)) + 
  geom_point(alpha = 0.5) + 
  scale_color_manual(values = c(f = "darkorange", m = "steelblue")) + 
  theme_classic() + 
  labs(
    x = "Log (body mass)",
    y = "Log (brain mass)"
  ) +
  geom_smooth(method = "lm") + 
  theme(legend.position = c(0.9,0.2),
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12))


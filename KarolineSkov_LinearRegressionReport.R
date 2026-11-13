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

## Summary statistics
# Means
summary(males)
m_mean_brain <- mean(males$brain_mass)
m_mean_body <- mean(males$body_mass)

summary(females)
f_mean_brain <- mean(females$brain_mass)
f_mean_body <- mean(females$body_mass)

# Standard deviations
m_sd_brain <- sd(males$brain_mass)
m_sd_body <- sd(males$body_mass)

f_sd_brain <- sd(females$brain_mass)
f_sd_body <- sd(females$body_mass)

# Min. and max. values 
m_min_brain <- min(males$brain_mass)
m_max_brain <- max(males$brain_mass)

m_min_body <- min(males$body_mass)
m_max_body <- max(males$body_mass)

f_min_brain <- min(females$brain_mass)
f_max_brain <- max(females$brain_mass)

f_min_body <- min(females$body_mass)
f_max_body <- max(females$body_mass)

# Standard errors
# males
m_se_brain <- m_sd_brain / sqrt(nrow(males))
m_se_body <- m_sd_body / sqrt(nrow(males))

# females
f_se_brain <- f_sd_brain / sqrt(nrow(females))
f_se_body <- f_sd_body / sqrt(nrow(females))

# Table of summary stats 
sum_stats <- data.frame(sex = c("male", "female"),
                    mean_brain_size = c(m_mean_brain, f_mean_brain),
                    sd_brain_size = c(m_sd_brain, f_sd_brain),
                    se_brain_size = c(m_se_brain, f_se_brain),
                    mean_body_size = c(m_mean_body, f_mean_body),
                    sd_body_size = c(m_sd_body, f_sd_body),
                    se_body_size = c(m_se_body, f_se_body),
                    min_brain = c(m_min_brain, f_min_brain),
                    max_brain = c(m_max_brain, f_max_brain),
                    min_body = c(m_min_body, f_min_body),
                    max_body = c(m_max_body, f_max_body))

sum_stats

## Fit linear model 
mm <- lm(log(brain_mass) ~ log(body_mass), data=males)
mf <- lm(log(brain_mass) ~ log(body_mass), data=females)

## Check residuals
hist(residuals(mm))
hist(residuals(mf))

## Summary for male model 
summary(mm)

# slope = 0.55 
# R-squared = 0.8345 
# standard error = 0.

## Summary for female model 
summary(mf)

# intercept = -1.998
# slope = 0.575 
# R-squared = 0.8307
# residual standard error = 0.4116

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
  scale_x_continuous(breaks = seq(0, 12, by = 2)) + 
  scale_y_continuous(limits = c(-1, 4)) + 
  theme(legend.position = c(0.9,0.2),
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12),
        panel.background = element_rect(color = "black", 
                                        fill = NA, 
                                        linewidth=0.7))


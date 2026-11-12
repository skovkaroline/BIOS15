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
m_mean_brain <- summary(males)[4,3] 
m_mean_body <- summary(males)[4,4]

summary(females)
f_mean_brain <- summary(females)[4,3]
f_mean_body <- summary(females)[4,4]


# Standard deviations
m_sd_brain <- sd(males$brain_mass)
m_sd_body <- sd(males$body_mass)

f_sd_brain <- sd(females$brain_mass)
f_sd_body <- sd(females$body_mass)

# Min. and max. values 
m_min_brain <- summary(males)[1,3]
m_max_brain <- summary(males)[6,3]

m_min_body <- summary(males)[1,4]
m_max_body <- summary(males)[6,4]

f_min_brain <- summary(females)[1,3]
f_max_brain <- summary(females)[6,3]

f_min_body <- summary(females)[1,4]
f_max_body <- summary(females)[6,4]

# Table of summary stats 
sum_stats <- data.frame(sex = c("male", "female"),
                    mean_brain_size = c(m_mean_brain, f_mean_brain),
                    mean_body_size = c(m_mean_body, f_mean_body),
                    sd_brain_size = c(m_sd_brain, f_sd_brain),
                    sd_body_size = c(m_sd_body, f_sd_body), 
                    min_brain = c(m_min_brain, f_min_brain),
                    max_brain = c(m_max_brain, f_max_brain),
                    min_body = c(m_min_body, f_min_body),
                    max_body = c(m_max_body, f_max_body))

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
  scale_x_continuous(breaks = seq(0, 12, by = 2)) + 
  scale_y_continuous(limits = c(-1, 4)) + 
  theme(legend.position = c(0.9,0.2),
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12),
        panel.background = element_rect(color = "black", fill = NA, linewidth=0.7))


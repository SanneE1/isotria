# ISSUE INDIVIDUALS (gaps in Stage information), SOLVE: 454  497 5079 5080  170  240
# setwd("C:/Users/pvitt/Dropbox/isotria_idiv")
library(dplyr)
library(tidyr)
library(readxl)
library(ggplot2)

# read data
vr <- read.csv('data/istoria_long.csv')

# survival plots ---------------------------------------------
ggplot(data = vr, 
       aes(x = log(size_t0), 
           y = surv)) +
geom_jitter(height = 0.05, 
            alpha = 0.5, 
            pch = 1) +
# Fit Logistic Regression - glm with family = "binomial"
geom_smooth(method = "glm", 
            method.args = list(family = "binomial")) +
# split in panels
facet_grid(Site ~ year_t1) +
theme_bw() +
ggtitle("surv_t1 ~ log(size_t0)") +
ggsave(filename = "results/plots_surv.png",
      dpi = 300, width = 50, height = 30, units = "cm")


# growth plots ---------------------------------------------
ggplot(data = vr, 
       aes(x = log(size_t0), 
           y = log(size_t1)) ) +
  geom_jitter(height = 0.05, 
              alpha = 0.5, 
              pch = 1) +
  # Fit Logistic Regression - glm with family = "binomial"
  geom_smooth(method = "lm") +
  # split in panels
  facet_grid(Site ~ year_t1) +
  theme_bw() +
  ggtitle("surv_t1 ~ log(size_t0)") +
  ggsave(filename = "results/plots_grow.png",
         dpi = 300, width = 50, height = 30, units = "cm")


# Flowering plots ------------------------------------------------------
ggplot(data = subset(vr, fruit_t1 != 2), 
       aes(x = log(size_t1), 
           y = fruit_t1)) +
geom_jitter(height = 0.05, 
            alpha = 0.5, 
            pch = 1) +
# Fit Logistic Regression - glm with family = "binomial"
geom_smooth(method = "glm", 
            method.args = list(family = "binomial")) +
# split in panels
facet_grid(Site ~ year_t1) +
theme_bw() +
ggtitle("reprostat1_t1 ~ log(size_t1)") +
ggsave(file = "results/plots_flow.png",
       dpi = 300, width = 50, height = 30, units = "cm")


# Dormancy plots ------------------------------------------------------
ggplot(data = vr,
       aes(x = log(size_t0), 
           y = dormancy_t1)) +
geom_jitter(height = 0.05, 
            alpha = 0.5, 
            pch = 1) +
# Fit Logistic Regression - glm with family = "binomial"
geom_smooth(method = "glm", 
            method.args = list(family = "binomial")) +
# split in panels
facet_grid(Site ~ year_t1) +
theme_bw() +
ggtitle("dormancy_t1 ~ log(size_t0)") +
ggsave(file = "results/plots_dorm.png",
       dpi = 300, width = 50, height = 30, units = "cm")


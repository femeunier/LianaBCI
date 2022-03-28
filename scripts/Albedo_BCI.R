rm(list = ls())

library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)

BCI.file <- "/home/femeunier/Documents/data/BCI/BCI_v5.1.csv"
BCI.data <- read.csv(BCI.file) %>% mutate(albedo = Rs_dn/Rs,
                                          date = as.character(date)) %>%
  mutate(date.date = as.POSIXct(date))

BCI.data.filter <- BCI.data %>% filter(albedo <= 1,
                                       FLAG == 1,
                                       Rs_dn > 0,
                                       Rs > 0) %>% mutate(year = year(date.date),
                                                          month = month(date.date))

data.month <- BCI.data.filter %>% group_by(year,month) %>% summarise(albedo = mean(albedo),
                                                                     Rs = mean(Rs),
                                                                     Rs_dn = mean(Rs_dn),
                                                                     .groups = 'keep') %>% ungroup()

data.month.long <- data.month %>% pivot_longer(cols = c(albedo,Rs,Rs_dn),
                                               names_to = "variable",
                                               values_to = "value") %>% group_by(variable) %>%
  mutate(month.num = 1:nrow(data.month))

data.seasonal <- BCI.data.filter %>% group_by(month) %>% summarise(albedo = mean(albedo),
                                                                   Rs = mean(Rs),
                                                                   Rs_dn = mean(Rs_dn),
                                                                   .groups = 'keep') %>% ungroup()

data.seasonal.long <- data.seasonal %>% pivot_longer(cols = c(albedo,Rs,Rs_dn),
                                                     names_to = "variable",
                                                     values_to = "value")



# summary(lm(data = data.month, formula = albedo ~ month.num))


ggplot(data = data.month.long,
       aes(x = month.num, y = value)) +
  geom_point() +
  stat_smooth(se = TRUE, method = "lm", fill = "lightgrey", alpha = 0.4, color = "black") +
  labs(x = "Month since July 2013", y = "") +
  facet_wrap(~ variable, scales = "free_y") +
  theme_bw()  +
  theme(text = element_text(size = 20))

ggplot(data = data.seasonal.long,
       aes(x = month, y = value)) +
  geom_line(color = "darkgrey") +
  geom_point() +
  labs(x = "Month", y = "") +
  facet_wrap(~ variable, scales = "free_y") +
  theme_bw()  +
  theme(text = element_text(size = 20))


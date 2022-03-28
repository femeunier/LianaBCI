rm(list = ls())

library(dplyr)
library(ggplot2)
library(tidyr)

system2("scp",paste("hpc:/data/gent/425/vsc42558/WVimages/*.RDS","./outputs/"))

df.all <- readRDS("./outputs/df_all.RDS")

df.Treatments <- data.frame(plot = 1:16,
                            treatment = c("c","r","r","c","r","c","r","c",
                                          "r","c","r","c","c","r","r","c"))
df.all <- df.all %>% left_join(df.Treatments,by = "plot") %>%
  mutate(time = year + (month-0.5)/12)

ggplot(data = df.all,
       aes(time,y = R,group = plot, color = as.factor(treatment))) +
  geom_line() +
  geom_point() +
  facet_wrap(~band,scales = "free") +
  theme_bw()

df.R <- readRDS("./outputs/df_R.RDS")
df.R <- df.R %>% left_join(df.Treatments,by = "plot")  %>%
  mutate(season = case_when(month >= 2 & month <= 4 ~ "dry",
                            month > 5 & month <= 11 ~ "wet",
                            TRUE ~ "Intermediary"))
df.R.select <- df.R %>% filter(year == 2017,month == 4)

ggplot(data = df.R.select %>% filter(band == 7)) +
  geom_boxplot(aes(x = as.factor(plot),y = R, fill = as.factor(treatment))) +
  theme_bw()

ggplot(data = df.R) +
  geom_boxplot(aes(x = as.factor(band),y = R, fill = as.factor(treatment))) +
  theme_bw()

df.R %>% group_by(band, treatment) %>% summarise(R = mean(R))

summary(aov(data = df.R %>% filter(band == 7),
            formula = R~treatment))

ggplot(data = df.R) +
  geom_boxplot(aes(x = as.factor(band),y = R, fill = as.factor(treatment))) +
  facet_wrap(~ season) +
  theme_bw()

ggplot(data = df.R) +
  geom_boxplot(aes(x = as.factor(band),y = R, fill = as.factor(treatment))) +
  facet_wrap(~ num) +
  theme_bw()

ggplot(data = df.R %>% filter(band == 7)) +
  geom_boxplot(aes(x = as.factor(season),y = R, fill = as.factor(treatment),group = num)) +
  theme_bw()


df.all.wide <- df.all %>% group_by(num,band,treatment,time,year,month) %>% summarise(R = mean(R),
                                                                                     .groups = "keep") %>%
  pivot_wider(values_from = R,
              names_from = treatment) %>% mutate(diff = c - r,
                                                 diff.rel = (c - r)/c)

ggplot(data = df.all.wide) +
  geom_boxplot(aes(x = as.factor(band), y = diff.rel)) +
  geom_hline(yintercept = 0,color = "red",linetype = 2) +
  theme_bw()

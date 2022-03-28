rm(list = ls())

library(dplyr)
library(ggplot2)
library(tidyr)

# system2("rsync",paste("-avz","hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/OP_ensemble_LianaBCI.RDS","./outputs/"))

OP_ensemble_LianaBCI <- readRDS("./outputs/OP_ensemble_LianaBCI.RDS")
times <- 2007 + (1:((max(OP_ensemble_LianaBCI$t)))-1)/12
OP_ensemble_LianaBCI <- OP_ensemble_LianaBCI %>% mutate(time = times[t])

best.runs <- readRDS("./outputs/best.runs.RDS")

df.data = data.frame(time = c(2007,2017),
                     nplant = c(70688,86730)/50/100/100,
                     pft = c(17,17),
                     ba = c(49,55.2)/50)

ggplot(data = OP_ensemble_LianaBCI %>% filter(pft == 17,
                                              run %in% best.runs)) +
  geom_line(aes(x = time, y = nplant, group = run)) +
  geom_point(data = df.data,
             aes(x = time, y = nplant), color = 'red') +
  theme_bw()


ggplot(data = OP_ensemble_LianaBCI %>% filter(pft == 17,
                                              run %in% best.runs)) +
  geom_line(aes(x = time, y = ba, group = run)) +
  geom_point(data = df.data,
             aes(x = time, y = ba), color = 'red') +
  theme_bw()


ggplot(data = OP_ensemble_LianaBCI %>% ungroup() %>% filter(pft == 17,
                                                            time == 2017) ) +
  geom_point(aes(x = nplant, y = ba), color = 'black') +
  geom_point(data = OP_ensemble_LianaBCI %>% ungroup() %>% filter(pft == 17,
                                                                  time == 2017,
                                                                  run %in% best.runs),
             aes(x = nplant, y = ba), color = 'red') +
  geom_hline(yintercept = df.data$ba[2],linetype = 2) +
  geom_vline(xintercept = df.data$nplant[2],linetype = 2) +
  theme_bw()





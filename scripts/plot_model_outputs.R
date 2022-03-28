rm(list = ls())

library(ggplot2)
library(dplyr)
library(tidyr)

system2("rsync",paste("-avz",
                      "hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/df.LianaBCIpatch.ensembles.RDS",
                      "./outputs/"))

best.runs <- readRDS("./outputs/best.runs.RDS")

OP.ED2 <- readRDS("./outputs/df.LianaBCIpatch.ensembles.RDS") %>% mutate(time = year + (month - 1)/12) %>%
  mutate(fraction.gpp = GPP_liana/GPP_eco,
         fraction.lai = LAI_liana/LAI_eco)

OP.ED2.long <-
  OP.ED2 %>%
  ungroup() %>%
  pivot_longer(cols = - c(name,num,type,year,month,time),
                          names_to = "var",
                          values_to = "value")
OP.ED2.sum <- OP.ED2.long %>%
  filter(num %in% best.runs) %>%
  group_by(type,time,year,month,var) %>%
  summarise(value.m = mean(value),
            value.sd = sd(value),
            .groups = "keep")

var2plot = "LAI_eco"

ggplot(data = OP.ED2.sum %>% filter(var == var2plot)) +
  geom_ribbon(aes(x = time, y =  value.m,
                  ymin = value.m - value.sd, ymax = value.m + value.sd),
              alpha = 0.2) +
  geom_line(aes(x = time, y =  value.m)) +
  geom_line(data = OP.ED2.long %>% filter(num %in% best.runs,
                                   var == var2plot),
            aes(x = time, y =  value, group = as.factor(num)),
            color = "black",size = 0.1) +
  facet_wrap(~type) +
  theme_bw()


OP.ED2.sum %>% group_by(year,type,var) %>%
  summarise(m = mean(value.m),
            .groups = "keep") %>%
  filter(year %in% c(2007,2016),
         var %in% c("fraction.gpp","fraction.lai")) %>%
  arrange(var,type,year) %>% pivot_wider(names_from = year,
                                         values_from = m) %>%
  mutate(change = `2016` - `2007`,
         rel.change = 100*(`2016` - `2007`)/`2007`)


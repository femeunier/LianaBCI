rm(list = ls())

library(dplyr)
library(ggplot2)
library(tidyr)

datum.file <- "/kyukon/scratch/gent/vo/000/gvo00074/felicien/LianaBCI/out/Ensemble.43/analy/BCI..RData"
local.datum.file <- file.path(getwd(),"outputs")
system2("rsync",paste("-avz",paste0("hpc:",datum.file),local.datum.file))

load(file.path(local.datum.file,"BCI..RData"))

dbh.min_liana = 0.25 ; dbh.min_tree = 10
df.OP <- df.OP.cat <- data.frame()

for (it in seq(1,length(datum$year))){
  ipa = datum$cohort$ipa[[it]]
  area = datum$cohort$area[[it]]
  pft = datum$cohort$pft[[it]]
  nplant = datum$cohort$nplant[[it]]/datum$cohort$area[[it]]
  dbh = datum$cohort$dbh[[it]]
  ba = datum$cohort$ba[[it]] /datum$cohort$area[[it]]

  ba[dbh < 1 & dbh >= dbh.min_liana & pft == 17] <- (1.5**2)*nplant[dbh < 1 & dbh >= dbh.min_liana & pft == 17]*pi/4
  dbh[dbh < 1 & dbh >= dbh.min_liana & pft == 17] <- 1.5

  cdf.cat <- data.frame(ipa, pft, nplant, area, dbh, ba) %>% filter((dbh >= dbh.min_liana & pft == 17) |
                                                                      dbh >= dbh.min_tree) %>%
    mutate(DBH.cat = case_when(pft != 17 ~ 0,
                               dbh < 2 ~ 1,
                               dbh < 3 ~ 2,
                               dbh < 5 ~ 3,
                               dbh < 10 ~ 4,
                               TRUE ~ 5)) %>%
    group_by(ipa,DBH.cat,pft) %>% summarise(nplant = sum(nplant),
                                            ba = sum(ba),
                                            dbh = mean(dbh),
                                            area = area[1],
                                            .groups = 'keep') %>%
    group_by(pft,DBH.cat) %>% summarise(nplant = weighted.mean(nplant, area),
                                        ba = weighted.mean(ba, area),
                                        dbh = weighted.mean(dbh, area),
                                        .groups = "keep") %>% mutate(time = 2007 + (it-1)/12,
                                                                     t = it)

  df.OP.cat <- bind_rows(list(df.OP.cat,
                              cdf.cat))

  df.OP <- bind_rows(list(df.OP,
                          cdf.cat %>%   group_by(time,t,pft) %>% summarise(nplant = sum(nplant),
                                                                           ba = sum(ba),
                                                                           dbh = sum(dbh),
                                                                           .groups = "keep")))



}

ggplot(data = df.OP.cat %>% filter(pft == 17,
                                   time %in% c(2007,2017)),
       aes(x = as.factor(DBH.cat), y = nplant*10000*50,fill = as.factor(t))) +
  geom_bar(stat = "identity",position="dodge") +
  scale_x_discrete(breaks = 1:5,
                   labels = c("1-2cm","2-3cm","3-5cm","5-10cm",">10cm")) +
  labs(x = "", y = "# Ind", fill = "Census") +
  scale_fill_manual(values = c("red","blue")) +
  theme_bw()

ggplot(data = df.OP.cat %>% filter(pft == 17,
                                   time %in% c(2007,2017)),
       aes(x = as.factor(DBH.cat), y = ba,fill = as.factor(t))) +
  geom_bar(stat = "identity",position="dodge") +
  scale_x_discrete(breaks = 1:5,
                   labels = c("1-2cm","2-3cm","3-5cm","5-10cm",">10cm")) +
  labs(x = "", y = "BA", fill = "Census") +
  scale_fill_manual(values = c("red","blue")) +
  theme_bw()


df.data = data.frame(time = c(2007,2017),
                     nplant = c(70688,91719)/50/100/100,
                     pft = c(17,17),
                     BA = c(49,55.2)/50)

ggplot() +
  geom_line(data = df.OP,
            aes(x = time, y = nplant)) +
  geom_point(data = df.data,
            aes(x = time, y = nplant), color = 'red') +
  facet_wrap(~ pft,scales = "free") +
  theme_bw()


ggplot() +
  geom_line(data = df.OP,
            aes(x = time, y = ba)) +
  geom_point(data = df.data,
             aes(x = time, y = BA), color = 'red') +
  facet_wrap(~ pft,scales = "free") +
  theme_bw()

matplot(datum$year + datum$month/12,datum$szpft$lai[,12,c(2,3,4,17,18)],type = 'l',col = c("lightgreen","green","darkgreen","darkblue","black"))


plot(datum$year + datum$month/12,datum$szpft$lai[,12,17]/datum$emean$lai,type = 'l')

plot(datum$year + datum$month/12,datum$szpft$gpp[,12,17]/datum$emean$gpp,type = 'l')
pos = datum$month %in% c(1,2,3,4)
contri <- datum$szpft$gpp[,12,17]/datum$emean$gpp
contri[!pos] <- NA
lines(datum$year + datum$month/12,contri,type = 'l',col = 'red')


ggplot(data = data.frame(time = datum$year + datum$month/12,
                         albedo = datum$emean$albedo,
                         gpp_liana = datum$szpft$gpp[,12,17],
                         lai_liana = datum$szpft$lai[,12,17],
                         lai = datum$emean$lai),
       aes(x = time, y = albedo)) +
  geom_point() +
  geom_line(color = "darkgrey") +
  stat_smooth(method = "lm", color = "black") +
  labs(x = "", y = "Albedo") +
  theme_bw()


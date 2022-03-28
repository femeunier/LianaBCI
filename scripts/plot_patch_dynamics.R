rm(list = ls())

library(dplyr)
library(ggplot2)

datum.file <- "/kyukon/scratch/gent/vo/000/gvo00074/felicien/LianaBCI/out/analy/BCI.RData"
datum.file <- "/kyukon/scratch/gent/vo/000/gvo00074/felicien/LianaBCI/out/Ensemble.43/analy/BCI..RData"

local.datum.file <- file.path(getwd(),"outputs")
system2("rsync",paste("-avz",paste0("hpc:",datum.file),local.datum.file))

load("./outputs/BCI..RData")

patch <- datum$patch
cohorts <- datum$cohort

t = length(patch$gpp)

patch.gpp <- patch$gpp[[t]]
patch.npp <- patch$npp[[t]]
patch.age <- patch$age[[t]]
patch.lai <- patch$lai[[t]]

cohort.area <- cohorts$area[[t]]
cohort.ipa <- cohorts$ipa[[t]]
cohort.ico <- cohorts$ico[[t]]
cohort.dbh <- cohorts$dbh[[t]]
cohort.gpp <- cohorts$gpp[[t]]
cohort.npp <- cohorts$npp[[t]]
cohort.lai <- cohorts$lai[[t]]
cohort.pft <- cohorts$pft[[t]]
cohort.nplant <- cohorts$nplant[[t]]/cohort.area
cohort.hite <- cohorts$height[[t]]

dbh.threshold <- 1 ; dbh.large <- 3 ; dbh.verylarge <- 10

df <- data.frame(pft = cohort.pft,
                 nplant = cohort.nplant,
                 area = cohort.area,
                 ipa = cohort.ipa,
                 lai = cohort.lai,
                 hite = cohort.hite,
                 gpp = cohort.gpp,
                 npp = cohort.npp,
                 dbh = cohort.dbh) %>% ungroup() %>%
  group_by(ipa,pft) %>% summarise(gpp = sum(nplant*gpp),
                                  npp = sum(nplant*npp),
                                  hite = max(hite),
                                  lai = sum(lai),
                                  nverylarge = sum(nplant[dbh >= dbh.verylarge]),
                                  nlarge = sum(nplant[dbh >= dbh.large]),
                                  nplant = sum(nplant[dbh >= dbh.threshold]),
                                  area = cohort.area[1],
                                  .groups = "keep")

patch.gpp.liana <- df %>% filter(pft == 17) %>% pull(gpp)
patch.npp.liana <- df %>% filter(pft == 17) %>% pull(npp)
patch.lai.liana <- df %>% filter(pft == 17) %>% pull(lai)
patch.nplant.liana <- df %>% filter(pft == 17) %>% pull(nplant)
patch.nlarge.liana <- df %>% filter(pft == 17) %>% pull(nlarge)
patch.nverylarge.liana <- df %>% filter(pft == 17) %>% pull(nverylarge)
patch.height <- df %>% group_by(ipa) %>% summarise(hite = max(hite),
                                                   .groups = "keep") %>% pull(hite)


ggplot(data = df) +
  geom_point(aes(x = ipa, y = hite, color = as.factor(pft))) +
  theme_bw()


df %>% group_by(ipa) %>% summarise(diff = hite[pft == 17] - max(hite[pft !=17])) %>% pull(diff) %>% plot()

plot(patch.age,patch.nplant.liana)

plot(patch.age,patch.gpp.liana/patch.gpp)
plot(patch.age,patch.gpp)
plot(patch.age,patch.lai.liana)
plot(patch.age,patch.lai.liana/patch.lai)


plot(patch.age,patch.height)
plot(patch.height,patch.lai.liana)


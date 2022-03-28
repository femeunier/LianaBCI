rm(list = ls())

library(dplyr)
library(tidyr)
library(purrr)

ensemble.size = 50

# Multi jobs
Nsimuperjob = 2
isimu = 0

rundir <- "/kyukon/scratch/gent/vo/000/gvo00074/felicien/LianaBCI/run"
outdir <- "/kyukon/scratch/gent/vo/000/gvo00074/felicien/LianaBCI/out/"

dbh.min_liana = 1 ; dbh.min_tree = 10
df.all <- df.all.cat <- data.frame()

df.state.vars <- data.frame()

for (iens in seq(1,ensemble.size)){

  print(iens/ensemble.size)
  run_name <- paste0("Ensemble.",iens)

  isimu = isimu + 1

  run_ref <- file.path(rundir,run_name)
  out_ref <- file.path(outdir,run_name)

  analy_folder <- file.path(out_ref,"analy")
  analy_file <- file.path(analy_folder,"BCI..RData")

  config.file <- file.path(run_ref,"config.xml")

  if (file.exists(analy_file)){

    load(analy_file)

    df.OP <- df.OP.cat <- data.frame()

    df.state.vars <- bind_rows(list(df.state.vars,
                                    data.frame(time = datum$year + datum$month/12,
                                               albedo = datum$emean$albedo,
                                               gpp = datum$emean$gpp,
                                               gpp_liana = datum$szpft$gpp[,12,17],
                                               lai = datum$emean$lai,
                                               lai_liana = datum$szpft$lai[,12,17]) %>% mutate(run = iens)
                                    )
                               )

    for (it in seq(1,length(datum$year),12)){
      ipa = datum$cohort$ipa[[it]]
      area = datum$cohort$area[[it]]
      pft = datum$cohort$pft[[it]]
      nplant = datum$cohort$nplant[[it]]/datum$cohort$area[[it]]
      dbh = datum$cohort$dbh[[it]]
      ba = datum$cohort$ba[[it]] /datum$cohort$area[[it]]

      # ba[dbh < 1 & dbh >= dbh.min_liana & pft == 17] <- (1.5**2)*nplant[dbh < 1 & dbh >= dbh.min_liana & pft == 17]*pi/4
      # dbh[dbh < 1 & dbh >= dbh.min_liana & pft == 17] <- 1.5

      cdf.cat <- data.frame(ipa, pft, nplant, area, dbh, ba) %>% filter((dbh >= dbh.min_liana & pft == 17) |
                                                                          dbh >= dbh.min_tree) %>%
        mutate(DBH.cat = case_when(pft == 17 & dbh < 1 ~ 0,
                                   pft == 17 & dbh < 2 ~ 1,
                                   pft == 17 & dbh < 3 ~ 2,
                                   pft == 17 & dbh < 5 ~ 3,
                                   pft == 17 & dbh < 10 ~ 4,
                                   pft == 17 ~ 5,
                                   TRUE ~ 0)) %>%
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

    df.all <- bind_rows(list(df.all,
                             df.OP %>% mutate(run = iens)))

    df.all.cat <- bind_rows(list(df.all.cat,
                                 df.OP.cat %>% mutate(run = iens)))
  }

}

saveRDS(df.all,"./OP_ensemble_LianaBCI.RDS")
saveRDS(df.all.cat,"./OP_cat_ensemble_LianaBCI.RDS")
saveRDS(df.state.vars,"./OP_ED2_ensemble_LianaBCI.RDS")

# scp /home/femeunier/Documents/projects/LianaBCI/scripts/analyze_ensemble_LianaBCI.R hpc:/data/gent/vo/000/gvo00074/felicien/R

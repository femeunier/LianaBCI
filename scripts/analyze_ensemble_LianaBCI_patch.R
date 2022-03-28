rm(list = ls())

library(dplyr)
library(tidyr)
library(PEcAnRTM)
library(purrr)
library(ED2scenarios)
library(PEcAn.ED2)
library(purrr)
library(ggplot2)
library(ggridges)
library(cowplot)
library(pracma)
library(BayesianTools)
library(raster)
library(stringr)
library(LidarED)

ref_dir <- "/kyukon/scratch/gent/vo/000/gvo00074/felicien/LianaBCI"
rundir <- "/kyukon/scratch/gent/vo/000/gvo00074/felicien/LianaBCI/run"
outdir <- "/kyukon/scratch/gent/vo/000/gvo00074/felicien/LianaBCI/out/"
ICdir <- "/kyukon/scratch/gent/vo/000/gvo00074/felicien/LianaBCI/inputs/"

types <- c("High","gap","recent.gap","former.gap")

dbh.min_liana = 1 ; dbh.min_tree = 10
df.all <- df.all.cat <- data.frame()

# Main loop
ensemble.size = 50

df.simu <- data.frame()
df.params.all <- data.frame()


for (iens in seq(1,ensemble.size)){

  print(iens/ensemble.size)

  run_name <- paste0("Ensemble.",iens)
  run_ref <- file.path(rundir,run_name)
  out_ref <- file.path(outdir,run_name)

  for (itype in seq(1,length(types))){

    #####################################################################################
    # Analy files
    datum.file <- file.path(outdir,run_name,"analy",paste0("BCI.",types[itype],".RData"))

    if(file.exists(datum.file)){
      load(datum.file)

      df.OP <- df.OP.cat <- data.frame()

      years <- datum$year
      months <- datum$month

      patch <- datum$patch
      cohorts <- datum$cohort

      t = length(patch$gpp)

      df <- data.frame()

      for (it in c(seq(1,length(datum$year),12),length(datum$year))){

        ipa = datum$cohort$ipa[[it]]
        area = datum$cohort$area[[it]]
        pft = datum$cohort$pft[[it]]
        nplant = datum$cohort$nplant[[it]]/datum$cohort$area[[it]]
        dbh = datum$cohort$dbh[[it]]
        ba = datum$cohort$ba[[it]] /datum$cohort$area[[it]]

        cdf.cat <- data.frame(ipa, pft, nplant, area, dbh, ba) %>%
          filter((dbh >= dbh.min_liana & pft == 17) |
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
                                cdf.cat %>% group_by(time,t,pft) %>% summarise(nplant = sum(nplant),
                                                                               ba = sum(ba),
                                                                               .groups = "keep")))

      }

      df.all <- bind_rows(list(df.all,
                               df.OP %>% mutate(num = iens,
                                                type = types[itype])
      ))

      n <- interp1((datum$year + (datum$month-1)/12)[c(seq(1,length(datum$year),12),length(datum$year))],
                   df.OP %>% group_by(t) %>% summarise(nplant = sum(nplant),
                                                       .groups = "keep") %>% pull(nplant),
                   (datum$year + (datum$month-1)/12))

      n.liana <- interp1((datum$year + (datum$month-1)/12)[c(seq(1,length(datum$year),12),length(datum$year))],
                         df.OP %>% filter(pft == 17) %>% pull(nplant),
                         (datum$year + (datum$month-1)/12))

      gpp <- datum$emean$gpp
      gpp.liana <- datum$szpft$gpp[,12,17]

      lai <- datum$emean$lai
      lai.liana <- datum$szpft$lai[,12,17]

      ba <- datum$szpft$ba[,12,18]
      ba.liana <- datum$szpft$ba[,12,17]

      agb <- datum$emean$agb
      agb.liana <- datum$szpft$agb[,12,17]

    } else{
      years <- months <- gpp <- lai <- agb <- ba <- n <-
        gpp.liana <- lai.liana <- agb.liana <- ba.liana <- n.liana <- NA
    }


    df.all.cat <- bind_rows(list(df.all.cat,
                                   df.OP.cat %>% mutate(num = iens,
                                                        type = types[itype])
                                 ))

    df.simu <- bind_rows(list(df.simu,
                              data.frame(name = run_name,
                                         num = iens,
                                         type = types[itype],
                                         year = years,
                                         month = months,
                                         density_eco = n,
                                         density_liana = n.liana,
                                         GPP_eco = gpp,
                                         GPP_liana = gpp.liana,
                                         LAI_eco = lai,
                                         LAI_liana = lai.liana,
                                         BA_eco = ba,
                                         BA_liana = ba.liana,
                                         AGB_eco = agb,
                                         AGB_liana = agb.liana)
                              )
                         )

  }

  #####################################################################################
  # Config file
  # Config file
  config.file <- file.path(rundir,run_name,"config.xml")

  df.params <- data.frame()
  if (file.exists(config.file)){
    for (pft in c(2,3,4,17)){

      df.params <- bind_rows(list(df.params,
                                  data.frame(param = "Vm0",
                                             value = get_ED_default_pft(config.file,"Vm0",pft),
                                             PFT = pft)))

      if (pft == 17){
        df.params <- bind_rows(list(df.params,
                                    data.frame(param = c("stomatal_slope","stoma_psi_b","seed_rain","repro_min_h","clumping_factor","seedling_mortality","r_fract","mort3","mort1","SLA"),
                                               value = c(get_ED_default_pft(config.file,"stomatal_slope",pft),
                                                         get_ED_default_pft(config.file,"stoma_psi_b",pft),
                                                         get_ED_default_pft(config.file,"seed_rain",pft),
                                                         get_ED_default_pft(config.file,"repro_min_h",pft),
                                                         get_ED_default_pft(config.file,"clumping_factor",pft),
                                                         get_ED_default_pft(config.file,"seedling_mortality",pft),
                                                         get_ED_default_pft(config.file,"r_fract",pft),
                                                         get_ED_default_pft(config.file,"mort3",pft),
                                                         get_ED_default_pft(config.file,"mort1",pft),
                                                         get_ED_default_pft(config.file,"SLA",pft))) %>% mutate(PFT = pft)
        ))
      }
    }

    df.params.all <- bind_rows(list(df.params.all,
                                    df.params %>% mutate(run = iens)))
  }
}

saveRDS(df.params.all,"df.LianaBCIpatch.params.RDS")
saveRDS(df.all,"./OP_patch_ensemble_LianaBCI.RDS")
saveRDS(df.all.cat,"./OP_patch_cat_ensemble_LianaBCI.RDS")
saveRDS(df.simu,"df.LianaBCIpatch.ensembles.RDS")

# scp /home/femeunier/Documents/projects/LianaBCI/scripts/analyze_ensemble_LianaBCI_patch.R hpc:/data/gent/vo/000/gvo00074/felicien/R


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

ref_dir <- "/kyukon/scratch/gent/vo/000/gvo00074/felicien/LianaBCI"
rundir <- "/kyukon/scratch/gent/vo/000/gvo00074/felicien/LianaBCI/run"
outdir <- "/kyukon/scratch/gent/vo/000/gvo00074/felicien/LianaBCI/out/"

ed2in_files <- c("ED2IN")

TREEFALL_DISTURBANCE_RATES <- 0.012

list_dir <- list()

# Lianas

pft_lowers_tree <- c(mult.Vm0 = 0.5)
pft_uppers_tree <- c(mult.Vm0 = 2.5)
prior_tree <- map2(pft_lowers_tree,pft_uppers_tree,createUniformPrior)
param_names_tree <- names(pft_lowers_tree)
Nparam_tree <- length(param_names_tree)

pft_lowers <- c(stomatal_slope = 5,
                Vm0 = 5,
                stoma_psi_b = 50,
                clumping_factor = 0.4,
                seedling_mortality = 0.95,
                r_fract = 0.5)

pft_uppers <- c(stomatal_slope = 20,
                Vm0 = 60,
                stoma_psi_b = 300,
                clumping_factor = 0.9,
                seedling_mortality = 0.99,
                r_fract = 0.9)

prior <- map2(pft_lowers,pft_uppers,createUniformPrior)

param_names <- names(pft_lowers)
Nparam <- length(param_names)

# Config file
PREFIX_XML <- "<?xml version=\"1.0\"?>\n<!DOCTYPE config SYSTEM \"ed.dtd\">\n"
defaults <- list_dir <- list()

# Default settings
settings <- list()

settings[["BCI"]] <- list(model = list(revision = "git",
                                           config.header = NULL),
                              pfts = list(pft = list(num = 2,
                                                     ed2_pft_number = 2,
                                                     name = "Early"),
                                          pft = list(num = 3,
                                                     ed2_pft_number = 3,
                                                     name = "Mid"),
                                          pft = list(num = 4,
                                                     ed2_pft_number = 4,
                                                     name = "Late"),
                                          pft = list(num = 17,
                                                     ed2_pft_number = 17,
                                                     name = "Liana")))


# Default config
config.all <- list()
config.all[["BCI"]] <- list()

config.all[["BCI"]][["Early"]] <- unlist(list(num = 2,
                                              Vcmax = 15*2.4,
                                              Vm0 = 15*2.4,
                                              clumping_factor = 0.7,
                                              leaf_trans_vis = 0.02,
                                              leaf_trans_nir = 0.33,
                                              leaf_reflect_vis = 0.05,
                                              leaf_reflect_nir = 0.38,
                                              orient_factor = -0.42))

config.all[["BCI"]][["Mid"]] <- unlist(list(num = 3,
                                            Vcmax = 10*2.4,
                                            Vm0 = 10*2.4,
                                            clumping_factor = 0.7,
                                            leaf_trans_vis = 0.02,
                                            leaf_trans_nir = 0.33,
                                            leaf_reflect_vis = 0.05,
                                            leaf_reflect_nir = 0.38,
                                            orient_factor = -0.42))

config.all[["BCI"]][["Late"]] <- unlist(list(num = 4,
                                             Vcmax = 5*2.4,
                                             Vm0 = 5*2.4,
                                             clumping_factor = 0.7,
                                             leaf_trans_vis = 0.02,
                                             leaf_trans_nir = 0.33,
                                             leaf_reflect_vis = 0.05,
                                             leaf_reflect_nir = 0.38,
                                             orient_factor = -0.42))

config.all[["BCI"]][["liana"]] <- unlist(list(num = 17,
                                                is_tropical=1,
                                                rho = 0.545942109450698,
                                                wood_Kexp = 2.00232536884098,
                                                b2Bs_large = 2.59538316642866,
                                                Vcmax = 12.7492298307518*2.4,
                                                wood_Kmax = 0.050239066507477,
                                                wood_water_cap = 0.00741320978983394*1000,
                                                b2Bl_large = 1.89671084557194,
                                                b1Bl_large = 0.0781732775329146,
                                                b1Bs_large = 0.274302877607988,
                                                wood_psi50 = 150.071936406719,
                                                growth_resp_factor = 0.35091178560855,
                                                SLA = 21.0913259640397/2,
                                                stoma_psi_b = -159.867138285947,
                                                root_respiration_factor = 0.280948147163726,
                                                b2Ht = 0.86775886967174,
                                                SRA = 48.1415201034397,
                                                r_fract = 0.823630078014685,
                                                stomatal_slope = 8.9609375084677,
                                                root_beta = 0.0496031290228129,
                                                b1Ht = 0.100234269234352,
                                                q = 0.992763117421418,
                                                mort1 = 0.,
                                                mort2 = 15.2909460115439,
                                                leaf_turnover_rate = 1.85036229281686,
                                                root_turnover_rate = 1.27003195811593,
                                                stoma_psi_c = 3.01433325046673,
                                                dark_respiration_factor = 0.0127936573754337,
                                                quantum_efficiency = 0.0686709659197022,
                                                mort3 = 0.045 - TREEFALL_DISTURBANCE_RATES,
                                                leaf_psi_tlp = 225.082639101945,
                                                leaf_water_cap = 0.000749843866562104*1000,
                                                Vm0 = 12.7492298307518*2.4,
                                                clumping_factor = 0.4,
                                                seedling_mortality = 0.98,
                                                leaf_trans_vis = 0.03,
                                                leaf_trans_nir = 0.35,
                                                leaf_reflect_vis = 0.06,
                                                leaf_reflect_nir = 0.36,
                                                orient_factor = 0.333))



# Main loop
ensemble.size = 100

# Multi jobs
Nsimuperjob = 2
isimu = 0

for (iens in seq(1,ensemble.size)){

  print(iens/ensemble.size)
  # Config

  # Sample Tree
  pft_samples_tree <- map(1:length(prior_tree), function(i){
    samples <- prior_tree[[i]]$sample()
    names(samples) <- names(pft_lowers_tree[[i]])
    return(samples)}) %>% set_names(names(prior_tree))

  # Sample
  pft_samples <- map(1:length(prior), function(i){
    samples <- prior[[i]]$sample()
    names(samples) <- names(pft_lowers[[i]])
    return(samples)}) %>% set_names(names(prior))

  run_name <- paste0("Ensemble.",iens)

  isimu = isimu + 1

  run_ref <- file.path(rundir,run_name)
  out_ref <- file.path(outdir,run_name)

  if(!dir.exists(run_ref)) dir.create(run_ref)
  if(!dir.exists(out_ref)) dir.create(out_ref)
  if(!dir.exists(file.path(out_ref,"analy"))) dir.create(file.path(out_ref,"analy"))
  if(!dir.exists(file.path(out_ref,"histo"))) dir.create(file.path(out_ref,"histo"))

  # ED2IN
  ed2in_scenar <- read_ed2in(file.path(ref_dir,ed2in_files))

  ed2in_scenar$IEDCNFGF <- file.path(run_ref,"config.xml")
  ed2in_scenar$FFILOUT <- file.path(out_ref,"analy",paste0("BCI."))
  ed2in_scenar$SFILOUT <- file.path(out_ref,"histo",paste0("BCI."))

  ed2in_scenar$IMONTHZ <- 2
  ed2in_scenar$IDATEZ <- 1
  ed2in_scenar$IYEARZ <- 2020

  ed2in_scenar$IQOUTPUT <- 3
  ed2in_scenar$ITOUTPUT <- 0
  ed2in_scenar$TREEFALL_DISTURBANCE_RATE <- TREEFALL_DISTURBANCE_RATES

  write_ed2in(ed2in_scenar,filename = file.path(run_ref,"ED2IN"))

  if (isimu == 1){
    isfirstjob = TRUE
    dir_joblauncher = run_ref
    list_dir[[run_name]] = run_ref
  } else{
    isfirstjob = FALSE
  }

  # Config
  config_simu <- config.all[["BCI"]]

  # Tree
  for (i in seq(1,Nparam_tree)){
    param_name <- param_names_tree[i]
    params_actual <- pft_samples_tree[[param_name]]

    for (ipft in seq(1,3)){
      if (param_name == "mult.Vm0"){
        config_simu[[ipft]][c("Vm0","Vcmax")] <- params_actual*config.all[["BCI"]][[ipft]]["Vm0"]
      } else{
        config_simu[[ipft]][param_name] <- params_actual
      }
    }
  }


   # Liana
  for (i in seq(1,Nparam)){
    param_name <- param_names[i]
    params_actual <- pft_samples[[param_name]]

    if (param_name == "Vm0"){
      config_simu[[4]][c(param_name,"Vcmax")] <- params_actual
    } else{
      config_simu[[4]][param_name] <- params_actual
    }
  }

  xml <- write.config.xml.ED2(defaults = defaults,
                              settings = settings[["BCI"]],
                              trait.values = config_simu)

  XML::saveXML(xml, file = file.path(run_ref,paste0("config.xml")), indent = TRUE,
               prefix = PREFIX_XML)

  # job.sh
  write_joblauncher(file =  file.path(dir_joblauncher,"job_BCI.sh"),
                    nodes = 1,ppn = 18,mem = 16,walltime = 24,
                    prerun = "ml purge ; ml UDUNITS/2.2.26-intel-2018a R/3.4.4-intel-2018a-X11-20180131 HDF5/1.10.1-intel-2018a; ulimit -s unlimited",
                    CD = run_ref,
                    ed_exec = "/user/scratchkyukon/gent/gvo000/gvo00074/felicien/ED2/ED/build/ed_2.1-opt-master-2bb6872",
                    ED2IN = "ED2IN",
                    firstjob = isfirstjob,clean = TRUE)

  if (isimu == Nsimuperjob){
    isimu = 0
  }

}


dumb <- write_bash_submission(file = file.path(rundir,"all_jobs.sh"),
                              list_files = list_dir,
                              job_name = "job_BCI.sh")

# scp /home/femeunier/Documents/projects/LianaBCI/scripts/generate_ensemble_LianaBCI.R hpc:/data/gent/vo/000/gvo00074/felicien/R

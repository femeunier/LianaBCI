rm(list = ls())

library(ggplot2)
library(dplyr)
library(LianaBCI)
library(BCI.AGB)

data.quadrant <- read.csv("./data/forest_scenarios_felicien.csv")

ggplot(data = data.quadrant) +
  geom_bar(aes(x = as.factor(valu))) +
  theme_bw()

delta_X = 20 ; delta_Y = 20

data.quadrant.mod <- data.quadrant %>% mutate(x = delta_X/2 + horz*delta_X,
                                              y = delta_Y/2 + vert*delta_Y,
                                              patch_x = horz + 1,
                                              patch_y = vert + 1)

# 0 = Undisturbed High-Canopy, 1 = Persistent Canopy Gaps, 2 = Recent Canopy Gaps, 3 = Former Canopy Gaps, and 4 = Unused Quadrats

data.file <- "/home/femeunier/Documents/projects/LianaBCI/data/2021-09-22_liana_bci_data_V9_0_felicien.csv"
data <- read.csv(data.file)

data.filtered <- data %>% filter(DBH >= 10)

Delta_X = Delta_Y = 20
data.patch <- data.filtered %>% filter(PX >= 0 & PX < 1000 &
                                         PY >= 0 & PY < 500,
                                       Status %in% c("L","N")) %>% ungroup() %>%
  mutate(patch = patchnumber_from_position(PX,PY,
                                           patch_X = Delta_X,
                                           patch_Y = Delta_Y,
                                           extr_x = c(0,1000),
                                           extr_y = c(0,500))[["patch"]])



data.quadrant <- read.csv("./data/forest_scenarios_felicien.csv") %>%
  mutate(patch = horz + 1 + vert*50) %>%
  dplyr::select(patch,valu)


data.patch.merged <- data.patch %>%
  left_join(data.quadrant,
            by = "patch")

data.patch.merged.summ <- data.patch.merged %>% group_by(Census,patch,valu) %>% summarise(N = length(DBH)/(Delta_X*Delta_Y),
                                                                                          DBH.mean = mean(DBH,na.rm = TRUE),
                                                                                          BA = sum(DBH/10*DBH/10*pi/4)/(Delta_X*Delta_Y),
                                                                                          .groups = "keep")


ggplot(data = data.patch.merged.summ %>% filter(valu < 4)) +
  geom_boxplot(aes(x = as.factor(valu), fill = as.factor(Census), y = BA)) +
  theme_bw()

ggplot(data = data.patch.merged.summ %>% filter(valu < 4)) +
  geom_boxplot(aes(x = as.factor(valu), fill = as.factor(Census), y = DBH.mean)) +
  theme_bw()

ggplot(data = data.patch.merged.summ %>% filter(valu < 4)) +
  geom_boxplot(aes(x = as.factor(valu), fill = as.factor(Census), y = N)) +
  theme_bw()

lianas <- data.patch.merged %>% filter(valu < 4,
                                       Census == 1) %>%
  dplyr::select(LianaID,StemID,PX,PY,patch,valu,DBH)


###############################################################################################################################

WD.Panama <- read.csv("./data/WD.DB.csv") %>% mutate(Latin = paste(genus,species),
                                                     WD = WD..g.cm3.) %>% dplyr::select(Latin,WD)

species.WD <- BCI.AGB::bci.spptable %>% dplyr::select(sp,Latin) %>% ungroup() %>% left_join(WD.Panama %>% ungroup(),
                                                                                            by = "Latin") %>% group_by(sp) %>%
  summarise(Latin = Latin[1],
            WD = mean(WD))

species.WD.NA <- species.WD %>%
  mutate(WD.type = case_when(is.na(WD) ~ "Average",
                             TRUE ~ "DB"),
         WD = case_when(is.na(WD) ~ mean(species.WD$WD,na.rm = TRUE),
                        TRUE ~ WD))


tree.BCI.WD <- tree.BCI %>% filter(census.time == 2010,
                                   DFstatus == "alive",
                                   !is.na(dbh), dbh > 0,
                                   gx <= 1000,gx >= 0,
                                   gy <= 500,gx >= 0) %>% left_join(species.WD.NA,
                                                                    by = "sp") %>%
  mutate(WD.type = case_when(is.na(WD) ~ "Average2",
                             TRUE ~ WD.type),
         WD = case_when(is.na(WD) ~ mean(species.WD$WD,na.rm = TRUE),
                        TRUE ~ WD))

tree.BCI.WD %>% group_by(WD.type) %>% summarise(N = length(WD))

hist(tree.BCI.WD %>% filter(WD.type == "DB") %>% pull(WD))

Delta_X = Delta_Y = 20
patches <- BCI.AGB::patchnumber_from_position(tree.BCI.WD$gx,
                                              tree.BCI.WD$gy,
                                              patch_X = Delta_X,
                                              patch_Y = Delta_Y)

tree.BCI.WD.patch <- tree.BCI.WD %>% filter(gx >= 0 & gx < 1000 &
                                              gy >= 0 & gy < 500) %>% ungroup() %>%
  mutate(patch = patches$patch,
         patch_x = patches$patch_X,
         patch_y = patches$patch_Y)




tree.BCI.WD.patch.quadrant <- tree.BCI.WD.patch %>%
  left_join(data.quadrant,
            by = c("patch"))

tree.BCI.WD.patch.quadrant.summ <- tree.BCI.WD.patch.quadrant %>% group_by(patch,valu) %>% summarise(N = length(dbh),
                                                                                                     dbh.mean = mean(dbh/1000),
                                                                                                     dbh.max = max(dbh/1000),
                                                                                                     BA = sum(dbh/1000*dbh/1000*pi/4),
                                                                                                     .groups = "keep")


ggplot(data = tree.BCI.WD.patch.quadrant.summ %>% filter(valu < 4)) +
  geom_boxplot(aes(x = as.factor(valu), y = N)) +
  theme_bw()

ggplot(data = tree.BCI.WD.patch.quadrant.summ %>% filter(valu < 4)) +
  geom_boxplot(aes(x = as.factor(valu), y = dbh.max)) +
  theme_bw()

trees <- tree.BCI.WD.patch.quadrant %>% dplyr::select(treeID,patch,dbh,gx,gy,valu,WD)

plants <- bind_rows(list(trees %>% mutate(is_liana = FALSE,
                                          dbh = dbh/10) %>% dplyr::select(dbh,valu,is_liana,patch,WD),
                         lianas %>% mutate(is_liana = TRUE,
                                           dbh = DBH/10,
                                           WD = 0.46) %>% dplyr::select(dbh,valu,is_liana,patch,WD)))

#########################################################################################################################################
# Create input files

dbh_minL = 3.; href <- 61.7;b1Ht <- 0.11;b2Ht <- 1.2;hmax <- 35 # Liana
b1ht_tree = 0.0352 ; b2ht_tree = 0.694 ; hmax_tree = 35 ; href_tree = 61.7 # Tree
WD.low = 0.5 ; WD.high = 0.7

cohorts <- plants %>% mutate(pft = case_when(is_liana ~ 17,
                                             WD < WD.low ~ 2,
                                             WD > WD.high ~ 4,
                                             TRUE ~ 3)) %>%
  group_by(patch,valu) %>%
  mutate(h = pmin(hmax_tree,href_tree*(1-exp(-b1ht_tree*(dbh**b2ht_tree))))) %>%
  mutate(h = case_when(!is_liana ~ h,
                       (dbh < dbh_minL) ~ pmin(hmax, href*(1 -exp(-b1Ht*(dbh**b2Ht)))),
                       TRUE ~ pmin(hmax,0.5 + max(h[!is_liana]))))

# pss file
FSC <- 0.15 ; STSC <- 5 ; SSC <- 4.546

direct <- "/home/femeunier/Documents/projects/LianaBCI/inputs"
csspssfile_name <- "BCI_small_patchtype"
types <- c("High","gap","recent.gap","former.gap")# 0 = Undisturbed High-Canopy, 1 = Persistent Canopy Gaps, 2 = Recent Canopy Gaps, 3 = Former Canopy Gaps, and 4 = Unused Quadrats

lianas.sum.all <- data.frame()

patch.select.max = 5

for (i in seq(0,3)){

  print(i)

  cohorts.select <- cohorts %>% ungroup() %>%
    filter(valu == i)

  patch.sum <- cohorts.select %>%
    filter(is_liana) %>%
    group_by(patch) %>%
    summarise(n = length(h)/(Delta_X*Delta_Y),
              ba = sum(dbh*dbh)*pi/4/(Delta_X*Delta_Y),
              .groups = "keep")

  m <- patch.sum %>% ungroup() %>%
    summarise(n.m = mean(n),
              ba.m = mean(ba))

  # Patch selection based on the mean BA
  patch.selection <- patch.sum %>% mutate(SSQ = (ba - m$ba.m)**2) %>% arrange(SSQ)

  cohorts.select <- cohorts.select %>% filter(patch %in% (patch.selection %>% pull(patch))[1:patch.select.max])

  patches.selected <- unique(cohorts.select$patch)

  ctype <- types[i + 1]
  c.cohorts <- cohorts.select %>% arrange(patch) %>%
    mutate(time = 2000,
           n = 1/(Delta_X*Delta_Y),
           bdead = 0,
           balive = 0,
           lai = 0,
           tag = 1:nrow(cohorts.select)) %>%
    dplyr::select(time,patch,tag,dbh,h,pft,n,bdead,balive,lai) %>% arrange(time,patch) %>% ungroup() %>%
    rename(cohort = tag,
           hite = h)

  # ggplot(data = c.cohorts) +
  #   geom_point(aes(x = patch, y = hite, color = as.factor(pft))) +
  #   theme_bw()

  lianas <- c.cohorts %>%
    filter(pft == 17,
           dbh >= 3) %>%
    group_by(patch) %>%
    summarise(n.total = sum(n),
              ba = sum(dbh*dbh*pi/4*n),
              .groups = "keep")

  lianas.sum <- lianas %>% ungroup() %>%
    summarise(nplant = mean(n.total),
              ba = mean(ba),
              .groups = "keep")

  lianas.sum.all <- bind_rows(list(lianas.sum.all,
                                   lianas.sum %>% mutate(gap = ctype)))

  write.table(x = c.cohorts,
              file = file.path(direct,paste0(csspssfile_name,"_",ctype,".lat9.000lon-79.000.css")),
              quote = FALSE, row.names = FALSE)

  c.patch <- data.frame(time = 2000,
                        patch = unique(c.cohorts$patch),
                        trk = 2,
                        age = 0,
                        area = 1/length(unique(c.cohorts$patch)),
                        water = 0,fsc = FSC,stsc =  STSC, stsl = STSC,ssc = SSC, lai = 0, msn = 0,fsn = 0, nep = 0, gpp = 0, rh = 0)

  write.table(c.patch, file.path(direct,paste0(csspssfile_name,"_",ctype,".lat9.000lon-79.000.pss")), quote = FALSE, row.names = FALSE)

}


system2("rsync",paste("-avz","./inputs","hpc:/kyukon/scratch/gent/vo/000/gvo00074/felicien/LianaBCI/inputs"))




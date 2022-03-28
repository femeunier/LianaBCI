rm(list = ls())

library(dplyr)
library(ggplot2)
library(tidyr)
library(BCI.AGB)
library(ggridges)

data.file <- "/home/femeunier/Documents/projects/LianaBCI/data/2021-09-22_liana_bci_data_V9_0_felicien.csv"
data <- read.csv(data.file)

threshold <- 1*10 # 3cm = 30mm
data.filtered <- data %>% filter(DBH >= 10)
data.filtered %>% group_by(Census) %>% summarise(Nliving = length(DBH[Status %in% c("L","N")]),
                                        N_large_living = length(DBH[Status %in% c("L","N") & DBH >= threshold]),
                                        N_very_large = length(DBH[Status %in% c("L","N") & DBH >= 100]),

                                        DBH_mean = mean(DBH[Status %in% c("L","N")], na.rm = TRUE)/10,
                                        DBH_large_mean = mean(DBH[Status %in% c("L","N") & DBH >= threshold], na.rm = TRUE)/10,

                                        BA_living = sum(pi*DBH[Status %in% c("L","N")]*DBH[Status %in% c("L","N")]/4)/1000/1000,

                                        BA_large_living = sum(pi*DBH[Status %in% c("L","N") & DBH >= threshold]*DBH[Status %in% c("L","N") & DBH >= threshold]/4)/1000/1000,

                                        .groups = "keep")

Delta_X = Delta_Y = 20
data.patch <- data.filtered %>% filter(PX >= 0 & PX < 1000 &
                              PY >= 0 & PY < 500,
                              Status %in% c("L","N")) %>% ungroup() %>%
  mutate(patch = LianaBCI::patchnumber_from_position(PX,PY,
                                           patch_X = Delta_X,patch_Y = Delta_Y,
                                           extr_x = c(0,1000),
                                           extr_y = c(0,500))[["patch"]])

patches <- sort(unique(data.patch$patch))
Npatches <- length(patches)

ggplot(data = data.filtered %>% filter(Status %in% c("L","N")),
       aes(x = DBH/10,y = as.factor(Census), fill = as.factor(Census))) +
  stat_density_ridges(quantile_lines = TRUE, alpha = 0.75) +
  theme_bw()


data.filtered.bin <- data.filtered %>% filter(Status %in% c("L","N")) %>% mutate(DBH.cat = case_when(DBH < 20 ~ 1,
                                                                                                     DBH < 30 ~ 2,
                                                                                                     DBH < 50 ~ 3,
                                                                                                     DBH < 100 ~ 4,
                                                                                                     TRUE ~ 5)) %>%
  group_by(DBH.cat,Census) %>% summarise(N = length(DBH)/50/10000,
                                         BA = sum(DBH*DBH,na.rm = TRUE)*pi/4/50/10000/100, #cm²/m²
                                         .groups = "keep")

ggplot(data = data.filtered.bin,
       aes(x = as.factor(DBH.cat), y = N,fill = as.factor(Census))) +
  geom_bar(stat = "identity",position="dodge") +
  scale_x_discrete(breaks = 1:5,
                   labels = c("1-2cm","2-3cm","3-5cm","5-10cm",">10cm")) +
  labs(x = "", y = "# Ind", fill = "Census") +
  scale_fill_manual(values = c("red","blue")) +
  # scale_y_log10() +
  theme_bw()

ggplot(data = data.filtered.bin %>% filter(DBH.cat >= 3),
       aes(x = as.factor(DBH.cat), y = N,fill = as.factor(Census))) +
  geom_bar(stat = "identity",position="dodge") +
  scale_x_discrete(breaks = 1:5,
                   labels = c("1-2cm","2-3cm","3-5cm","5-10cm",">10cm")) +
  labs(x = "", y = "# Ind", fill = "Census") +
  scale_fill_manual(values = c("red","blue")) +
  # scale_y_log10() +
  theme_bw()

ggplot(data = data.filtered.bin,
       aes(x = as.factor(DBH.cat), y = BA,fill = as.factor(Census))) +
  geom_bar(stat = "identity",position="dodge") +
  scale_x_discrete(breaks = 1:5,
                   labels = c("1-2cm","2-3cm","3-5cm","5-10cm",">10cm")) +
  labs(x = "", y = "BA", fill = "Census") +
  scale_fill_manual(values = c("red","blue")) +
  # scale_y_log10() +
  theme_bw()

saveRDS(data.filtered.bin,file = "./data/data.Liana.census.RDS")

data.filtered.bin.cat <- data.patch %>%
  mutate(DBH.cat = case_when(DBH < 20 ~ 1,
                             DBH < 30 ~ 2,
                             DBH < 50 ~ 3,
                             DBH < 100 ~ 4,
                             TRUE ~ 5)) %>%
  group_by(DBH.cat,patch,Census) %>% summarise(N = length(DBH)/(Delta_X*Delta_Y),
                                               BA = sum(DBH*DBH,na.rm = TRUE)*pi/4/(Delta_X*Delta_Y)/100, #cm²/m²
                                               .groups = "keep") %>%
  ungroup() %>%
  complete(DBH.cat = 1:5,patch = 1:1250,Census = 1:2,fill = list(N = 0,
                                                                 BA = 0))

data.quadrant <- read.csv("./data/forest_scenarios_felicien.csv") %>%
  mutate(patch = horz + 1 + vert*50) %>%
  dplyr::select(patch,valu)

data.filtered.bin.cat.patch <- data.filtered.bin.cat %>%
  left_join(data.quadrant,
            by = "patch")

ggplot(data = data.filtered.bin.cat.patch %>% filter(DBH.cat == 1, Census == 1)) +
  geom_histogram(aes(x = valu)) +
  theme_bw()

saveRDS(data.filtered.bin.cat.patch,file = "./data/data.Liana.cat.census.RDS")

data.filtered.bin.cat.patch.evo <-
  data.filtered.bin.cat.patch %>%
  group_by(patch, Census, valu) %>%
  summarise(N = sum(N),
            BA = sum(BA),
            .groups = "keep") %>%
  pivot_wider(names_from = "Census",
              values_from = c(N,BA)) %>%
  mutate(N.change = N_2 - N_1,
         N.rel.change = (N_2 - N_1)/N_1,
         BA.change = BA_2 - BA_1,
         BA.rel.change = (BA_2 - BA_1)/BA_1) %>%
  group_by(valu) %>%
  summarise(N.init = mean(N_1),
            N.end = mean(N_2),
            N.change.m = mean(N.change),
            # N.rel.change.m = 100*mean(N.rel.change),

            BA.init = mean(BA_1),
            BA.end = mean(BA_2),
            BA.change.m = mean(BA.change),
            # BA.rel.change.m = 100*mean(BA.rel.change),
            .groups = "keep") %>%
  ungroup() %>%
  mutate(gap = case_when(valu == 0 ~ "High",
                         valu == 1 ~ "Gap",
                         valu == 2 ~ "Recent",
                         valu == 3 ~ "Former",
                         valu == 4 ~ "Unused")) %>%
  filter(valu < 4)

# 0 = Undisturbed High-Canopy, 1 = Persistent Canopy Gaps, 2 = Recent Canopy Gaps, 3 = Former Canopy Gaps, and 4 = Unused Quadrats

saveRDS(data.filtered.bin.cat.patch.evo,file = "./data/data.Liana.cat.valu.census.RDS")


data.completed <- data %>%  group_by(No.,LianaID,StemID) %>% mutate(DBH = case_when(any(is.na(DBH)) ~ DBH[!is.na(DBH)],
                                                                  TRUE ~ DBH),
                                                  PX = case_when(any(is.na(PX)) ~ PX[!is.na(PX)],
                                                                  TRUE ~ PX),
                                                  PY = case_when(any(is.na(PY)) ~ PY[!is.na(PY)],
                                                                  TRUE ~ PY))

data.filtered.completed <- data.completed %>% filter(DBH >= 10) %>% ungroup()

data.filtered.completed.bin <- data.filtered.completed %>% filter(Status %in% c("L","N","D")) %>% mutate(DBH.cat = case_when(DBH < 20 ~ 1,
                                                                                                                             DBH < 30 ~ 2,
                                                                                                                             DBH < 50 ~ 3,
                                                                                                                             DBH < 100 ~ 4,
                                                                                                                             TRUE ~ 5)) %>%
  group_by(DBH.cat,Status,Census) %>% summarise(N = length(DBH),
                                         .groups = "keep") %>% mutate(N = case_when(Status == "D" ~ - N,
                                                                                    TRUE ~ N)) %>%
  arrange(DBH.cat,Census)

data.filtered.completed.bin.result <- bind_rows(list(data.filtered.completed.bin,
                                                     data.filtered.completed.bin %>%
                                                       group_by(Census,DBH.cat) %>%
                                                       filter(Census == 2,
                                                              Status %in% c("L","N")) %>%
                                                       summarise(N = sum(N),
                                                                 .groups = "keep") %>%
                                                       mutate(Status = "Total")))

data.filtered.completed.bin.result <- data.filtered.completed.bin.result %>%
  mutate(Status.all = factor(paste0(Census,Status),levels = c("1L","2Total","2L","2D","2N")))

ggplot(data = data.filtered.completed.bin.result ,
       aes(x = as.factor(DBH.cat), y = N,fill = Status.all,alpha = Status.all)) +
  geom_bar(stat = "identity",position="dodge") +
  scale_x_discrete(breaks = 1:5,
                   labels = c("1-2cm","2-3cm","3-5cm","5-10cm",">10cm")) +
  labs(x = "", y = "# Ind", fill = "") +
  scale_fill_manual(values = c("red","blue","lightblue","deepskyblue2","darkblue"),
                    labels = c("Total lianas 2007","Total lianas 2017","Persistent Lianas 2007-2017",
                               "Dead Lianas 2007-2017","Recruited Lianas 2007-2017")) +
  scale_alpha_manual(values = rep(1,5)) +
  theme_bw() +
  theme(legend.position = c(0.8,0.8),
        text = element_text(size = 20)) +
  guides(alpha = "none")


data.filtered.completed.bin %>% pivot_wider(names_from = c(Status,Census),
                                            values_from = N) %>%
  mutate(mortality = -D_2/L_1,
         recruitment = N_2/L_1,
         mortality_rate = -D_2/L_1/10)

data.filtered.completed.bin.result %>% ungroup() %>% filter(DBH.cat == max(DBH.cat))

data %>% filter(No. %in% c(data %>% filter(Census == 2, DBH == 0) %>% pull(No.)))
###################################################
# Liana/tree LAI

tree.census <- BCI.AGB::bci.full7 %>% dplyr::select(treeID,stemID,sp,status,gx,gy,dbh) %>% filter(!is.na(dbh),!is.na(gx),!is.na(gy), status == "A") %>%
  filter(gx >= 0 & gx <= 1000 &
           gy >= 0 & gy <= 500) %>%
  mutate(patch = LianaBCI::patchnumber_from_position(gx,gy,patch_X = Delta_X,patch_Y = Delta_Y)[["patch"]])

# LAI parameters
b1Bl = c(Liana = 0.049, Tree = 0.02)
b2Bl = c(Liana = 1.89, Tree = 1.85)
SLA = c(Liana = 1/0.064, Tree = 1/0.087)

data.Bl <- data.patch %>%
  mutate(Bl = b1Bl["Liana"]*((DBH/10)**b2Bl["Liana"]),
         lai = SLA["Liana"]*Bl/(Delta_X*Delta_Y))

tree.Bl <- tree.census %>%
  mutate(Bl = b1Bl["Tree"]*((dbh/10)**b2Bl["Tree"]),
         lai = SLA["Tree"]*Bl/(Delta_X*Delta_Y))

data.Bl.summ <- data.Bl %>% group_by(Census,patch) %>% summarise(lai_liana = sum(lai,na.rm = TRUE),
                                                                 .groups = "keep")


tree.Bl.summ <- tree.Bl %>% group_by(patch) %>% summarise(lai_tree = sum(lai,na.rm = TRUE),
                                                          .groups = "keep")


data.all.summ <- data.Bl.summ %>% left_join(tree.Bl.summ,
                                            by = 'patch') %>% mutate(lai_total = lai_liana + lai_tree) %>%
  mutate(lai_liana_frac = lai_liana/lai_total)

ggplot(data = data.all.summ,
       aes(x = lai_liana,y = as.factor(Census), fill = as.factor(Census))) +
  stat_density_ridges(quantile_lines = TRUE, alpha = 0.4) +
  theme_bw()

ggplot(data = data.all.summ,
       aes(y = lai_liana,x = as.factor(Census), fill = as.factor(Census))) +
  # geom_jitter(shape=16, position=position_jitter(0.2)) +
  geom_violin(alpha = 0.4) +
  geom_boxplot(width=0.1,outlier.shape = NA) +
  theme_bw()

summary(data.Bl.summ %>% filter(Census == 1) %>% pull(lai_liana))
summary(data.Bl.summ %>% filter(Census == 2) %>% pull(lai_liana))

ggplot(data = data.all.summ,
       aes(x = lai_liana_frac,y = as.factor(Census), fill = as.factor(Census))) +
  stat_density_ridges(quantile_lines = TRUE, alpha = 0.75) +
  theme_bw()

summary(tree.Bl.summ$lai_tree)

data.Bl.summ.census <- data.Bl.summ %>%
  mutate(Census = as.character(Census)) %>%
  pivot_wider(values_from = lai_liana,
              names_from = Census) %>% mutate(diff_lai_liana = `2` - `1`)

ggplot(data = data.Bl.summ.census,
       aes(x = diff_lai_liana)) +
  geom_density(alpha = 0.75) +
  theme_bw()

summary(data.Bl.summ.census$diff_lai_liana)

data.all.summ.census <- data.all.summ %>%
  dplyr::select(-c(lai_total,lai_liana_frac)) %>%
  mutate(Census = as.character(Census)) %>%
  pivot_wider(values_from = lai_liana,
              names_from = Census) %>% mutate(diff_lai_liana = `2` - `1`,
                                              diff_contribution_liana = `2` /(`2` + lai_tree) - `1` /(`1` + lai_tree))

ggplot(data = data.all.summ.census,
       aes(x = diff_contribution_liana)) +
  geom_density(alpha = 0.75) +
  theme_bw()

summary(data.all.summ.census$diff_contribution_liana)


rm(list = ls())

library(dplyr)
library(ggplot2)
library(tidyr)

system2("rsync",
        paste("-avz","hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/OP_patch_cat_ensemble_LianaBCI.RDS",
              "./outputs/"))

####################################################################################

data <- readRDS("/home/femeunier/Documents/projects/LianaBCI/data/data.Liana.cat.census.RDS") %>%
  rename(nplant = N,
         ba = BA) %>%
  filter(valu <= 3) %>%
  mutate(gap = case_when(valu == 0 ~ "High",
                         valu == 1 ~ "gap",
                         valu == 2 ~ "recent.gap",
                         valu == 3 ~ "former.gap")) %>%
  dplyr::select(-valu) %>%
  group_by(DBH.cat,Census,gap) %>%
  summarise(nplant = mean(nplant),
            ba = mean(ba),
            .groups = "keep") %>%
  ungroup() %>%
  mutate(pft = 17,
         num = 1,
         type = "Data") %>%
  filter(DBH.cat >=3)

data %>% group_by(Census,gap) %>%
  filter(Census == 2) %>%
  summarise(N = sum(nplant),
            BA = sum(ba)*20*20/100/100,
            .groups = "keep")


readRDS("/home/femeunier/Documents/projects/LianaBCI/data/data.Liana.census.RDS") %>% group_by(Census) %>%
  summarise(N = sum(N),
            ba = sum(BA),
            .groups = "keep")

# 0 = Undisturbed High-Canopy, 1 = Persistent Canopy Gaps, 2 = Recent Canopy Gaps, 3 = Former Canopy Gaps, and 4 = Unused Quadrats

OP_cat_ensemble_LianaBCI <- readRDS("./outputs/OP_patch_cat_ensemble_LianaBCI.RDS")

times <- 2007 + (1:((max(OP_cat_ensemble_LianaBCI$t)))-1)/12
OP_cat_ensemble_LianaBCI.formatted <- OP_cat_ensemble_LianaBCI %>% mutate(time = times[t]) %>%
  filter(time %in% c(2007,2016+11/12)) %>%
  rename(gap = type) %>%
  mutate(Census = case_when(time == 2007 ~ 1,
                            time == 2016+11/12 ~ 2),
         type = "Model") %>%
  dplyr::select(-c(t,time,dbh))%>%
  filter(DBH.cat >=3)


runs <- unique(OP_cat_ensemble_LianaBCI.formatted$num)
Nruns <- length(runs)
Ncat <- length(unique(OP_cat_ensemble_LianaBCI.formatted %>% filter(pft == 17) %>% pull(DBH.cat)))

data.vs.models <- OP_cat_ensemble_LianaBCI.formatted
for (irun in seq(1,Nruns)){
  data.vs.models <- bind_rows(list(data.vs.models,
                                   data %>% mutate(num = runs[irun])))
}

data.vs.models <- data.vs.models %>% filter(pft == 17)

data.vs.models.wide <- data.vs.models %>% pivot_wider(values_from = c(nplant,ba),
                                                      names_from = type)


ggplot(data = data.vs.models.wide %>%
         filter(DBH.cat >=3),
       aes(x = nplant_Data, y = nplant_Model,color = as.factor(num))) +
  geom_point() +
  labs(x = "Data", y = "Model", color = "run") +
  stat_smooth(method = "lm", se = FALSE) +
  geom_abline(slope = 1,intercept = 0, color = "black", linetype = 2) +
  facet_wrap(~ as.factor(Census)) +
  coord_fixed() +
  theme_bw()


ggplot(data = data.vs.models.wide %>% filter(Census == 1),
       aes(x = ba_Data, y = ba_Model,color = as.factor(num))) +
  geom_point() +
  labs(x = "Data", y = "Model", color = "run") +
  stat_smooth(method = "lm", se = FALSE) +
  geom_abline(slope = 1,intercept = 0, color = "black", linetype = 2) +
  facet_wrap(~ as.factor(Census)) +
  theme_bw()

df.RMSE <- data.vs.models.wide %>%
  group_by(num) %>%
  summarise(RMSE.rel = 100*sqrt((sum(((nplant_Model - nplant_Data)/nplant_Data)**2))/length(nplant_Model)),
            RMSE.BA.rel = 100*sqrt((sum(((ba_Model - ba_Data)/ba_Data)**2))/length(nplant_Model)),
            slope = coef(lm(formula = nplant_Model ~ nplant_Data))[2],
            .groups = "keep") %>%
  arrange((RMSE.BA.rel))

best.runs <- df.RMSE$num[1:3]

ggplot(data = data.vs.models %>% filter((type == "Model" & num %in% best.runs) | (type == "Data" & num == 1)),
       aes(x = as.factor(DBH.cat), y = nplant,fill = interaction(num,as.factor(type)))) +
  geom_bar(stat = "identity",position="dodge") +
  scale_x_discrete(breaks = 1:5,
                   labels = c("1-2cm","2-3cm","3-5cm","5-10cm",">10cm")) +
  labs(x = "", y = "# Ind", fill = "Census") +
  facet_wrap(~ as.factor(Census)) +
  theme_bw()

ggplot(data = data.vs.models %>% filter(num %in% best.runs[1]),
       aes(x = as.factor(DBH.cat), y = nplant,fill = interaction(Census))) +
  geom_bar(stat = "identity",position="dodge") +
  scale_x_discrete(breaks = 1:5,
                   labels = c("1-2cm","2-3cm","3-5cm","5-10cm",">10cm")) +
  labs(x = "", y = "# Ind", fill = "Census") +
  facet_wrap(~ as.factor(type)) +
  theme_bw()

######################################################################################
# We test the mean of best runs

data.vs.models.mean.gap <- data.vs.models %>% filter(num %in% df.RMSE$num[1:20]) %>%
  group_by(Census,type,DBH.cat,gap) %>% summarise(nplant = mean(nplant),
                                                  ba = mean(ba),
                                                  .groups = "keep")
data.vs.models.mean <- data.vs.models.mean.gap %>%
  group_by(Census,type,DBH.cat) %>% summarise(nplant = mean(nplant),
                                              ba = mean(ba),
                                              .groups = "keep")


ggplot(data = data.vs.models.mean,
       aes(x = as.factor(DBH.cat), y = nplant,fill = as.factor(Census))) +
  geom_bar(stat = "identity",position="dodge") +
  scale_x_discrete(breaks = 1:5,
                   labels = c("1-2cm","2-3cm","3-5cm","5-10cm",">10cm")) +
  labs(x = "", y = "# Ind", fill = "Census") +
  facet_wrap(~ as.factor(type)) +
  theme_bw()

ggplot(data = data.vs.models.mean,
       aes(x = as.factor(DBH.cat), y = ba,fill = as.factor(Census))) +
  geom_bar(stat = "identity",position="dodge") +
  scale_x_discrete(breaks = 1:5,
                   labels = c("1-2cm","2-3cm","3-5cm","5-10cm",">10cm")) +
  labs(x = "", y = "BA", fill = "Census") +
  facet_wrap(~ as.factor(type)) +
  theme_bw()

saveRDS(df.RMSE$num[1:20],"./outputs/best.runs.RDS")

ggplot(data = data.vs.models.mean.gap,
       aes(x = as.factor(DBH.cat), y = ba,fill = as.factor(Census))) +
  geom_bar(stat = "identity",position="dodge") +
  scale_x_discrete(breaks = 1:5,
                   labels = c("1-2cm","2-3cm","3-5cm","5-10cm",">10cm")) +
  labs(x = "", y = "BA", fill = "Census") +
  facet_grid(gap ~ as.factor(type)) +
  theme_bw()

data.vs.models.mean.gap %>% group_by(Census,type,gap) %>%
  summarise(nplant = sum(nplant),
            ba = sum(ba),
            .groups = "keep") %>%
  pivot_wider(names_from = Census,
              values_from = c(nplant,ba)) %>%
  mutate(n_change = nplant_2 - nplant_1,
         ba_change = ba_2 -ba_1)

#####################################################################################################


df.RMSE <- data.vs.models.wide %>%
  group_by(num) %>%
  summarise(RMSE.rel = 100*sqrt((sum(((nplant_Model - nplant_Data)/nplant_Data)**2))/length(nplant_Model)),
            RMSE.BA.rel = 100*sqrt((sum(((ba_Model - ba_Data)/ba_Data)**2))/length(nplant_Model)),
            slope = coef(lm(formula = nplant_Model ~ nplant_Data))[2],
            .groups = "keep") %>%
  arrange((RMSE.BA.rel))

best.runs <- df.RMSE$num[1:3]

ggplot(data = data.vs.models %>% filter((type == "Model" & num %in% best.runs) | (type == "Data" & num == 1)),
       aes(x = as.factor(DBH.cat), y = nplant,fill = interaction(num,as.factor(type)))) +
  geom_bar(stat = "identity",position="dodge") +
  scale_x_discrete(breaks = 1:5,
                   labels = c("1-2cm","2-3cm","3-5cm","5-10cm",">10cm")) +
  labs(x = "", y = "# Ind", fill = "Census") +
  facet_wrap(~ as.factor(Census)) +
  theme_bw()

ggplot(data = data.vs.models %>% filter(num %in% best.runs[1]),
       aes(x = as.factor(DBH.cat), y = nplant,fill = interaction(Census))) +
  geom_bar(stat = "identity",position="dodge") +
  scale_x_discrete(breaks = 1:5,
                   labels = c("1-2cm","2-3cm","3-5cm","5-10cm",">10cm")) +
  labs(x = "", y = "# Ind", fill = "Census") +
  facet_wrap(~ as.factor(type)) +
  theme_bw()

######################################################################################
# We test the mean of best runs

df.RMSE.gap <- data.vs.models.wide %>%
  group_by(num,gap) %>%
  summarise(RMSE.rel = 100*sqrt((sum(((nplant_Model - nplant_Data)/nplant_Data)**2))/length(nplant_Model)),
            RMSE.BA.rel = 100*sqrt((sum(((ba_Model - ba_Data)/ba_Data)**2))/length(nplant_Model)),
            slope = coef(lm(formula = nplant_Model ~ nplant_Data))[2],
            .groups = "keep") %>%
  arrange((RMSE.BA.rel))

best.runs.gap <- df.RMSE %>% group_by(gap) %>%
  top_n(n = 5,desc(RMSE.BA.rel)) %>%
  arrange(gap)

data.vs.models.mean.gap <- data.vs.models %>%
  left_join(df.RMSE.gap %>% dplyr::select(num,gap,RMSE.BA.rel),
            by = c("gap","num")) %>%
  group_by(gap) %>%
  filter(RMSE.BA.rel %in% (sort(unique(RMSE.BA.rel)))[1:10]) %>%
  group_by(Census,type,DBH.cat,gap) %>%
  summarise(nplant = mean(nplant),
            ba = mean(ba),
            .groups = "keep")

data.vs.models.mean <- data.vs.models.mean.gap %>%
  group_by(Census,type,DBH.cat) %>% summarise(nplant = mean(nplant),
                                              ba = mean(ba),
                                              .groups = "keep")


ggplot(data = data.vs.models.mean,
       aes(x = as.factor(DBH.cat), y = nplant,fill = as.factor(Census))) +
  geom_bar(stat = "identity",position="dodge") +
  scale_x_discrete(breaks = 1:5,
                   labels = c("1-2cm","2-3cm","3-5cm","5-10cm",">10cm")) +
  labs(x = "", y = "# Ind", fill = "Census") +
  facet_wrap(~ as.factor(type)) +
  theme_bw()

ggplot(data = data.vs.models.mean,
       aes(x = as.factor(DBH.cat), y = ba,fill = as.factor(Census))) +
  geom_bar(stat = "identity",position="dodge") +
  scale_x_discrete(breaks = 1:5,
                   labels = c("1-2cm","2-3cm","3-5cm","5-10cm",">10cm")) +
  labs(x = "", y = "BA", fill = "Census") +
  facet_wrap(~ as.factor(type)) +
  theme_bw()

saveRDS(df.RMSE$num[1:20],"./outputs/best.runs.RDS")

ggplot(data = data.vs.models.mean.gap,
       aes(x = as.factor(DBH.cat), y = ba,fill = as.factor(Census))) +
  geom_bar(stat = "identity",position="dodge") +
  scale_x_discrete(breaks = 1:5,
                   labels = c("1-2cm","2-3cm","3-5cm","5-10cm",">10cm")) +
  labs(x = "", y = "BA", fill = "Census") +
  facet_grid(gap ~ as.factor(type)) +
  theme_bw()

data.vs.models.mean.gap %>% group_by(Census,type,gap) %>%
  summarise(nplant = sum(nplant),
            ba = sum(ba),
            .groups = "keep") %>%
  pivot_wider(names_from = Census,
              values_from = c(nplant,ba)) %>%
  mutate(n_change = nplant_2 - nplant_1,
         ba_change = ba_2 -ba_1)

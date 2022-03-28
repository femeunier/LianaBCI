rm(list = ls())

library(dplyr)
library(ggplot2)
library(tidyr)

system2("rsync",
        c("-avz","hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/df.LianaBCIpatch.ensembles.RDS",
          file.path("/home/femeunier/Documents/projects/LianaBCI","outputs","df.LianaBCIpatch.ensembles.RDS")))
system2("rsync",c("-avz","hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/df.LianaBCIpatch.params.RDS",
                  file.path("/home/femeunier/Documents/projects/LianaBCI","outputs","df.LianaBCIpatch.params.RDS")))

####################################################################################################################

data <- readRDS(file = "./data/data.Liana.cat.valu.census.RDS") %>%
  mutate(type = case_when(valu == 0 ~ "High",
                          valu == 1 ~ "gap",
                          valu == 2 ~ "recent.gap",
                          valu == 3 ~ "former.gap")) %>%
  dplyr::select(-c(gap,N.change.m,BA.change.m)) %>%
  pivot_longer(cols = -c(valu,type),
               names_to = "var",
               values_to = "value") %>%
  mutate(time = case_when(grepl("init",var) ~ 2007,
                          TRUE ~ 2017),
         variable = case_when(grepl("N",var) ~ "density",
                              TRUE ~ "BA"))


# 0 = Undisturbed High-Canopy, 1 = Persistent Canopy Gaps, 2 = Recent Canopy Gaps, 3 = Former Canopy Gaps, and 4 = Unused Quadrats


####################################################################################################################


df.LianaBCIpatch.params <- readRDS(file.path("/home/femeunier/Documents/projects/LianaBCI","outputs","df.LianaBCIpatch.params.RDS"))

select.runs <- df.LianaBCIpatch.params %>% filter(PFT == 17, param == "Vm0",value < Inf) %>% pull(run)

df.LianaBCIpatch.ensembles <- readRDS(file.path("/home/femeunier/Documents/projects/LianaBCI","outputs","df.LianaBCIpatch.ensembles.RDS")) %>%
  mutate(time = year + (month - 1)/12) %>% filter(num %in% select.runs)

ggplot(data = df.LianaBCIpatch.ensembles) +
  geom_line(aes(x = time, y = density_eco, group = num)) +
  facet_wrap(~ type, scales = "free", nrow = 1) +
  theme_bw()



df.LianaBCIpatch.ensembles %>% filter(type == "High",num == 1,year == 2007, month == 1)

ggplot(data = df.LianaBCIpatch.ensembles) +
  geom_point(data = data %>% filter(variable == "density"),
             aes(x = time, y = value), color = "red") +
  geom_line(aes(x = time, y = density_liana,
                group = interaction(num,type))) +
  facet_wrap(~ type,nrow = 1) +
  theme_bw()

ggplot(data = df.LianaBCIpatch.ensembles) +
  geom_point(data = data %>% filter(variable == "BA"),
             aes(x = time, y = value), color = "red") +
  geom_line(aes(x = time, y = BA_liana,
                group = interaction(num,type))) +
  facet_wrap(~ type, nrow = 1) +
  theme_bw()

ggplot(data = df.LianaBCIpatch.ensembles %>%
         group_by(type,num) %>%
         filter(time == max(df.LianaBCIpatch.ensembles$time,na.rm = TRUE)),
       aes(x = density_liana, y = BA_liana,color = type)) +
  geom_point() +
  stat_smooth(method =  "lm", se = FALSE) +
  theme_bw()



IO <- df.LianaBCIpatch.params %>% rename(num = run) %>%
  right_join(df.LianaBCIpatch.ensembles %>% group_by(num,type) %>% filter(time %in% c(min(time),max(time))),
             by = c("num"))

IO %>% filter(param == "Vm0",PFT == 2,year == 2007) %>% group_by(type) %>% summarise(N = length(num),
                                                                                     .groups = "keep")

IO.wide <- IO %>%
  filter(!is.na(time)) %>%
  ungroup() %>%
  mutate(timing = case_when(time == min(time) ~ "Init",
                            time == max(time) ~ "End")) %>%
  dplyr::select(timing,param,value,PFT,num,name,type,density_liana,BA_liana) %>%
  pivot_wider(names_from = timing,
              values_from = c(density_liana,BA_liana)) %>%
  mutate(density_change = density_liana_End - density_liana_Init,
         BA_change = BA_liana_End - BA_liana_Init)


df.density <- summary(aov(lm(as.formula(paste0("density_change ~ .")),
               data = IO.wide %>% filter(PFT == 17) %>%
                 dplyr::select(num,param,value,PFT,type,density_change) %>%
                 pivot_wider(values_from = "value",
                             names_from = "param") %>%
                 dplyr::select(-c("num","PFT")))))

df.BA <- summary(aov(lm(as.formula(paste0("BA_change ~ .")),
               data = IO.wide %>% filter(PFT == 17) %>%
                 dplyr::select(num,param,value,PFT,type,BA_change) %>%
                 pivot_wider(values_from = "value",
                             names_from = "param") %>%
                 dplyr::select(-c("num","PFT")))))


df.all <- data.frame(name = rownames(df.density[[1]]),
                     p.val.density = df.density[[1]][,5]) %>%
  left_join(data.frame(name = rownames(df.BA[[1]]),
                       p.val.BA = df.BA[[1]][,5]),
            by = "name") %>%
  filter(!is.na(p.val.density)) %>%
  mutate(signif.density = case_when(p.val.density < 0.001 ~ "***",
                                    p.val.density < 0.01 ~ "**",
                                    p.val.density < 0.05 ~ "*",
                                    p.val.density < 0.1 ~ ".",
                                    TRUE ~ "N.S"),
         signif.BA = case_when(p.val.BA < 0.001 ~ "***",
                               p.val.BA < 0.01 ~ "**",
                               p.val.BA < 0.05 ~ "*",
                               p.val.BA < 0.1 ~ ".",
                               TRUE ~ "N.S"))

ggplot(data = IO.wide %>% filter(param == "stomatal_slope",PFT == 17),
       aes(x = value, y = BA_change)) +
  geom_point() +
  facet_wrap(~ type, scales = "free") +
  stat_smooth(method = "lm") +
  scale_y_log10() +
  theme_bw()

summary(aov(lm(as.formula(paste0("density_change ~ .")),
               data = IO.wide %>% filter(PFT == 17) %>%
                 dplyr::select(num,param,value,PFT,type,density_change) %>%
                 pivot_wider(values_from = "value",
                             names_from = "param") %>%
                 dplyr::select(-c("num","PFT")))))


ggplot(data = IO %>% ungroup() %>% filter(param == "Vm0",time == max(time,na.rm = TRUE),PFT == 17),
       aes(x = value,y = density_liana*20*20)) +
  geom_point() +
  facet_wrap(~ type) +
  stat_smooth(method = "lm") +
  theme_bw()

summary(aov(lm(as.formula(paste0("density_liana ~ .")),
                  data = IO %>% filter(time == max(time,na.rm = TRUE), PFT == 17) %>%
                    dplyr::select(num,param,value,PFT,type,density_liana) %>%
                    pivot_wider(values_from = "value",
                                names_from = "param") %>%
                    dplyr::select(-c("num","PFT")))))


ggplot(data = IO %>% filter(param == "clumping_factor",time == max(time,na.rm = TRUE),PFT == 17),
       aes(x = value,y = BA_liana)) +
  geom_point() +
  facet_wrap(~ type) +
  stat_smooth(method = "lm") +
  theme_bw()

summary(aov(lm(as.formula(paste0("BA_liana ~ .")),
               data = IO %>% filter(time == max(time,na.rm = TRUE), PFT == 17) %>%
                 dplyr::select(num,param,value,PFT,type,BA_liana) %>%
                 pivot_wider(values_from = "value",
                             names_from = "param") %>%
                 dplyr::select(-c("num","PFT")))))



df.LianaBCIpatch.ensembles.long <- df.LianaBCIpatch.ensembles %>% pivot_longer(cols = -c(name,num,type,year,month,time),
                                                                               names_to = "var",
                                                                               values_to = "value") %>%
  mutate(level =  sub(".*\\_", "", var),
         var = sub("\\_.*", "", var)) %>%
  pivot_wider(names_from = level,
              values_from = value) %>%
  mutate(ratio = liana/eco)

df.LianaBCIpatch.ensembles.long.sum <- df.LianaBCIpatch.ensembles.long %>%
  group_by(type,time,var) %>%
  summarise(liana_m = mean(liana),
            liana_sd = sd(liana),
            ratio_m = mean(ratio),
            ratio_sd = sd(ratio),
            .groups = "keep")

ggplot(data = df.LianaBCIpatch.ensembles.long.sum %>% filter(var == "BA")) +
  geom_ribbon(aes(x = time, ymin = ratio_m - ratio_sd, ymax = ratio_m + ratio_sd,fill = type),
                  color = NA, alpha = 0.25) +
  geom_line(aes(x = time, y = ratio_m, color = type)) +
  theme_bw()

ggplot(data = df.LianaBCIpatch.ensembles.long.sum) +
  geom_ribbon(aes(x = time, ymin = ratio_m - ratio_sd, ymax = ratio_m + ratio_sd,fill = type),
              color = NA, alpha = 0.25) +
  geom_line(aes(x = time, y = ratio_m, color = type)) +
  facet_wrap(~ var,scales = "free_y",nrow = 1) +
  theme_bw()

ggplot(data = df.LianaBCIpatch.ensembles.long.sum) +
  geom_ribbon(aes(x = time, ymin = liana_m - liana_sd, ymax = liana_m + liana_sd,fill = type),
              color = NA, alpha = 0.25) +
  geom_line(aes(x = time, y = liana_m, color = type)) +

  geom_point(data = data %>% mutate(var = variable),
             aes(x = time, y = value, color = type)) +

  facet_wrap(~ var,scales = "free_y",nrow = 1) +
  theme_bw()

#################################################################################################
# Change

df.LianaBCIpatch.ensembles.long.change <- df.LianaBCIpatch.ensembles.long %>% ungroup() %>% filter(time %in% c(2007,2016+(11/12))) %>%
  group_by(var,num,type) %>%
  mutate(change = liana - liana[1],
         rel.change = change/liana[1])


df.LianaBCIpatch.ensembles.long.change.sum <- df.LianaBCIpatch.ensembles.long.change %>%
  ungroup() %>%
  filter(time > 2008) %>%
  group_by(type,var) %>%
  summarise(change_m = mean(change),
            change_sd = sd(change),
            rel_change_m = mean(rel.change),
            rel_change_sd = sd(rel.change),
            .groups = "keep")

df.LianaBCIpatch.ensembles.long.change$type <- factor(as.character(df.LianaBCIpatch.ensembles.long.change$type),
                                                      levels = c("High","gap","recent.gap","former.gap"))

ggplot(data = df.LianaBCIpatch.ensembles.long.change %>% filter(var == "BA",
                                                                time > 2008)) +
  geom_boxplot(aes(x = type, y = change/100/100*20*20), outlier.shape = NA) +
  geom_point(data = data %>% filter(variable == "BA") %>% dplyr::select(-var) %>%
               pivot_wider(names_from = time,
                           values_from = value) %>%
               mutate(change = (`2017` - `2007`)/100/100*20*20),
             aes(x = type, y = change), color = "red") +
  labs(x = "", y = "BA change") +
  scale_y_log10() +
  theme_bw()

ggplot(data = df.LianaBCIpatch.ensembles.long.change %>% filter(var == "density",
                                                                time > 2008)) +
  geom_boxplot(aes(x = type, y = change*20*20), outlier.shape = NA) +
  geom_point(data = data %>% filter(variable == "density") %>% dplyr::select(-var) %>%
               pivot_wider(names_from = time,
                           values_from = value) %>%
               mutate(change = (`2017` - `2007`)*20*20),
             aes(x = type, y = change), color = "red") +
  labs(x = "", y = "Density change") +
  # scale_y_continuous(limits = c(0,100)) +
  theme_bw()

###################################################################################################################

init.end <- df.LianaBCIpatch.ensembles.long %>% filter((year == 2007 & month == 1) |
                                                         (year == 2016 & month == 12)) %>%
  filter(var %in% c("BA","density")) %>%
  group_by(num,type,var) %>%
  mutate(timing = case_when(year == 2007 ~ "init",
                            TRUE ~ "end")) %>%
  mutate(timing = factor(timing, levels = c("init","end")))

ggplot(data = init.end %>% filter(var == "BA")) +
  geom_boxplot(aes(x = type, y = liana/100/100*20*20)) +
  geom_point(data = data %>% filter(variable == "BA") %>%
               mutate(timing = case_when(time == 2007 ~ "init",
                                         time == 2017 ~ "end")) %>%
               mutate(timing = factor(timing, levels = c("init","end"))),
             aes(x = type, y = value/100/100*20*20), color = "red") +
  labs(x = "Gap type", y = "Liana BA") +
  facet_wrap(~ timing) +
  theme_bw()

ggplot(data = init.end %>% filter(var == "density")) +
  geom_boxplot(aes(x = type, y = liana*20*20)) +
  geom_point(data = data %>% filter(variable == "density") %>%
               mutate(timing = case_when(time == 2007 ~ "init",
                                         time == 2017 ~ "end")) %>%
               mutate(timing = factor(timing, levels = c("init","end"))),
             aes(x = type, y = value*20*20), color = "red") +
  labs(x = "Gap type", y = "Liana density") +
  facet_wrap(~ timing) +
  scale_y_continuous(limits = c(0,150),breaks = seq(0,150,20)) +
  theme_bw()



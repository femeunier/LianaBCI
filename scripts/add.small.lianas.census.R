rm(list = ls())

library(ggplot2)
library(dplyr)
library(BCI.AGB)

direct <- "/home/femeunier/Documents/projects/LianaBCI/inputs"
csspssfile_name <- "BCI_small_patchtype"
types <- c("High","gap","recent.gap","former.gap")
# 0 = Undisturbed High-Canopy, 1 = Persistent Canopy Gaps, 2 = Recent Canopy Gaps, 3 = Former Canopy Gaps, and 4 = Unused Quadrats


href <- 61.7 ; b1Ht <- 0.1 ; b2Ht <- 0.87

df.census <- data.frame()

for (ctype in types){
  css.file <- file.path(direct,paste0(csspssfile_name,"_",ctype,".lat9.000lon-79.000.css"))
  tmp <- read.csv(css.file,header = TRUE,sep = " ")
  liana <- tmp %>% filter(pft == 17)

  df.census <- bind_rows(list(df.census,
                              liana %>% mutate(type = ctype)))
}

df.census.cat <- df.census %>%
  mutate(dbh.cat = case_when(dbh < 1 ~ 0,
                             dbh < 2 ~ 1,
                             dbh < 3 ~ 2,
                             dbh < 4 ~ 3,
                             dbh < 5 ~ 4,
                             dbh < 6 ~ 5,
                             dbh < 7 ~ 6,
                             dbh < 8 ~ 7,
                             dbh < 9 ~ 8,
                             dbh < 10 ~ 9,
                             TRUE ~ 10))

ggplot(data = df.census.cat %>%
         filter(dbh.cat < 10) %>%
         group_by(type,dbh.cat) %>%
         summarise(N = length(dbh),
                   .groups = "keep"),
       aes(x = dbh.cat, y = N)) +
  # geom_bar(stat="identity") +
  geom_point() +
  stat_smooth(method = "lm") +
  facet_wrap(~ type) +
  scale_y_log10() +
  scale_x_continuous(limits = c(0,10)) +
  theme_bw()


for (ctype in types){
  css.file <- file.path(direct,paste0(csspssfile_name,"_",ctype,".lat9.000lon-79.000.css"))
  tmp <- read.csv(css.file,header = TRUE,sep = " ")
  liana <- tmp %>% filter(pft == 17)

  liana.cat <- liana %>%
    mutate(dbh.cat = case_when(dbh < 1 ~ 0,
                               dbh < 2 ~ 1,
                               dbh < 3 ~ 2,
                               dbh < 4 ~ 3,
                               dbh < 5 ~ 4,
                               dbh < 6 ~ 5,
                               dbh < 7 ~ 6,
                               dbh < 8 ~ 7,
                               dbh < 9 ~ 8,
                               dbh < 10 ~ 9,
                               TRUE ~ 10)) %>% filter(dbh.cat < 10)

  liana.cat.sum <- liana.cat %>%
    group_by(dbh.cat) %>%
    summarise(N = length(dbh),
              .groups = "keep")

  Npatches <- length(unique(liana.cat$patch))

  N2add <- 10**(predict(lm(data = liana.cat.sum,
              log10(N) ~ dbh.cat),newdata = data.frame(dbh.cat = 0)))[[("1")]]

  Nperpatch <- round(N2add/Npatches)

  # dbhs <- runif(Nperpatch*Npatches,min = 0, max = 1)
  # h <- href*(1 -exp(-b1Ht*(dbhs**b2Ht)))
  # Test 1
  # liana.mod <- bind_rows(list(tmp,
  #                             data.frame(time = 2000,
  #                                        patch = sort(rep(unique(tmp$patch),Nperpatch)),
  #                                        cohort = max(tmp$cohort) + (1:(Nperpatch*Npatches)),
  #                                        dbh =  dbhs,
  #                                        hite = h,
  #                                        pft = 17,
  #                                        n = 0.0025,
  #                                        bdead = 0,
  #                                        balive = 0,
  #                                        lai = 0)))

  # Test 2

  liana.mod <- tmp

  for (i in seq(1,10)){
    liana.mod <- bind_rows(list(liana.mod,
                                data.frame(time = 2000,
                                           patch = unique(tmp$patch),
                                           cohort = max(liana.mod$cohort) + (1:(length(unique(tmp$patch)))),
                                           dbh =  (i - 1) * 0.1,
                                           hite = href*(1 -exp(-b1Ht*(((i - 1) * 0.1)**b2Ht))),
                                           pft = 17,
                                           n = (Nperpatch)/10/(20*20),
                                           bdead = 0,
                                           balive = 0,
                                           lai = 0)))
  }


  print(c(ctype,N2add,Npatches,N2add/Npatches))

  write.table(x = liana.mod,
              file = file.path(direct,paste0(csspssfile_name,"_",ctype,"_mod.lat9.000lon-79.000.css")),
              quote = FALSE, row.names = FALSE)

  system2("cp",
          c(file.path(direct,paste0(csspssfile_name,"_",ctype,".lat9.000lon-79.000.pss")),
          file.path(direct,paste0(csspssfile_name,"_",ctype,"_mod.lat9.000lon-79.000.pss"))))

}

system2("rsync",paste("-avz","./inputs","hpc:/kyukon/scratch/gent/vo/000/gvo00074/felicien/LianaBCI"))


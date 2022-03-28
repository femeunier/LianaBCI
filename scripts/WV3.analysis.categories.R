rm(list = ls())

library(raster)
library(dplyr)
library(ggplot2)

files.in <- c("/home/femeunier/Downloads/WV3/3/011237512010_01_003/011237512010_01/011237512010_01_P001_MUL/14OCT22153719-M3DS_R1C1-011237512010_01_P001.TIF",
              # "/home/femeunier/Downloads/WV3/4/011237526010_01_003/011237526010_01/011237526010_01_P001_MUL/15DEC07155546-M3DS_R1C1-011237526010_01_P001.TIF",
              "/home/femeunier/Downloads/WV3/1/010398231010_01_003/010398231010_01/010398231010_01_P001_MUL/18MAR04160611-M3DS_R1C1-010398231010_01_P001.TIF",
              "/home/femeunier/Downloads/WV3/2/011237522010_01_003/011237522010_01/011237522010_01_P001_MUL/19MAR21155820-M3DS_R1C1-011237522010_01_P001.TIF")

years <- c(2014,2018,2019)
type <- c(1,1,1,1)

# # Bands
# 1. Coastal Blue (400 - 450 nm)
# 2. Blue (450 - 510 nm)
# 3. Green (510 - 580 nm)
# 4. Yellow (585 - 625 nm)
# 5. Red (630 - 690 nm)
# 6. Red edge (705 - 745 nm)
# 7. Near-IR1 (770 - 895 nm)
# 8. Near-IR2 (860 - 1040 nm)

X <- t(matrix(c(625773.9,1011776,
                626773.3,1011743,
                626789.6,1012243,
                625790.1,1012276,
                625773.9,1011776),
              nrow = 2))
p <- Polygon(X)
BCI = SpatialPolygons(list(Polygons(list(p), "p")))
data <- stack(files.in[1])
crs(BCI) <- crs(data)

# 0 = Undisturbed High-Canopy, 1 = Persistent Canopy Gaps, 2 = Recent Canopy Gaps, 3 = Former Canopy Gaps, and 4 = Unused Quadrats

X <- t(matrix(c(625773.9,1011776,
                626773.3,1011743,
                626789.6,1012243,
                625790.1,1012276),
              nrow = 2))

L <- sqrt((X[1,1] - X[2,1])**2 + (X[1,2] - X[2,2])**2)
l <- sqrt((X[3,1] - X[2,1])**2 + (X[3,2] - X[2,2])**2)

cx <- (X[1,1] + X[3,1])/2
cy <- (X[1,2] + X[3,2])/2

x <- cx - L/2
y <- cy - l/2

alpha <- atan((X[1,1] - X[4,1])/(X[1,2] - X[4,2])) #*180/pi

R <- t(matrix(c(cos(-alpha),-sin(-alpha),sin(-alpha),cos(-alpha)),ncol = 2))

data.quadrant <- read.csv("./data/forest_scenarios_felicien.csv") %>%
  mutate(patch = horz*25 + 1 + vert)

delta_X = 20 ; delta_Y = 20

data.quadrant.mod <- data.quadrant %>% mutate(x = delta_X/2 + horz*delta_X,
                                              y = delta_Y/2 + vert*delta_Y)

data.quadrant.all <- bind_rows(list(data.quadrant.mod %>% mutate(x = x - delta_X/2,
                                                                 y = y - delta_Y/2),
                                    data.quadrant.mod %>% mutate(x = x - delta_X/2,
                                                                 y = y + delta_Y/2),
                                    data.quadrant.mod %>% mutate(x = x + delta_X/2,
                                                                 y = y + delta_Y/2),
                                    data.quadrant.mod %>% mutate(x = x + delta_X/2,
                                                                 y = y - delta_Y/2),
                                    data.quadrant.mod %>% mutate(x = x - delta_X/2,
                                                                 y = y - delta_Y/2))) %>% arrange(patch) %>%
  mutate(x_bci = X[1,1] + x*R[1,1] + y*R[1,2],
         y_bci = X[1,2] + x*R[2,1] + y*R[2,2])


ggplot(data = data.quadrant) +
  geom_bar(aes(x = as.factor(valu))) +
  theme_bw()

N <- table(data.quadrant$valu)

df.all <- data.frame()
df.R <- data.frame()

for (ifile in seq(1,length(years))){

  print("==================")
  print(ifile)

  data <- stack(files.in[ifile])
  data.cropped <- mask(crop(data,extent(BCI)),BCI)

  for (icat in seq(1,(length(N)-1))){

    print(icat)
    cshapes <- data.quadrant.all %>% filter(valu == (icat-1))
    cpatches <- unique(cshapes$patch)

    for (i in seq(1,N[icat])){

      cshape <- cshapes %>% filter(patch == cpatches[i])

      X <- matrix(c(cshape$x_bci,cshape$y_bci),
                  ncol = 2)
      p <- Polygon(X)
      cquadrant = SpatialPolygons(list(Polygons(list(p), "p")))
      data.cropped.cquadrant <- mask(crop(data.cropped,extent(cquadrant)),cquadrant)

      for (iband in seq(1,8)){

        Rvec <- as.vector(data.cropped.cquadrant[[iband]])
        df.R <- bind_rows(list(df.R,
                               data.frame(year = years[ifile],
                                          cat = icat - 1,
                                          quad = cpatches[i],
                                          iquad = i,
                                          band = iband,
                                          R = Rvec[!is.na(Rvec)])))

        df.all <- bind_rows(list(df.all,
                                 data.frame(year = years[ifile],
                                            cat = icat - 1,
                                            quad = cpatches[i],
                                            iquad = i,
                                            band = iband,
                                            R = mean(as.matrix(data.cropped.cquadrant[[iband]]),na.rm = TRUE))))

      }
    }
  }
}

df.R.sum <- df.R %>% group_by(year,cat,band,iquad) %>% summarise(R = mean(R,na.rm = TRUE),
                                                             .groups = "keep")

ggplot(data =   df.R.sum) +
  geom_boxplot(aes(x = as.factor(cat), y = R/255, fill = as.factor(year)),outlier.shape = NA) +
  facet_wrap(~ band, scales = "free") +
  theme_bw()

ggplot(data =   df.R.sum %>% filter(band %in% c(3,7,8))) +
  geom_boxplot(aes(x = as.factor(cat), y = R/255, fill = as.factor(year)),outlier.shape = NA) +
  facet_wrap(~ band, scales = "free") +
  theme_bw()

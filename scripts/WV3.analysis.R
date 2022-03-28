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

# XMin <- 625773.8594 ; YMin <- 1011743.3795 ; XMax <- 626789.5653 ; YMax <- 1012275.5751
# e <- extent(c(XMin,XMax,YMin,YMax))
# ul_lat <- 9.156129 ; lr_lat <- 9.152909 ; ul_lon <- -79.852753 ; lr_lon <- -79.846058
# e2 <- extent(c(ul_lon,lr_lon,lr_lat,ul_lat))

df.all <- data.frame()
df.R <- data.frame()

for (ifile in seq(1,length(years))){

  data <- stack(files.in[ifile])
  data.cropped <- mask(crop(data,extent(BCI)),BCI)

  # plot(data[[7]])
  # plot(BCI, axes=TRUE,add = TRUE,col = sf.colors(categorical = TRUE, alpha = 0.5))

  for (iband in seq(1,8)){

    Rvec <- as.vector(data.cropped[[iband]])
    df.R <- bind_rows(list(df.R,
                           data.frame(year = years[ifile],
                                      band = iband,
                                      R = Rvec[!is.na(Rvec)])))

    df.all <- bind_rows(list(df.all,
                             data.frame(year = years[ifile],
                                        band = iband,
                                        R = mean(as.matrix(data.cropped[[iband]]),na.rm = TRUE))))
  }
}

df.init <- bind_rows(df.all %>% filter(year == min(years)),
                     df.all %>% filter(year == min(years)) %>% mutate(year = max(years)))

ggplot(data = df.all) +
  geom_line(aes(x = year, y = R, color = as.factor(band))) +
  geom_line(data = df.init,
            aes(x = year, y = R, color = as.factor(band)),linetype = 2) +
  theme_bw()

df.changes <- df.all %>% group_by(band) %>% mutate(Rchange = R - R[year == min(years)],
                                                   Rrel = 1+(R - R[year == min(years)])/R[year == min(years)])

ggplot(data = df.changes) +
  geom_line(aes(x = year, y = Rrel, color = as.factor(band))) +
  theme_bw()

ggplot(data = df.changes %>% filter(band %in% c(3,7,8))) +
  geom_line(aes(x = year, y = Rrel, color = as.factor(band))) +
  theme_bw()

ggplot(data = df.R) +
  geom_boxplot(aes(x = as.factor(band), y = R/255, fill = as.factor(year)),outlier.shape = NA) +
  theme_bw()

#################################################################################################################




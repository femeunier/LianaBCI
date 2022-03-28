rm(list = ls())

library(raster)
library(dplyr)
data.loc.plots <- read.csv(file.path(".","location_plots.csv"))
plots.select <- sort(unique(data.loc.plots$Plot))
plots <- list()

for (iplot in seq(1,length(plots.select))){
  cplot <- data.loc.plots %>% filter(Plot == plots.select[iplot])
  alt.corners <- cplot
  alt.corners[1,"X"] <- cplot[2,"X"]
  alt.corners[2,"X"] <- cplot[1,"X"]

  cplot.extended <- bind_rows(list(cplot,
                                   alt.corners))

  coordinates(cplot.extended) <- c("X", "Y")
  proj4string(cplot.extended) <- CRS("+proj=longlat +datum=WGS84")
  plots[[iplot]] <- spTransform(cplot.extended, CRS("+proj=utm +zone=17 ellps=WGS84"))

}

df.R <- df.all <- data.frame()

files <- paste0("/data/gent/425/vsc42558/WVimages/",
               c("20110717/20110717","20120214/20120214","20120411/20120411","20130201/20130201",
                 "20130322/20130322","20160410/20160410","20161003/20161003","20170408/20170408",
                 "20180407/20180407","20190321/20190321"),
               "_Corrected_TIF.tif")



compt <- 0
for (ifile in seq(1,length(files))){

  print(ifile)
  compt = compt + 1
  for (iplot in seq(1,length(plots.select))){

    print(paste("- ",iplot))

    if (file.exists(files[ifile])){

      r.file <- stack(files[ifile])
      FN <- basename(files[ifile])
      cyear <- as.numeric(substr(FN,1,4))
      cmonth <- as.numeric(substr(FN,5,6))

      data <- stack(r.file)
      data.cropped <- tryCatch(crop(data,extent(plots[[iplot]])),
                               error = function(err){NA})

      if (!all(is.na(as.vector(data.cropped))) & all(as.vector(data.cropped[[1]] < 1000))){

        for (iband in seq(1,8)){
          Rvec <- as.vector(data.cropped[[iband]])
          Rvec[Rvec <= 0] <- NA

          if (all(!is.na(Rvec))){

            df.R <- bind_rows(list(df.R,
                                   data.frame(num = compt,
                                              year = cyear,
                                              month = cmonth,
                                              band = iband,
                                              plot = plots.select[iplot],
                                              R = Rvec[!is.na(Rvec)])))

            df.all <- bind_rows(list(df.all,
                                     data.frame(num = compt,
                                                year = cyear,
                                                month = cmonth,
                                                band = iband,
                                                plot = plots.select[iplot],
                                                R = mean(Rvec,na.rm = TRUE))))
          }
        }
      } else {
        warning(paste("File ", r.file,"does not exist"))
      }
    }
  }
}


saveRDS(object = df.R,"./df_R.RDS")
saveRDS(object = df.all,"./df_all.RDS")

# r.file <- stack("/home/femeunier/Downloads/20110717_Corrected_TIF.tif")
# plot(r.file[[1]])
# plot(r.file[[1]],xlim = c(625620, 625690),ylim = c(1008090, 1008160))
# plot(plots[[1]], add = T)

# scp /home/femeunier/Documents/projects/LianaBCI/scripts/WV.Gigante.R hpc:/data/gent/425/vsc42558/WVimages/

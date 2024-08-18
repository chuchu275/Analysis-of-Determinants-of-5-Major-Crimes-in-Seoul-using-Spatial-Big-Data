rm(list = ls())
gc()

library(GWmodel)      ### GW models
library(sp)           ## Data management
library(spdep)        ## Spatial autocorrelation
library(RColorBrewer) ## Visualization
library(classInt)     ## Class intervals
library(raster)       ## spatial data
library(grid)         # plot
library(gridExtra)    # Multiple plot
library(ggplot2)      # Multiple plot
library(gtable)
library(MASS)
library(reshape2)
library(rgdal)
library(tmap)

mf<-read.csv(paste0("residuals.csv"), header=T)
seoul<-readOGR(dsn = ".", layer = "seoulOnly")
SPDF<-merge(seoul,mf,by="code")
names(SPDF)
queen.nb=read.gal("seoulOnly.gal", region.id=SPDF$code)
summary(queen.nb)
queen.R.nb=poly2nb(SPDF, row.names=SPDF$code)
summary(queen.R.nb)
isTRUE(all.equal(queen.nb,queen.R.nb,check.attributes=FALSE))

tm_shape(SPDF) + tm_fill(col="crime", style="quantile", n=10) +
  tm_legend(outside=TRUE)

gw = localmoran(SPDF$gwpr, nb2listw(queen.nb))
nb = localmoran(SPDF$nb, nb2listw(queen.nb))
lm = localmoran(SPDF$lm, nb2listw(queen.nb))
ps = localmoran(SPDF$poisson, nb2listw(queen.nb))

write.csv(gw,file="gw.csv")
write.csv(nb,file="nb.csv")
write.csv(lm,file="lm.csv")
write.csv(ps,file="ps.csv")

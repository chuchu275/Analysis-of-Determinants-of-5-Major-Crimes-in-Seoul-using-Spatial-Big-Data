rm(list = ls())
gc()

R.version

library(GWmodel)    
library(sp)       
library(spdep)     
library(RColorBrewer) 
library(classInt)    
library(raster)     
library(grid)        
library(gridExtra)   
library(ggplot2)   
library(gtable)
library(MASS)
library(reshape2)
library(rgdal)
library(tmap)

mf<-read.csv(paste0("data.csv"), header=T)
seoul<-readOGR(dsn = ".", layer = "seoulOnly")
SPDF<-merge(seoul,mf,by="code")
names(SPDF)
queen.nb=read.gal("seoulOnly.gal", region.id=SPDF$code)
summary(queen.nb)
queen.R.nb=poly2nb(SPDF, row.names=SPDF$code)
summary(queen.R.nb)
isTRUE(all.equal(queen.nb,queen.R.nb,check.attributes=FALSE))

moran.test(SPDF$crime,nb2listw(queen.nb))

df=mf
head(df)

DM<-gw.dist(dp.locat=coordinates(SPDF))

bgwr.res <- ggwr.basic(fiveMajorCrimes ~ populationDensity+foreignerRatio+area+commercialLandArea+pub+cctv+multiHouseholdHouse, 
                       data = SPDF,
                       family = "poisson",
                       bw = bw.gwr, 
                       kernel = "bisquare", 
                       adaptive = TRUE,
                       dMat = DM)

bgwr.res

linear_model <- lm(fiveMajorCrimes ~ populationDensity+foreignerRatio+area+commercialLandArea+pub+cctv+multiHouseholdHouse, data = SPDF)
aic_lm <- AIC(linear_model)
aic_lm

poisson_model <- glm(fiveMajorCrimes ~ populationDensity+foreignerRatio+area+commercialLandArea+pub+cctv+multiHouseholdHouse, 
                     data = SPDF, family = poisson)
aic_poisson <- AIC(poisson_model)
aic_poisson

negbin_model <- glm.nb(fiveMajorCrimes ~ populationDensity+foreignerRatio+area+commercialLandArea+pub+cctv+multiHouseholdHouse, 
                       data = SPDF)
summary(negbin_model)
aic_nb <- AIC(negbin_model)
aic_nb

write.csv(bgwr.res$glms$residuals,file="gwpr.csv")
write.csv(negbin_model$residuals,file="negbin.csv")
write.csv(linear_model$residuals,file="linear.csv")
write.csv(poisson_model$residuals,file="poisson.csv")

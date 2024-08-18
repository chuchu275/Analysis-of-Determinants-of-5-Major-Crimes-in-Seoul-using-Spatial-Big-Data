rm(list = ls())
gc()

install.packages("psych")
install.packages("car")

library(psych)
library(car)

packageVersion("car")

data <- read.csv(paste0("data.csv"), header=T)

print(describe(data$fiveMajorCrimes))

X <- data[, c("populationDensity", "foreignerRatio", "area", "commercialLandArea", "pub", "cctv", "multiHouseholdHouse")]
y <- data$fiveMajorCrimes

model <- lm(y ~ ., data = X)

vif_result <- car::vif(model)

print(vif_result)

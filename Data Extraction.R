###DATA EXTRACTION OF DTA FILES NSS 68 CES###
###Ananya & Rajsi############################

#Setting Working Directory

setwd("C:/Ananya Iyengar/Delhi School of Economics/406_Applied Consumption Analysis/ACA Term Paper")

#Loading Packages

library(haven)
library(dplyr)

#Loading Data

level1 <- read_dta("level1dta.dta")
level2 <- read_dta("level2dta.dta")
level3 <- read_dta("level3dta.dta")
level5 <- read_dta("level5dta.dta")

#Merging Data

data12 <- inner_join(level1, level2, by = "HHID")
data123 <- inner_join(level3, data12, by = "HHID")
data1235 <- inner_join(level5, data123, by  = "HHID")

#Creating Data Frame

consumption68 <- as.data.frame(data1235)

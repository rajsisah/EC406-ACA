################################################################################
######### RAJSI & ANANYA #######################################################
######### Running AIDS Model ###################################################


#Loading Libraries
library(haven) #Loadind DTA files
library(micEconAids) #Running linear and non-linear AIDS model
library(dplyr) #data manipulation
library(systemfit) #consistency checks
library(stargazer) #latex tables


###############################################################################
#AIDS ESTIMATION
#Variable Descriptions

#There are four categories of food: Food1, Food2, Food3 and Food4. 
#Note that the AIDS equation requires the following variables: expenditure shares, prices and total expenditure.
#Then, we use the following variables: 

# - "price_ccs" , "price159", "price169", "price189"                 
# - "price199"  ,  "price_fv" , "price_otherfd", "price_349","price_379","price_389",
# - "price_399"  : prices of the 4 types of food for all the households

# - "expenditure_share_159"     "expenditure_share_169"     "expenditure_share_189"     "expenditure_share_199"    
# - "expenditure_share_fv"      "expenditure_share_othfood" "expenditure_share_349"     "expenditure_share_379"    
# - "expenditure_share_389"     "expenditure_share_399" : expenditure shares of the 4 types of food for all the households

# - "totalexp" : Total Expenditure on food

###############################################################################

## JHARKHAND TOP 50

jh_top50 <- read_dta('jh_top50')
state_data <- as.data.frame(jh_top50)
summary(state_data)

#Creating vectors for price and expenditure share names 

state_data$price_ccs <- as.numeric(state_data$price_ccs)
state_data$price159 <- as.numeric(state_data$price159)
state_data$price169 <- as.numeric(state_data$price169)
state_data$price189 <- as.numeric(state_data$price189)
state_data$price199 <- as.numeric(state_data$price199)
state_data$price_fv <- as.numeric(state_data$price_fv)
state_data$price_otherfd <- as.numeric(state_data$price_otherfd)
state_data$price_349 <- as.numeric(state_data$price_349)
state_data$price_379 <- as.numeric(state_data$price_379)
state_data$price_389 <- as.numeric(state_data$price_389)
state_data$price_399 <- as.numeric(state_data$price_399)

state_data$totalexp <- as.numeric(state_data$totalexp)

state_data$expenditure_share_ccs <- as.numeric(state_data$expenditure_share_ccs)
state_data$expenditure_share_159 <- as.numeric(state_data$expenditure_share_159)
state_data$expenditure_share_169 <- as.numeric(state_data$expenditure_share_169)
state_data$expenditure_share_189 <- as.numeric(state_data$expenditure_share_189)
state_data$expenditure_share_199 <- as.numeric(state_data$expenditure_share_199)
state_data$expenditure_share_fv <- as.numeric(state_data$expenditure_share_fv)
state_data$expenditure_share_othfood <- as.numeric(state_data$expenditure_share_othfood)
state_data$expenditure_share_349 <- as.numeric(state_data$expenditure_share_349)
state_data$expenditure_share_379 <- as.numeric(state_data$expenditure_share_379)
state_data$expenditure_share_389 <- as.numeric(state_data$expenditure_share_389)
state_data$expenditure_share_399 <- as.numeric(state_data$expenditure_share_399)

attach(state_data)

priceNames <- c("price_ccs" , "price159", "price169", "price189", "price199"  ,  "price_fv" , "price_otherfd", 
                "price_349","price_379","price_389", "price_399" )
shareNames <- c( "expenditure_share_ccs", "expenditure_share_159",    "expenditure_share_169",     "expenditure_share_189",     "expenditure_share_199",    
                 "expenditure_share_fv" ,   "expenditure_share_othfood", "expenditure_share_349",   "expenditure_share_379",
                 "expenditure_share_389",     "expenditure_share_399" )

################################################################################

#Linear Approximation AIDS Equation using the Stone Price Index

#The AIDS Equation is: w = alpha + sum_gamma_pij + beta_ln(x/P) + error

#The system of demand equations is estimated by the Seemingly Unrelated Estimation Method (Zellner, 1962)


linear_aids_1 <- aidsEst(priceNames, shareNames, "totalexp", data = state_data, priceIndex = "S")
summary(linear_aids_1)

#Now, since expenditure shares are seen on both sides of the AIDS expression, it ould lead to simultaneity bias. 
#To eradicate this problem, we used lagged values of expenditure shares in construction of the price index (Blanciforti et al. 1986)

#Linear Approximation AIDS Model Correcting for Simultaneity (Lagged Stone Index)

linear_aids_2 <- aidsEst(priceNames, shareNames, "totalexp", data = state_data, priceIndex = "SL")
summary(linear_aids_2)

#Non-Linear AIDS Using the Iterated Linear Least Squares Estimator (Blundell and Robin, 1999)

nonlinear_aids_1 <- aidsEst(priceNames, shareNames, "totalexp", data = state_data, method = "IL", priceIndex = "SL")
summary(nonlinear_aids_1)

################################################################################

#Testing for Properties of Demand 

#We must specify some value of alpha0 here 

linear_aids_1_coefficients <- coef(linear_aids_1)
linear_aids_1_coefficients$alpha0 <- 1
aidsConsist(priceNames, "totalexp", coef = linear_aids_1_coefficients, data = state_data)

linear_aids_2_coefficients <- coef(linear_aids_2)
linear_aids_2_coefficients$alpha0 <- 1
aidsConsist(priceNames, "totalexp", coef = linear_aids_2_coefficients, data = state_data)

nonlinear_aids_1_coefficients <- coef(nonlinear_aids_1)
aidsConsist(priceNames, "totalexp", coef = nonlinear_aids_1_coefficients, data = state_data)

################################################################################

#Computing Elasticities 

pMeans <- colMeans( state_data[ , priceNames ] )
wMeans <- colMeans( state_data[ , shareNames ] )
xtMean <- mean( state_data[ , "totalexp" ] )


#For Linear Model 1

linear_aids_1_el <- aidsElas(linear_aids_1_coefficients, prices = pMeans, shares = wMeans)
linear_aids_1_el
linear_aids_1_el_summary <- aidsElas(linear_aids_1_coefficients, prices = pMeans, totExp = xtMean, coefCov = vcov(linear_aids_1), df = df.residual(linear_aids_1))
summary(linear_aids_1_el_summary)  


#For Linear Model 2

linear_aids_2_el <- aidsElas(linear_aids_2_coefficients, prices = pMeans, shares = wMeans)
linear_aids_2_el  
linear_aids_2_el_summary <- aidsElas(linear_aids_2_coefficients, prices = pMeans, totExp = xtMean, coefCov = vcov(linear_aids_2), df = df.residual(linear_aids_2))
summary(linear_aids_1_el_summary)
stargazer(linear_aids_2_el$exp)
stargazer(linear_aids_2_el$marshall)
stargazer(linear_aids_2_el$hicks)


#For Non Linear Model 1 

nonlinear_aids_1_el <- aidsElas(nonlinear_aids_1_coefficients, prices = pMeans, shares = wMeans)
nonlinear_aids_1_el
nonlinear_aids_1_el_summary <- aidsElas(nonlinear_aids_1_coefficients, prices = pMeans, totExp = xtMean, coefCov = vcov(nonlinear_aids_1), df = df.residual(nonlinear_aids_1))
summary(nonlinear_aids_1_el_summary)
stargazer(nonlinear_aids_1_el$exp) 
stargazer(nonlinear_aids_1_el$marshall) 
stargazer(nonlinear_aids_1_el$hicks) 

################################################################################

#JHARKHAND BOTTOM 50
state_data2 <- read_dta('jh_bot50.dta')
state_data2 <- as.data.frame(state_data2)
summary(state_data2)

#Creating vectors for price and expenditure share names 

state_data2$price_ccs <- as.numeric(state_data2$price_ccs)
state_data2$price159 <- as.numeric(state_data2$price159)
state_data2$price169 <- as.numeric(state_data2$price169)
state_data2$price189 <- as.numeric(state_data2$price189)
state_data2$price199 <- as.numeric(state_data2$price199)
state_data2$price_fv <- as.numeric(state_data2$price_fv)
state_data2$price_otherfd <- as.numeric(state_data2$price_otherfd)
state_data2$price_349 <- as.numeric(state_data2$price_349)
state_data2$price_379 <- as.numeric(state_data2$price_379)
state_data2$price_389 <- as.numeric(state_data2$price_389)
state_data2$price_399 <- as.numeric(state_data2$price_399)

state_data2$totalexp <- as.numeric(state_data2$totalexp)

state_data2$expenditure_share_ccs <- as.numeric(state_data2$expenditure_share_ccs)
state_data2$expenditure_share_159 <- as.numeric(state_data2$expenditure_share_159)
state_data2$expenditure_share_169 <- as.numeric(state_data2$expenditure_share_169)
state_data2$expenditure_share_189 <- as.numeric(state_data2$expenditure_share_189)
state_data2$expenditure_share_199 <- as.numeric(state_data2$expenditure_share_199)
state_data2$expenditure_share_fv <- as.numeric(state_data2$expenditure_share_fv)
state_data2$expenditure_share_othfood <- as.numeric(state_data2$expenditure_share_othfood)
state_data2$expenditure_share_349 <- as.numeric(state_data2$expenditure_share_349)
state_data2$expenditure_share_379 <- as.numeric(state_data2$expenditure_share_379)
state_data2$expenditure_share_389 <- as.numeric(state_data2$expenditure_share_389)
state_data2$expenditure_share_399 <- as.numeric(state_data2$expenditure_share_399)

attach(state_data2)

priceNames <- c("price_ccs" , "price159", "price169", "price189", "price199"  ,  "price_fv" , "price_otherfd", 
                "price_349","price_379","price_389", "price_399" )
shareNames <- c( "expenditure_share_ccs", "expenditure_share_159",    "expenditure_share_169",     "expenditure_share_189",     "expenditure_share_199",    
                 "expenditure_share_fv" ,   "expenditure_share_othfood", "expenditure_share_349",   "expenditure_share_379",
                 "expenditure_share_389",     "expenditure_share_399" )

################################################################################

#Linear Approximation AIDS Equation using the Stone Price Index

#The AIDS Equation is: w = alpha + sum_gamma_pij + beta_ln(x/P) + error

#The system of demand equations is estimated by the Seemingly Unrelated Estimation Method (Zellner, 1962)


linear_aids_1 <- aidsEst(priceNames, shareNames, "totalexp", data = state_data2, priceIndex = "S")
summary(linear_aids_1)

#Now, since expenditure shares are seen on both sides of the AIDS expression, it ould lead to simultaneity bias. 
#To eradicate this problem, we used lagged values of expenditure shares in construction of the price index (Blanciforti et al. 1986)

#Linear Approximation AIDS Model Correcting for Simultaneity (Lagged Stone Index)

linear_aids_2 <- aidsEst(priceNames, shareNames, "totalexp", data = state_data2, priceIndex = "SL")
summary(linear_aids_2)

#Non-Linear AIDS Using the Iterated Linear Least Squares Estimator (Blundell and Robin, 1999)

nonlinear_aids_1 <- aidsEst(priceNames, shareNames, "totalexp", data = state_data2, method = "IL", priceIndex = "SL")
summary(nonlinear_aids_1)

################################################################################

#Testing for Properties of Demand 

#We must specify some value of alpha0 here 

linear_aids_1_coefficients <- coef(linear_aids_1)
linear_aids_1_coefficients$alpha0 <- 1
aidsConsist(priceNames, "totalexp", coef = linear_aids_1_coefficients, data = state_data2)

linear_aids_2_coefficients <- coef(linear_aids_2)
linear_aids_2_coefficients$alpha0 <- 1
aidsConsist(priceNames, "totalexp", coef = linear_aids_2_coefficients, data = state_data2)

nonlinear_aids_1_coefficients <- coef(nonlinear_aids_1)
aidsConsist(priceNames, "totalexp", coef = nonlinear_aids_1_coefficients, data = state_data2)

################################################################################

#Computing Elasticities 

pMeans <- colMeans( state_data2[ , priceNames ] )
wMeans <- colMeans( state_data2[ , shareNames ] )
xtMean <- mean( state_data2[ , "totalexp" ] )


#For Linear Model 1

linear_aids_1_el <- aidsElas(linear_aids_1_coefficients, prices = pMeans, shares = wMeans)
linear_aids_1_el
linear_aids_1_el_summary <- aidsElas(linear_aids_1_coefficients, prices = pMeans, totExp = xtMean, coefCov = vcov(linear_aids_1), df = df.residual(linear_aids_1))
summary(linear_aids_1_el_summary)  


#For Linear Model 2

linear_aids_2_el <- aidsElas(linear_aids_2_coefficients, prices = pMeans, shares = wMeans)
linear_aids_2_el  
linear_aids_2_el_summary <- aidsElas(linear_aids_2_coefficients, prices = pMeans, totExp = xtMean, coefCov = vcov(linear_aids_2), df = df.residual(linear_aids_2))
summary(linear_aids_1_el_summary) 
stargazer(linear_aids_2_el$exp)
stargazer(linear_aids_2_el$marshall)
stargazer(linear_aids_2_el$hicks)


#For Non Linear Model 1 

nonlinear_aids_1_el <- aidsElas(nonlinear_aids_1_coefficients, prices = pMeans, shares = wMeans)
nonlinear_aids_1_el
nonlinear_aids_1_el_summary <- aidsElas(nonlinear_aids_1_coefficients, prices = pMeans, totExp = xtMean, coefCov = vcov(nonlinear_aids_1), df = df.residual(nonlinear_aids_1))
summary(nonlinear_aids_1_el_summary)

stargazer(nonlinear_aids_1_el$exp)
stargazer(nonlinear_aids_1_el$marshall)
stargazer(nonlinear_aids_1_el$hicks)
################################################################################

#CHATTISGARH TOP 50
state_data2 <- read_dta('ch_top50.dta')
state_data2 <- as.data.frame(state_data2)
summary(state_data2)

#Creating vectors for price and expenditure share names 

state_data2$price_ccs <- as.numeric(state_data2$price_ccs)
state_data2$price159 <- as.numeric(state_data2$price159)
state_data2$price169 <- as.numeric(state_data2$price169)
state_data2$price189 <- as.numeric(state_data2$price189)
state_data2$price199 <- as.numeric(state_data2$price199)
state_data2$price_fv <- as.numeric(state_data2$price_fv)
state_data2$price_otherfd <- as.numeric(state_data2$price_otherfd)
state_data2$price_349 <- as.numeric(state_data2$price_349)
state_data2$price_379 <- as.numeric(state_data2$price_379)
state_data2$price_389 <- as.numeric(state_data2$price_389)
state_data2$price_399 <- as.numeric(state_data2$price_399)

state_data2$totalexp <- as.numeric(state_data2$totalexp)

state_data2$expenditure_share_ccs <- as.numeric(state_data2$expenditure_share_ccs)
state_data2$expenditure_share_159 <- as.numeric(state_data2$expenditure_share_159)
state_data2$expenditure_share_169 <- as.numeric(state_data2$expenditure_share_169)
state_data2$expenditure_share_189 <- as.numeric(state_data2$expenditure_share_189)
state_data2$expenditure_share_199 <- as.numeric(state_data2$expenditure_share_199)
state_data2$expenditure_share_fv <- as.numeric(state_data2$expenditure_share_fv)
state_data2$expenditure_share_othfood <- as.numeric(state_data2$expenditure_share_othfood)
state_data2$expenditure_share_349 <- as.numeric(state_data2$expenditure_share_349)
state_data2$expenditure_share_379 <- as.numeric(state_data2$expenditure_share_379)
state_data2$expenditure_share_389 <- as.numeric(state_data2$expenditure_share_389)
state_data2$expenditure_share_399 <- as.numeric(state_data2$expenditure_share_399)

attach(state_data2)

priceNames <- c("price_ccs" , "price159", "price169", "price189", "price199"  ,  "price_fv" , "price_otherfd", 
                "price_349","price_379","price_389", "price_399" )
shareNames <- c( "expenditure_share_ccs", "expenditure_share_159",    "expenditure_share_169",     "expenditure_share_189",     "expenditure_share_199",    
                 "expenditure_share_fv" ,   "expenditure_share_othfood", "expenditure_share_349",   "expenditure_share_379",
                 "expenditure_share_389",     "expenditure_share_399" )

################################################################################

#Linear Approximation AIDS Equation using the Stone Price Index

#The AIDS Equation is: w = alpha + sum_gamma_pij + beta_ln(x/P) + error

#The system of demand equations is estimated by the Seemingly Unrelated Estimation Method (Zellner, 1962)


linear_aids_1 <- aidsEst(priceNames, shareNames, "totalexp", data = state_data2, priceIndex = "S")
summary(linear_aids_1)

#Now, since expenditure shares are seen on both sides of the AIDS expression, it ould lead to simultaneity bias. 
#To eradicate this problem, we used lagged values of expenditure shares in construction of the price index (Blanciforti et al. 1986)

#Linear Approximation AIDS Model Correcting for Simultaneity (Lagged Stone Index)

linear_aids_2 <- aidsEst(priceNames, shareNames, "totalexp", data = state_data2, priceIndex = "SL")
summary(linear_aids_2)

#Non-Linear AIDS Using the Iterated Linear Least Squares Estimator (Blundell and Robin, 1999)

nonlinear_aids_1 <- aidsEst(priceNames, shareNames, "totalexp", data = state_data2, method = "IL", priceIndex = "SL")
summary(nonlinear_aids_1)

################################################################################

#Testing for Properties of Demand 

#We must specify some value of alpha0 here 

linear_aids_1_coefficients <- coef(linear_aids_1)
linear_aids_1_coefficients$alpha0 <- 1
aidsConsist(priceNames, "totalexp", coef = linear_aids_1_coefficients, data = state_data2)

linear_aids_2_coefficients <- coef(linear_aids_2)
linear_aids_2_coefficients$alpha0 <- 1
aidsConsist(priceNames, "totalexp", coef = linear_aids_2_coefficients, data = state_data2)

nonlinear_aids_1_coefficients <- coef(nonlinear_aids_1)
aidsConsist(priceNames, "totalexp", coef = nonlinear_aids_1_coefficients, data = state_data2)

################################################################################

#Computing Elasticities 

pMeans <- colMeans( state_data2[ , priceNames ] )
wMeans <- colMeans( state_data2[ , shareNames ] )
xtMean <- mean( state_data2[ , "totalexp" ] )


#For Linear Model 1

linear_aids_1_el <- aidsElas(linear_aids_1_coefficients, prices = pMeans, shares = wMeans)
linear_aids_1_el
linear_aids_1_el_summary <- aidsElas(linear_aids_1_coefficients, prices = pMeans, totExp = xtMean, coefCov = vcov(linear_aids_1), df = df.residual(linear_aids_1))
summary(linear_aids_1_el_summary)  


#For Linear Model 2

linear_aids_2_el <- aidsElas(linear_aids_2_coefficients, prices = pMeans, shares = wMeans)
linear_aids_2_el  
linear_aids_2_el_summary <- aidsElas(linear_aids_2_coefficients, prices = pMeans, totExp = xtMean, coefCov = vcov(linear_aids_2), df = df.residual(linear_aids_2))
summary(linear_aids_1_el_summary)  
stargazer(linear_aids_2_el$exp)
stargazer(linear_aids_2_el$marshall)
stargazer(linear_aids_2_el$hicks)

#For Non Linear Model 1 

nonlinear_aids_1_el <- aidsElas(nonlinear_aids_1_coefficients, prices = pMeans, shares = wMeans)
nonlinear_aids_1_el
nonlinear_aids_1_el_summary <- aidsElas(nonlinear_aids_1_coefficients, prices = pMeans, totExp = xtMean, coefCov = vcov(nonlinear_aids_1), df = df.residual(nonlinear_aids_1))
summary(nonlinear_aids_1_el_summary)
stargazer(nonlinear_aids_1_el$exp)
stargazer(nonlinear_aids_1_el$marshall)
stargazer(nonlinear_aids_1_el$hicks)

################################################################################
#CHATTISGARH BOTTOM 50
state_data3 <- read_dta('ch_bot50.dta')
state_data3 <- as.data.frame(state_data3)
summary(state_data3)

#Creating vectors for price and expenditure share names 

state_data3$price_ccs <- as.numeric(state_data3$price_ccs)
state_data3$price159 <- as.numeric(state_data3$price159)
state_data3$price169 <- as.numeric(state_data3$price169)
state_data3$price189 <- as.numeric(state_data3$price189)
state_data3$price199 <- as.numeric(state_data3$price199)
state_data3$price_fv <- as.numeric(state_data3$price_fv)
state_data3$price_otherfd <- as.numeric(state_data3$price_otherfd)
state_data3$price_349 <- as.numeric(state_data3$price_349)
state_data3$price_379 <- as.numeric(state_data3$price_379)
state_data3$price_389 <- as.numeric(state_data3$price_389)
state_data3$price_399 <- as.numeric(state_data3$price_399)

state_data3$totalexp <- as.numeric(state_data3$totalexp)

state_data3$expenditure_share_ccs <- as.numeric(state_data3$expenditure_share_ccs)
state_data3$expenditure_share_159 <- as.numeric(state_data3$expenditure_share_159)
state_data3$expenditure_share_169 <- as.numeric(state_data3$expenditure_share_169)
state_data3$expenditure_share_189 <- as.numeric(state_data3$expenditure_share_189)
state_data3$expenditure_share_199 <- as.numeric(state_data3$expenditure_share_199)
state_data3$expenditure_share_fv <- as.numeric(state_data3$expenditure_share_fv)
state_data3$expenditure_share_othfood <- as.numeric(state_data3$expenditure_share_othfood)
state_data3$expenditure_share_349 <- as.numeric(state_data3$expenditure_share_349)
state_data3$expenditure_share_379 <- as.numeric(state_data3$expenditure_share_379)
state_data3$expenditure_share_389 <- as.numeric(state_data3$expenditure_share_389)
state_data3$expenditure_share_399 <- as.numeric(state_data3$expenditure_share_399)

attach(state_data3)

priceNames <- c("price_ccs" , "price159", "price169", "price189", "price199"  ,  "price_fv" , "price_otherfd", 
                "price_349","price_379","price_389", "price_399" )
shareNames <- c( "expenditure_share_ccs", "expenditure_share_159",    "expenditure_share_169",     "expenditure_share_189",     "expenditure_share_199",    
                 "expenditure_share_fv" ,   "expenditure_share_othfood", "expenditure_share_349",   "expenditure_share_379",
                 "expenditure_share_389",     "expenditure_share_399" )

################################################################################

#Linear Approximation AIDS Equation using the Stone Price Index

#The AIDS Equation is: w = alpha + sum_gamma_pij + beta_ln(x/P) + error

#The system of demand equations is estimated by the Seemingly Unrelated Estimation Method (Zellner, 1962)


linear_aids_1 <- aidsEst(priceNames, shareNames, "totalexp", data = state_data3, priceIndex = "S")
summary(linear_aids_1)

#Now, since expenditure shares are seen on both sides of the AIDS expression, it ould lead to simultaneity bias. 
#To eradicate this problem, we used lagged values of expenditure shares in construction of the price index (Blanciforti et al. 1986)

#Linear Approximation AIDS Model Correcting for Simultaneity (Lagged Stone Index)

linear_aids_2 <- aidsEst(priceNames, shareNames, "totalexp", data = state_data3, priceIndex = "SL")
summary(linear_aids_2)

#Non-Linear AIDS Using the Iterated Linear Least Squares Estimator (Blundell and Robin, 1999)

nonlinear_aids_1 <- aidsEst(priceNames, shareNames, "totalexp", data = state_data3, method = "IL", priceIndex = "SL")
summary(nonlinear_aids_1)

################################################################################

#Testing for Properties of Demand 

#We must specify some value of alpha0 here 

linear_aids_1_coefficients <- coef(linear_aids_1)
linear_aids_1_coefficients$alpha0 <- 1
aidsConsist(priceNames, "totalexp", coef = linear_aids_1_coefficients, data = state_data3)

linear_aids_2_coefficients <- coef(linear_aids_2)
linear_aids_2_coefficients$alpha0 <- 1
aidsConsist(priceNames, "totalexp", coef = linear_aids_2_coefficients, data = state_data3)

nonlinear_aids_1_coefficients <- coef(nonlinear_aids_1)
aidsConsist(priceNames, "totalexp", coef = nonlinear_aids_1_coefficients, data = state_data3)

################################################################################

#Computing Elasticities 

pMeans <- colMeans( state_data3[ , priceNames ] )
wMeans <- colMeans( state_data3[ , shareNames ] )
xtMean <- mean( state_data3[ , "totalexp" ] )


#For Linear Model 1

linear_aids_1_el <- aidsElas(linear_aids_1_coefficients, prices = pMeans, shares = wMeans)
linear_aids_1_el
linear_aids_1_el_summary <- aidsElas(linear_aids_1_coefficients, prices = pMeans, totExp = xtMean, coefCov = vcov(linear_aids_1), df = df.residual(linear_aids_1))
summary(linear_aids_1_el_summary)  


#For Linear Model 2

linear_aids_2_el <- aidsElas(linear_aids_2_coefficients, prices = pMeans, shares = wMeans)
linear_aids_2_el  
linear_aids_2_el_summary <- aidsElas(linear_aids_2_coefficients, prices = pMeans, totExp = xtMean, coefCov = vcov(linear_aids_2), df = df.residual(linear_aids_2))
summary(linear_aids_2_el_summary)  


#For Non Linear Model 1 

nonlinear_aids_1_el <- aidsElas(nonlinear_aids_1_coefficients, prices = pMeans, shares = wMeans)
nonlinear_aids_1_el
nonlinear_aids_1_el_summary <- aidsElas(nonlinear_aids_1_coefficients, prices = pMeans, totExp = xtMean, coefCov = vcov(nonlinear_aids_1), df = df.residual(nonlinear_aids_1))
summary(nonlinear_aids_1_el_summary)
################################################################################



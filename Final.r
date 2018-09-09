##Loading Libraries
library(stargazer)
library(ggplot2)
library(car)
library(leaps)
library(normtest)
library (lmtest)
library('sandwich')
library(normtest)
library(L1pack)
library(e1071)
library(moments)

##Conversion values from PUMS Data, converts everything to 1999 Dollars using CPI
fiveto99 = 1.093
zero4to99 = 0.882
twelveto99 = 0.726   

##New Person Coming Along
newPerson = data.frame("FEMALE" = c(1), "NONWHITE" = c(0), "UNION" = c(1), "EDUC" = c(12), "AGE" = c(21), "EXPER" = c(3), "WAGE" = c(1254), "LNWAGE" = c(0.123))

##Reading in CPS Data
cps95 = read.csv(file = "C:/Users/Ronaldo/Documents/School Work/Penn/ECON 104/HW2/Data Sets/output95_update.csv")
cps04 = read.csv(file = "C:/Users/Ronaldo/Documents/School Work/Penn/ECON 104/HW2/Data Sets/output04_update.csv")
cps12 = read.csv(file = "C:/Users/Ronaldo/Documents/School Work/Penn/ECON 104/HW2/Data Sets/output12_update.csv")


##Converting All data to constant 1999 Dollars
##1995
cps95$WAGE = cps95$WAGE * fiveto99
cps95$LNWAGE = log(cps95$WAGE)
##2004
cps04$WAGE = cps04$WAGE * zero4to99
cps04$LNWAGE = log(cps04$WAGE)
##2012
cps12$WAGE = cps12$WAGE * twelveto99
cps12$LNWAGE = log(cps12$WAGE)

##Dealing with Outliers and Response Normality
##Any observations with wages beneath the minimum tipped hourly wage of $2.13 are surely errors and are cut from the data set. Similarly for
##observations with education or experience below 0.
cps95 = subset(cps95, WAGE >= 2.13 & EDUC >= 0 & EXPER >= 0)
cps04 = subset(cps04, WAGE >= 2.13 & EDUC >= 0 & EXPER >= 0)
cps12 = subset(cps12, WAGE >= 2.13 & EDUC >= 0 & EXPER >= 0)

##Creating Other Variable Terms
{
  EDUCsquared.1995 <- (cps95$EDUC)^2
  EXPERsquared.1995 <- (cps95$EXPER)^2
  EDUC.NONWHITE.1995 <- (cps95$EDUC)*(cps95$NONWHITE)
  EDUC.FEMALE.1995 <- (cps95$EDUC)*(cps95$FEMALE)
  EDUC.UNION.1995 <- (cps95$EDUC)*(cps95$UNION)
  EDUC.EXPER.1995 <- (cps95$EDUC)*(cps95$EXPER)
  EXPER.NONWHITE.1995 <- (cps95$EXPER)*(cps95$NONWHITE)
  EXPER.FEMALE.1995 <- (cps95$EXPER)*(cps95$FEMALE)
  EXPER.UNION.1995 <- (cps95$EXPER)*(cps95$UNION)
  NONWHITE.FEMALE.1995 <- (cps95$NONWHITE)*(cps95$FEMALE)
  NONWHITE.UNION.1995 <- (cps95$NONWHITE)*(cps95$UNION)
  FEMALE.UNION.1995 <- (cps95$FEMALE)*(cps95$UNION)
  
  EDUCsquared.2004 <- (cps04$EDUC)^2
  EXPERsquared.2004 <- (cps04$EXPER)^2
  EDUC.NONWHITE.2004 <- (cps04$EDUC)*(cps04$NONWHITE)
  EDUC.FEMALE.2004 <- (cps04$EDUC)*(cps04$FEMALE)
  EDUC.UNION.2004 <- (cps04$EDUC)*(cps04$UNION)
  EDUC.EXPER.2004 <- (cps04$EDUC)*(cps04$EXPER)
  EXPER.NONWHITE.2004 <- (cps04$EXPER)*(cps04$NONWHITE)
  EXPER.FEMALE.2004 <- (cps04$EXPER)*(cps04$FEMALE)
  EXPER.UNION.2004 <- (cps04$EXPER)*(cps04$UNION)
  NONWHITE.FEMALE.2004 <- (cps04$NONWHITE)*(cps04$FEMALE)
  NONWHITE.UNION.2004 <- (cps04$NONWHITE)*(cps04$UNION)
  FEMALE.UNION.2004 <- (cps04$FEMALE)*(cps04$UNION)
  
  EDUCsquared.2012 <- (cps12$EDUC)^2
  EXPERsquared.2012 <- (cps12$EXPER)^2
  EDUC.NONWHITE.2012 <- (cps12$EDUC)*(cps12$NONWHITE)
  EDUC.FEMALE.2012 <- (cps12$EDUC)*(cps12$FEMALE)
  EDUC.UNION.2012 <- (cps12$EDUC)*(cps12$UNION)
  EDUC.EXPER.2012 <- (cps12$EDUC)*(cps12$EXPER)
  EXPER.NONWHITE.2012 <- (cps12$EXPER)*(cps12$NONWHITE)
  EXPER.FEMALE.2012 <- (cps12$EXPER)*(cps12$FEMALE)
  EXPER.UNION.2012 <- (cps12$EXPER)*(cps12$UNION)
  NONWHITE.FEMALE.2012 <- (cps12$NONWHITE)*(cps12$FEMALE)
  NONWHITE.UNION.2012 <- (cps12$NONWHITE)*(cps12$UNION)
  FEMALE.UNION.2012 <- (cps12$FEMALE)*(cps12$UNION)
}
##Histograms
hist(cps95$LNWAGE, xlab = "Ln of Wage", ylab = "Frequency", main = "1995 Ln(Wage) Distribution", breaks = seq(from = 0.5, to = 4.5, by = 0.25))
hist(cps04$LNWAGE, xlab = "Ln of Wage", ylab = "Frequency", main = "2004 Ln(Wage) Distribution", breaks = seq(from = 0.5, to = 4.5, by = 0.25))
hist(cps12$LNWAGE, xlab = "Ln of Wage", ylab = "Frequency", main = "2012 Ln(Wage) Distribution", breaks = seq(from = 0.5, to = 4.5, by = 0.25))

##Getting the Appropriate 

##Kurtosis, Skewness and Jarque-Bera Tests
##95
skewness.norm.test(cps95$LNWAGE)
kurtosis.norm.test(cps95$LNWAGE)
jb.norm.test(cps95$LNWAGE, nrepl = 2000)
qqPlot(cps95$LNWAGE, xlab = "Normal Quantiles", ylab = "LN(Wage)", main = "QQ Plot of 1995 CPS LN(WAGE)")
##04
skewness.norm.test(cps04$LNWAGE)
kurtosis.norm.test(cps04$LNWAGE)
jb.norm.test(cps04$LNWAGE, nrepl = 2000)
qqPlot(cps04$LNWAGE, xlab = "Normal Quantiles", ylab = "LN(Wage)", main = "QQ Plot of 2004 CPS LN(WAGE)")
##12
skewness.norm.test(cps12$LNWAGE)
kurtosis.norm.test(cps12$LNWAGE)
jb.norm.test(cps12$LNWAGE, nrepl = 2000)
qqPlot(cps12$LNWAGE, xlab = "Normal Quantiles", ylab = "LN(Wage)", main = "QQ Plot of 2012 CPS LN(WAGE)")

##Bivariate Analysis
##1995
plot(LNWAGE ~ EXPER, data = cps95, col = "black", xlab = "Experience", ylab = "Ln(Wage)", main = "Ln(Wage) vs Work Experience, CPS 1995")
with(cps95, lines(loess.smooth(EXPER, y = LNWAGE), col = "red", lwd = 2))
plot(LNWAGE ~ EDUC, data = cps95, col = "black", xlab = "Education", ylab = "Ln(Wage)", main = "Ln(Wage) versus Work Education, CPS 1995")
with(cps95, lines(loess.smooth(EDUC, y = LNWAGE), col = "red", lwd = 2))
##Wage by Union Status
boxplot(LNWAGE~ifelse(UNION == 1, yes = "Union", no = "Non-Union"),data=cps95, main="LN(Wage) by Union Status, CPS 1995", 
        xlab="Union Status", ylab="LN(Wage)")

##Wage by Gender
boxplot(LNWAGE~ifelse(FEMALE == 1, yes = "Female", no = "Male"),data=cps95, main="LN(Wage) by Gender, CPS 1995", 
        xlab="Gender", ylab="LN(Wage)")

##Wage by Race
boxplot(LNWAGE~ifelse(NONWHITE == 1, yes = "Minority", no = "White"), data=cps95, main="LN(Wage) by Minority Status, CPS 1995", 
        xlab="Minority", ylab="LN(Wage)")

##2004
plot(LNWAGE ~ EXPER, data = cps04, col = "black", xlab = "Experience", ylab = "Ln(Wage)", main = "Ln(Wage) versus Work Experience, CPS 2004")
with(cps95, lines(loess.smooth(EXPER, y = LNWAGE), col = "red", lwd = 2))
plot(LNWAGE ~ EDUC, data = cps04, col = "black", xlab = "Education", ylab = "Ln(Wage)", main = "Ln(Wage) versus Work Education, CPS 2004")
with(cps95, lines(loess.smooth(EDUC, y = LNWAGE), col = "red", lwd = 2))
##Wage by Union Status
boxplot(LNWAGE~ifelse(UNION == 1, yes = "Union", no = "Non-Union"),data=cps04, main="LN(Wage) by Union Status, CPS 2004", 
        xlab="Union Status", ylab="LN(Wage)")

##Wage by Gender
boxplot(LNWAGE~ifelse(FEMALE == 1, yes = "Female", no = "Male"),data=cps04, main="LN(Wage) by Gender, CPS 2004", 
        xlab="Gender", ylab="LN(Wage)")

##Wage by Race
boxplot(LNWAGE~ifelse(NONWHITE == 1, yes = "Minority", no = "White"), data=cps04, main="LN(Wage) by Minority Status, CPS 2004", 
        xlab="Minority", ylab="LN(Wage)")

##2012
plot(LNWAGE ~ EXPER, data = cps12, col = "black", xlab = "Experience", ylab = "Ln(Wage)", main = "Ln(Wage) versus Work Experience, CPS 2012")
with(cps95, lines(loess.smooth(EXPER, y = LNWAGE), col = "red", lwd = 2))
plot(LNWAGE ~ EDUC, data = cps12, col = "black", xlab = "Education", ylab = "Ln(Wage)", main = "Ln(Wage) versus Work Education, CPS 2012")
with(cps95, lines(loess.smooth(EDUC, y = LNWAGE), col = "red", lwd = 2))
##Wage by Union Status
boxplot(LNWAGE~ifelse(UNION == 1, yes = "Union", no = "Non-Union"),data=cps12, main="LN(Wage) by Union Status, CPS 2012", 
        xlab="Union Status", ylab="LN(Wage)")

##Wage by Gender
boxplot(LNWAGE~ifelse(FEMALE == 1, yes = "Female", no = "Male"),data=cps12, main="LN(Wage) by Gender, CPS 2012", 
        xlab="Gender", ylab="LN(Wage)")

##Wage by Race
boxplot(LNWAGE~ifelse(NONWHITE == 1, yes = "Minority", no = "White"), data=cps12, main="LN(Wage) by Minority Status, CPS 2012", 
        xlab="Minority", ylab="LN(Wage)")

##RegSubsets testing
regressors.1995 <- matrix(data = c(cps95$EDUC, cps95$EXPER, cps95$FEMALE, cps95$NONWHITE, cps95$UNION, EDUCsquared.1995, EXPERsquared.1995, EDUC.EXPER.1995, EDUC.FEMALE.1995, EDUC.NONWHITE.1995, EDUC.UNION.1995, EXPER.FEMALE.1995, EXPER.NONWHITE.1995, EXPER.UNION.1995, NONWHITE.FEMALE.1995, NONWHITE.UNION.1995, FEMALE.UNION.1995), nrow = 1320, ncol = 17, byrow = FALSE)
colnames(regressors.1995) <- c("ED", "EXP", "Fem", "Non-white", "Union", "ED^2", "EXP^2", "ED X EXP", "Ed X Female", "ED X NW", "ED X Union", "EX X FEM", "EX X NW", "EX X UN", "NW X FEM", "NW X UN", "FEM X UN")
regressand.1995 <- c(cps95$LNWAGE)
models.1995 <- regsubsets(x = regressors.1995, y = regressand.1995, nested=FALSE)
plot(models.1995, main ="1995 Reg-Subsets Results", xlab = "Variables", ylab = "Schwarz Information Criterion (Improvement Over Just Intercept)")

regressors.2004 <- matrix(data = c(cps04$EDUC, cps04$EXPER, cps04$FEMALE, cps04$NONWHITE, cps04$UNION, EDUCsquared.2004, EXPERsquared.2004, EDUC.EXPER.2004, EDUC.FEMALE.2004, EDUC.NONWHITE.2004, EDUC.UNION.2004, EXPER.FEMALE.2004, EXPER.NONWHITE.2004, EXPER.UNION.2004, NONWHITE.FEMALE.2004, NONWHITE.UNION.2004, FEMALE.UNION.2004), nrow = 1925, ncol = 17, byrow = FALSE)
colnames(regressors.2004) <- c("ED", "EXP", "Fem", "Non-white", "Union", "ED^2", "EXP^2", "ED X EXP", "Ed X Female", "ED X NW", "ED X Union", "EX X FEM", "EX X NW", "EX X UN", "NW X FEM", "NW X UN", "FEM X UN")
regressand.2004 <- c(cps04$LNWAGE)
models.2004 <- regsubsets(x = regressors.2004, y = regressand.2004)
plot(models.2004, main = "2004 Reg-Subsets Results")

regressors.2012 <- matrix(data = c(cps12$EDUC, cps12$EXPER, cps12$FEMALE, cps12$NONWHITE, cps12$UNION, EDUCsquared.2012, EXPERsquared.2012, EDUC.EXPER.2012, EDUC.FEMALE.2012, EDUC.NONWHITE.2012, EDUC.UNION.2012, EXPER.FEMALE.2012, EXPER.NONWHITE.2012, EXPER.UNION.2012, NONWHITE.FEMALE.2012, NONWHITE.UNION.2012, FEMALE.UNION.2012), nrow = 1223, ncol = 17, byrow = FALSE)
colnames(regressors.2012) <- c("ED", "EXP", "Fem", "Non-white", "Union", "ED^2", "EXP^2", "ED X EXP", "Ed X Female", "ED X NW", "ED X Union", "EX X FEM", "EX X NW", "EX X UN", "NW X FEM", "NW X UN", "FEM X UN")
regressand.2012 <- c(cps12$LNWAGE)
models.2012 <- regsubsets(x = regressors.2012, y = regressand.2012)
plot(models.2012, main="2012 Reg-Subsets Results")

##Regression Fitting and BIC Scores
##1995
out95 = lm(cps95$LNWAGE ~ cps95$EDUC + cps95$EXPER + cps95$FEMALE + cps95$NONWHITE + cps95$UNION + EXPERsquared.1995 + EDUC.EXPER.1995)

summary(out95)
regsub.95 <- lm(cps95$LNWAGE ~ cps95$EDUC + cps95$EXPER + cps95$FEMALE + cps95$NONWHITE+ cps95$UNION+ EXPERsquared.1995+ EDUC.EXPER.1995 + EXPER.FEMALE.1995)
BIC(out95, regsub.95)
###################################################################################################
######################################################################################################

##2004
out04 = lm(cps04$LNWAGE ~ cps04$EXPER + cps04$FEMALE + cps04$NONWHITE + cps04$UNION+ EDUCsquared.2004+ EXPERsquared.2004)
summary(out04)
regsub.04 <- lm(cps04$LNWAGE ~ cps04$EXPER + cps04$FEMALE + cps04$UNION+ EDUCsquared.2004+ EXPERsquared.2004+ EXPER.NONWHITE.2004)
BIC(out04, regsub.04)

######################################################################################################
######################################################################################################

##2012
out12 = lm(cps12$LNWAGE ~ cps12$EXPER + cps12$FEMALE + cps12$UNION + EXPER.NONWHITE.2012 + EDUCsquared.2012+ EXPERsquared.2012)
summary(out12)
regsub.12 <- lm(cps12$LNWAGE ~ cps12$EXPER + cps12$UNION+ EDUCsquared.2012+ EXPERsquared.2012 + EDUC.FEMALE.2012+ EXPER.NONWHITE.2012)
summary(regsub.12)
BIC(out12, regsub.12)

######################################################################################################
######################################################################################################

##Stargazer!
stargazer(exp(out95$coefficients), exp(out04$coefficients), exp(out12$coefficients))
stargazer(out95)
stargazer(out04)
stargazer(out12)
##Testing For Heteroskedasticity Using White's Test
##Function for White's Test
whiteTest = function(frame) {
regression = lm(frame$RESID2 ~ FEMALE + UNION + EXPER + EDUC + NONWHITE +
                  EXPER*EXPER + EDUC*EDUC + EXPER * EDUC + FEMALE*UNION + FEMALE*EXPER + FEMALE*EDUC + FEMALE *NONWHITE +
                  UNION *EXPER + UNION*EDUC + UNION*NONWHITE + EXPER*NONWHITE + EDUC*NONWHITE, data = frame)
r2 <- summary(regression)$r.squared;
chiValue <-nrow(frame)*r2;
pVal <- pchisq(chiValue, 8, lower.tail = FALSE);
return(list(pVal, regression))
}

##95
##Calculating Squared Residuals
cps95 = cbind(cps95, (predict(out95, data = cps95) - cps95$LNWAGE) ^ 2)
colnames(cps95)[9] = "RESID2"
hetero95 = whiteTest(cps95)
hetero95[[1]]

##Recalculating Error terms for non-normality
resid95 = (predict(out95, newdata = cps95) - cps95$LNWAGE) / sqrt(predict(hetero95[[2]], newdata = cps95))
hist(resid95, breaks = 15, main = "Residual Distribution, CPS 1995", xlab = "Residual Value")
qqPlot(resid95, main = "QQ Plot of Residuals, CPS 1995", xlab = "Normal Quantiles", ylab = "Residual Value")
skewness(resid95, na.rm = TRUE)
kurtosis(resid95, na.rm = TRUE) + 3
jb.norm.test(resid95[!is.na(resid95)], nrepl = 2000)

##Calculating Squared Residuals
##04
cps04 = cbind(cps04, (predict(out04, data = cps04) - cps04$LNWAGE) ^ 2)
colnames(cps04)[9] = "RESID2"
hetero04 = whiteTest(cps04)
hetero04[[1]]

##Recalculating Error terms for non-normality
resid04 = (predict(out04, newdata = cps04) - cps04$LNWAGE) / sqrt(predict(hetero04[[2]], newdata = cps04))
##which.min(resid04)
##resid04 = resid04[-360]
hist(resid04, breaks = 15, main = "Residual Distribution, CPS 2004", xlab = "Residual Value")
qqPlot(resid04, main = "QQ Plot of Residuals, CPS 2004", xlab = "Normal Quantiles", ylab = "Residual Value")
skewness(resid04, na.rm = TRUE)
kurtosis(resid04, na.rm = TRUE) + 3
jb.norm.test(resid04[!is.na(resid04)], nrepl = 2000)

##Calculating Squared Residuals
##12
cps12 = cbind(cps12, (predict(out12, data = cps12) - cps12$LNWAGE) ^ 2)
colnames(cps12)[9] = "RESID2"

hetero12 = whiteTest(cps12)
hetero12[[1]]

##Recalculating Error terms for non-normality
resid12 = (predict(out12, newdata = cps12) - cps12$LNWAGE) / sqrt(predict(hetero12[[2]], newdata = cps12))
##which.min(resid04)
##resid04 = resid04[-360]
hist(resid12, breaks = 15, main = "Residual Distribution, CPS 2012", xlab = "Residual Value")
qqPlot(resid12, main = "QQ Plot of Residuals, CPS 2012", xlab = "Normal Quantiles", ylab = "Residual Value")
skewness(resid12, na.rm = TRUE)
kurtosis(resid12, na.rm = TRUE) + 3
jb.norm.test(resid12[!is.na(resid12)], nrepl = 2000)

##Point Prediction, and Interval and Density Forecasts for new person
##(union, white, female, 12 years education, 3 years experience)
##For interval we use 1.96 * Fitted SE using hetoroskeadstic model
##Density we use simulation of Gaussian disturbances given a set standard deviation.
prediction <- function(year){
  point = 0
  sdEst = 0
  Female = 1
  Nonwhite = 0
  Union = 1
  Educ = 12
  Age = 21
  Exp = 3
  if (year == 1995) {
     point = out95$coefficients[[1]] + out95$coefficients[[2]] * Educ + out95$coefficients[[3]] * Exp + out95$coefficients[[4]] * Female +
       out95$coefficients[[5]] * Nonwhite + out95$coefficients[[6]] * Union + out95$coefficients[[7]] * Exp * Exp +
       out95$coefficients[[8]] * Educ * Exp
     print(point)
     sdEst = sqrt(predict(object = hetero95[[2]], newdata = newPerson))
  } else if (year == 2004) {
    point = out04$coefficients[[1]] + out04$coefficients[[2]] * Exp + out04$coefficients[[3]] * Female + out04$coefficients[[4]] * Nonwhite +
      out04$coefficients[[5]] * Union + out04$coefficients[[6]] * Educ * Educ + out04$coefficients[[7]] * Exp * Exp
    sdEst = sqrt(predict(object = hetero04[[2]], newdata = newPerson))  
  } else if (year == 2012) {
    
    point = out12$coefficients[[1]] + out12$coefficients[[2]] * Exp + out12$coefficients[[3]] * Female + out12$coefficients[[4]] * Union +
      out12$coefficients[[5]] * Exp * Nonwhite + out12$coefficients[[6]] * Educ * Educ + out12$coefficients[[7]] * Exp * Exp
    sdEst = sqrt(predict(object = hetero12[[2]], newdata = newPerson))
  }
  
  interval = sdEst*1.96  
  densitySimulation = matrix(0,10000)
  for (i in 1:10000){
    densitySimulation[i] = rnorm(1, mean = point, sd= sdEst)
  }
  return(list(densitySimulation, point, interval))
}
##1995
predictions1995 = prediction(1995)
hist(predictions1995[[1]], breaks = 25, main = "Density Forecast 1995 New Comer", xlab = "Simulated Ln(Wage), 1999 Dollars", ylab = "Frequency")
predictions1995[[2]] - predictions1995[[3]]
predictions1995[[2]] + predictions1995[[3]]
exp(predictions1995[[2]])

##2004
predictions2004 = prediction(2004)
hist(predictions2004[[1]], breaks = 25, main = "Density Forecast 2004 New Comer", xlab = "Simulated Ln(Wage), 1999 Dollars", ylab = "Frequency")
predictions2004[[2]] - predictions2004[[3]]
predictions2004[[2]] + predictions2004[[3]]
exp(predictions2004[[2]])

##2012
predictions2012 = prediction(2012)
hist(predictions2012[[1]], breaks = 25, main = "Density Forecast 2012 New Comer", xlab = "Simulated Ln(Wage), 1999 Dollars", ylab = "Frequency")
predictions2012[[2]] - predictions2012[[3]]
predictions2012[[2]] + predictions2012[[3]]
exp(predictions2012[[2]])

##Calculating White's Robust Standard Errors
robust_se.1995 <- sqrt(diag(vcovHC(out95, type = "HC"))) 
print(robust_se.1995)
robust_se.2004 <- sqrt(diag(vcovHC(out04, type = "HC"))) 
print(robust_se.2004)
robust_se.2012 <- sqrt(diag(vcovHC(out12, type = "HC")))
print(robust_se.2012)

#Using White Robust Standard Errors to Adjust T.Stats
coefficients.1995 <- c(coefficients(out95))
new.t.stats.1995 <- coefficients.1995/robust_se.1995
print(new.t.stats.1995)

coefficients.2004 <- c(coefficients(out04))
new.t.stats.2004 <- coefficients.2004/robust_se.2004
print(new.t.stats.2004)

coefficients.2012 <- c(coefficients(out12))
new.t.stats.2012 <- coefficients.2012/robust_se.2012
print(new.t.stats.2012)

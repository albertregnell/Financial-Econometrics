#Assignment 2. Dataset 148

#Making required packages available

library(xts)
library(forecast)
library(TSSS)

#loading dataset 148

load(file = "Assignment 2 2022 dataset 148.RData")
nrow(data) 

#### Series 1 analysis ####

s1 <- data$series1

#Plots
par(mfrow = c(3,1))
plot.ts(s1, main = "Plot against time")
acf(s1, lag = 15, main = "Autocorrelation plot")
pacf(s1, lag = 15, main = "Partial autocorrelation plot")
par(mfrow = c(1,1))

#ACF abruptly disappears after 1 spike
#PACF i would say tapers off more
#this result would indicate a MA(1) process
#but the PACF is difficult to interpret

#Lets try the MA(1) model

summary(fit1 <- Arima(s1, order = c(0,0,1), include.mean = TRUE))
fit1$code
acf(fit1$residuals)

#the coefficient is really significant, which is good
#it converges to 0
#The ACF residuals looks really good with everything under the significant level
#This pretty much confirms its the right model

Box.test(resid(fit1), type = "Ljung-Box", lag=5, fitdf = (0+1))

#p-value is 0,2698, so we cant reject null-hypothesis, which is good.


#### Series 2 analysis ####

s2 <- data$series2

#Plots
par(mfrow = c(3,1))
plot.ts(s2, main = "Plot against time")
acf(s2, lag = 15, main = "Autocorrelation plot")
pacf(s2, lag = 15, main = "Partial autocorrelation plot")
par(mfrow = c(1,1))

#ACF tapers off
#PACF is only one spike, then almost 0
#Its a obvious RA(1) model
#from the plot against time it looks to have negative coeff - a lot of up and down

#Lets try the RA(1) model

summary(fit2 <- Arima(s2, order = c(1,0,0), include.mean = TRUE))
fit2$code
acf(fit2$residuals)

#the coeff is very significant an negative, as aspected
#converges
#the acf residuals looks awesome.everything equals 0.
#RA(1) is the correct model

#### Series 3 analysis ####

s3 <- data$series3

#Plots
par(mfrow = c(3,1))
plot.ts(s3, main = "Plot against time")
acf(s3, lag = 15, main = "Autocorrelation plot")
pacf(s3, lag = 15, main = "Partial autocorrelation plot")
par(mfrow = c(1,1))


#looks to have positive coeff
#the acf tenders off
#the PACF looks goes much lower after first spike, but looks like it may tender off
#looks like a mixed one, i will have to try several alternatives

#first we try at pure AR model
summary(fit3.1 <- Arima(s3, order = c(1,0,0), include.mean = TRUE))
fit3.1$code
acf(fit3.1$residuals)

#the residuals looks bad. 
#Lets try a mixed model

summary(fit3.3 <- Arima(s3, order = c(3,0,3), include.mean = TRUE))
fit3.3$code
acf(fit3.3$residuals)

#ar3 is not significant. 
#log likeloof = -1202,33. BIC = 2452,01 AIC = 2420,66
#converges
#the acf residuals looks awesome. one spike then everything equals 0.

#It may overparametrised, so i will try on lower level of ar

summary(fit3.2.3 <- Arima(s3, order = c(2,0,3), include.mean = TRUE))
fit3.2.3$code
acf(fit3.2.3$residuals)

#ma 3 nor ar2 is significant 
#LL = -1203. AIC = 2416, BIC = 2447

summary(fit3.1.2 <- Arima(s3, order = c(1,0,2), include.mean = TRUE))
fit3.1.2$code
acf(fit3.1.2$residuals)

#now ar1 and ma2 is significant
#AIC = 2416, BIC = 2436, LL = -1203

#but will try ARMA(1,0,1) and compare
summary(fit3.1.1 <- Arima(s3, order = c(1,0,1), include.mean = TRUE))
fit3.1.1$code
acf(fit3.1.1$residuals)

#both are really significant. LL = -1208,04. AIC = 2424, BIC = 2439
#converge
#residuals looking good, only one outside lines

#Will have to do the likelihood ration test. Maybe the Ljung-box test as well

#Likelihood ratio test. ARMA(1,0,1) vs ARMA(1,0,2)

(lrStatistic <- -2 * (fit3.1.1$loglik - fit3.1.2$loglik))
# number of restrictions is 1 so LR test done with Chi-squared 1 DF.
( lrCriticalValue <- qchisq(0.05,1, lower.tail = FALSE) )
( lrPValue <- pchisq(lrStatistic, 1, lower.tail = FALSE ) )

#LR us 9>3,8. So we can reject the 0-hypothesis. p-value = 0,002. So ARMA(1,0,2) model.

#to show it even more we can look at the ljung box
Box.test(resid(fit3.1.2), type = "Ljung-Box", lag=5, fitdf = (1+2))
Box.test(resid(fit3.1.1), type = "Ljung-Box", lag=5, fitdf = (1+1))
Box.test(resid(fit3.3), type = "Ljung-Box", lag=10, fitdf = (3+3))

#### Series 4 analysis ####

s4 <- data$series4

#Plots
par(mfrow = c(3,1))
plot.ts(s4, main = "Plot against time")
acf(s4, lag = 15, main = "Autocorrelation plot")
pacf(s4, lag = 15, main = "Partial autocorrelation plot")
par(mfrow = c(1,1))

#Looks to have positive coefficients
#ACF tapers off but is almost non-discretionary
#PACF looks like it disappears after 6 spikes. Hard to say
#this would suggest a AR(6) model

#Lets try the RA(6) model

summary(fit4.6 <- Arima(s4, order = c(6,0,0), include.mean = TRUE))
fit4.6$code
acf(fit4.6$residuals)

(t <- 0.0916/0.0518) 
(critialvalue <- qt(0.975, df = 365, lower.tail = TRUE))

#the residuals looks really good, but ar6 is not significant. ar5 are though
#maybe its overparametrized and we can take order 5 instead
#LL = -541,25. AIC = 1098,54, BIC = 1129,88

summary(fit4.5 <- Arima(s4, order = c(5,0,0), include.mean = TRUE))
fit4.5$code
acf(fit4.5$residuals)

#the ar5 is very significant
#it converges to 0
#The ACF residuals looks really good with everything under the significant level
#LL = -542,82. AIC = 1099,64. BIC=1127,07. BIC is better

#can do a likelihood test but its pretty obvious ar(5) is better when looking at BIC and LL

#Just to check to take even lower

summary(fit4.4 <- Arima(s4, order = c(4,0,0), include.mean = TRUE))
fit4.4$code
acf(fit4.4$residuals)

#residuals much worse. The AIC and BIC became much higher.. LL is worse as well
#Pretty sure AR(5)

#LL-test AR(5) vs AR(6)

(lrStatistic <- -2 * (fit4.5$loglik - fit4.6$loglik))
# number of restrictions is 1 so LR test done with Chi-squared 1 DF.
( lrCriticalValue <- qchisq(0.05,1, lower.tail = FALSE) )
( lrPValue <- pchisq(lrStatistic, 1, lower.tail = FALSE ) )

#p value is 0,077. So we cant reject null hypothesis and will use the AR(5)
#Lets check the ljung box for the AR(5)

Box.test(resid(fit4.5), type = "Ljung-Box", lag=7, fitdf = (5))

#the p value is 0.1377. So the residuals are OK. We decide for the AR(5) model

#### Series 5 analysis ####

s5 <- data$series5

#Plots
par(mfrow = c(3,1))
plot.ts(s5, main = "Plot against time")
acf(s5, lag = 15, main = "Autocorrelation plot")
pacf(s5, lag = 15, main = "Partial autocorrelation plot")
par(mfrow = c(1,1))

#negative coeff for sure
#unclear ACF. Has spikes at 3 and 4. Then disappears
#PACF tapers off, but almost non-discreationary
#Suggest a MA(4) model

#Lets try the MA(4) model

summary(fit5.4 <- Arima(s5, order = c(0,0,4), include.mean = TRUE))
fit5.4$code
acf(fit5.4$residuals)

#ma4 is really significant. LL=-778, AIC=1568. BIC=1591
#it converges to 0
#The ACF residuals looks really good with everything under the significant level
#This pretty much confirms its the right model
#To be sure, i will try MA(3) as well

summary(fit5.3 <- Arima(s5, order = c(0,0,3), include.mean = TRUE))
fit5.3$code
acf(fit5.3$residuals)

#everything is worse, residuals, AIC, BIC and LL much worse
#for sure the MA(4) model. Needs no more testing.

#for fun, lets check ljung-box

Box.test(resid(fit5.4), type = "Ljung-Box", lag=7, fitdf = (4))

#pvalue = 0.54. Very good.

#install.packages(c("xts"))
library(xts)
load("Assignment 1 dataset 158.RData")

N <- ncol(fundTotalReturnIndices)

# Make sure data is what I expect
# summary()
# plot()

# Compute rates of return from total return indices
# simple rate of return V(t)/V(t-1)-1
# lag the data and get rid of NA values

laggedFundTotalReturnIndices <- lag(fundTotalReturnIndices, k = 1)
fundRatesOfReturn <- na.omit(fundTotalReturnIndices/laggedFundTotalReturnIndices-1)
                        
# do the same thing for the market index, leaves that to us

# exxess is the return minus the risk free rate
# create the excess rate of return

excessFundRatesOfReturn <- fundRatesOfReturn
for(fund in 1:N) {
  excessFundRatesOfReturn[, fund] <- fundRatesOfReturn[, fund] - riskFreeRateOfReturn
}

# calculate mean and var/cov for every fund
(meanFundExcessReturns <- colMeans(excessFundRatesOfReturn))

varCovForFund <- var(excessFundRatesOfReturn)

(r_f <- tail(riskFreeRateOfReturn, n = 1))

(meanFundReturns <- meanFundExcessReturns + r_f[[1]])

# get the data to excel

write.csv(varCovForFund, "variance.csv", row.names = TRUE)
write.csv(meanFundReturns, "meanFundReturns.csv", row.names = TRUE)

#calculate kurtosis

optimalPortWeights <- c(0.468239974,
                        -0.322542174,
                        0.342500346,
                        0.111757818,
                        -0.151835311,
                        0.404480513,
                        0.112371916,
                        0.653664459,
                        -1.154423801,
                        0.50887923,
                        0.026907014)
fundAndRiskFreeReturn <- cbind(fundRatesOfReturn, riskFreeRateOfReturn) #adding riskfree returns
optimalPortReturn <- t(optimalPortWeights %*% t(fundAndRiskFreeReturn)) #calculating the actual return for optimal portfolio
summary(optimalPortReturn) #ungefär samma mean som enligt excel
var(optimalPortReturn) # same variance

(excessKurtosisOptimalPort <- mean(moments::kurtosis(optimalPortReturn))-3)
(skewnessOptimalPort <- mean(moments::skewness(optimalPortReturn)))

plot(optimalPortReturn)
plot(optimalPortWeights)
hist(optimalPortReturn, breaks = 10)


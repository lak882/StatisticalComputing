## Deliverables For Pairs Traiding 

## 1.
graphics <- downloadStockPairDF("AMD", "NVDA", 2011, nyears=10)
cor(graphics$stock1, graphics$stock2)
plotStocks(graphics)
plotRatio(graphics)

## 2.
ratio <- graphics$ratio
m <- mean(ratio)
s <- sd(ratio)

positions <- findPositions(ratio, m, s)
positions
addPositions(ratio, positions)

## 3.
openDay <- positions[[1]][1]
closeDay <- positions[[1]][2]
highLow <- positions[[1]][3]

graphics$stock1[openDay]
graphics$stock2[openDay]
graphics$stock1[closeDay]
graphics$stock2[closeDay]

highLow

positionProfit(graphics, positions, net=FALSE)
positionProfit(graphics, positions)

## 4.

## optimal k
optimalK <- findOptimalK(graphics)
optimalK

## profit at different k values
findOptimalK(graphics, plot=TRUE)

## ratio with optimal k
plotRatio(graphics, k=optimalK)
positions <- findPositions(ratio, m, s, optimalK)
positionProfit(graphics, positions)
positionProfit(graphics, positions, net=FALSE)
addPositions(ratio, positions)


## 5.
evaluatePairsTrading(graphics)
evaluatePairsTrading(graphics, plot=TRUE)

## evaluate many different types of pairs

## positive, sony and mcdonalds
positive <- downloadStockPairDF("SONY", "MCD", start=2012, nyear=5)
cor(positive$stock1, positive$stock2)
evaluatePairsTrading(positive, plot=TRUE)

## none, ford and the mcdonalds
none <- downloadStockPairDF("F", "MCD", start=2010, nyears=10)
cor(none$stock1, none$stock2)
evaluatePairsTrading(none, plot=TRUE)

## negative, ford and gold
negative <- downloadStockPairDF("F", "GLD", start=2010, nyear=10)
cor(negative$stock1, negative$stock2)
evaluatePairsTrading(negative, plot=TRUE)

##6.
profit <- simulateDistribution(n=1000, sigma1=1, sigma2=2, rho=0.9, psi=0.9)
## mean = 7.5334, std. error = 0.3295
profit.model <- lm(profit ~ 1)
summary(profit.model)
## conf interval 6.886744 & 8.180086
confint(profit.model, level=0.95)


correlation <- simulateDistribution(n=1000, sigma1=1, sigma2=2, rho=0.9, psi=0.9, returnCorrelation=TRUE)
## mean = 0.863305, std. error = 0.001597
correlation.model <- lm(correlation ~ 1)
summary(correlation.model)
## conf interval 0.8601711 & 0.8664381
confint(correlation.model, level=0.95)

##7.
rhoPsiHeatmap()

##8.
rhoPsiHeatmap(plotCorrelation=TRUE, n=1000)

##9.
## one additonal VAR parmater sigma1 vs sigma 2
sigmaHeatmap()

##Extension

## test to find optmalTF
findOptimalTF(graphics)
findOptimalTF(graphics, returnProfit=TRUE, plot=TRUE)
findOptimalTF(positive)
findOptimalTF(positive, returnProfit=TRUE, plot=TRUE)
findOptimalTF(none)
findOptimalTF(none, returnProfit=TRUE, plot=TRUE)
findOptimalTF(negative)
findOptimalTF(negative, returnProfit=TRUE, plot=TRUE)

## get heatmap with training fractions
rhoPsiHeatmap(plotTF=TRUE)


## Functions for Pairs Trading

library(BatchGetSymbols)

## 02-22
## return data frame of prices of a stock from start for nyears
downloadPriceDF <- function(stock, start = 2010, nyears = 1) {
    ## get first & last date
    first <- paste0(start, "-01-01")
    last <- paste0(start + nyears, "-01-01")
    
    ## download stock data
    stockData <- BatchGetSymbols(tickers = stock, first.date = first, last.date = last)
    stockPrices <- stockData$df.tickers
    
    ## get prices and dates (as string) in data frame
    price <- stockPrices$price.adjusted
    date <- as.character(stockPrices$ref.date)
    
    stockDF <- data.frame(date, price)
    return(stockDF)
}

## return dataframe of two stock prices and their ratio from start for nyears
downloadStockPairDF <- function(stock1, stock2, start = 2010, nyears = 1) {
    ## get both stocks
    stockDF1 <- downloadPriceDF(stock1, start, nyears)
    stockDF2 <- downloadPriceDF(stock2, start, nyears)
    
    ## make sure the date ranges are the same
    if (!identical(stockDF1$date, stockDF2$date)) {
        stop("Date ranges are not identical")
    }
    
    ## get prices, ratio of prices and put in data frame
    stock1 <- stockDF1$price
    stock2 <- stockDF2$price
    ratio <- c(stock1 / stock2)
    
    stockPairDF <- data.frame(stock1, stock2, ratio)
    return(stockPairDF)
}

## plot stock1 and stock2 
plotStocks <- function(stockDF) {
    ## get stocks
    stock1 <- stockDF$stock1
    stock2 <- stockDF$stock2
    ## get length of stocks 
    n <- length(stock1)
    
    ## make sure there is enough room to display stock2
    plot(1:n, stock1, type='l', col="red",  ylim=c(0,max(stock2)), xlab="Days", ylab="Price")
    lines(stock2, col="blue")
}

## plot ratio, mean of ratio, k-standard deviations above & below mean of ratio
plotRatio <- function(stockDF, k = 1) {
    ## get ratio of stocks
    ratio <- stockDF$ratio
    n <- length(ratio)
    
    ## plot ratio
    plot(1:n, ratio, type='l', xlab="Days", ylab="Ratio")
    ## plot mean of ratio
    abline(h = mean(ratio), col="red")
    ## plot standard deviations above and below ratio
    abline(h = mean(ratio) + k * sd(ratio), lty=2, col="blue")
    abline(h = mean(ratio) - k * sd(ratio), lty=2, col="blue")
}

## 02-25

## finds positions to open & close stocks    
findPositions <- function(ratio, m, s, k = 1) {
    positions <- list()
    upper <- m + k*s
    lower <- m - k*s
    current <- 1
    n <- length(ratio)
    
    while(current < n) {
        ## see if there is ever a day (greater than current day)
        ## when we would open a new pair of positions
        ## only open positons on >= current
        possibleOpenDays <- ((ratio > upper) | (ratio < lower)) & (1:n >= current)
        
        ## check if there are any possible open days
        if (any(possibleOpenDays)) {
            ## record when we would open positions (openDay)            
            openDay <- which(possibleOpenDays)[1]
            
            ## 1: bought high, -1: bought low
            ## set close days, depending on how it crosses the mean
            if (ratio[openDay] > upper) {
                highLow <- 1
                possibleCloseDays <- (ratio < m) & (1:n >= openDay)               
            } else if (ratio[openDay] < lower) {
                highLow <- -1
                possibleCloseDays <- (ratio > m) & (1:n >= openDay)
            }
            
            ## record when we would close the positions (closeDay)
            ## when it crosses the mean
            if (any(possibleCloseDays)) {
                closeDay <- which(possibleCloseDays)[1]
            } else {
                ## no more close days: set to final day
                closeDay <- n
            }
            
            ## append onto list
            positions[[length(positions) + 1]] <- c(openDay, closeDay, highLow)

            ## update close day
            current <- closeDay + 1
        }
        else {
            ## no open days ever, positions will be empty
            break
        }
    }
    
    return (positions)
}
    
## plot open / close positions 
addPositions <- function(ratio, positions, start=0, stocksDF=NULL) {
    for( p in positions ) {
        ## get open & close day
        openDay <- p[1]
        closeDay <- p[2]
        
        ## plot open in green / close in blue
        points(x=openDay+start, y=ratio[openDay], pch=19, col='green')
        points(x=closeDay+start, y=ratio[closeDay], pch=19, col='blue')
    }
}

## 3-1

## get profits from opening/closing stocks
positionProfit <- function(stockDF, positions, net=TRUE) {
    ## corner case: empty profits
    if ( length(positions) == 0 ) {
        if (net) {
            return(0)
        }
        return(c())
    }
        
    profits <- sapply(positions, function(pos) {
        openDay <- pos[1]
        closeDay <- pos[2]
        highLow <- pos[3]

        ## openDay
        ## calcualte the inital price of $1 of share
        ## high: sell stock1, buy stock2 on openDay
        ## low: sell stock2, buy stock1 on openDay
        open1 <- 1/stockDF$stock1[openDay]
        open2 <- 1/stockDF$stock2[openDay]

        ## closeDay
        ## calculate how much our shares from openDay are worth
        close1 <- open1 * stockDF$stock1[closeDay]
        close2 <- open2 * stockDF$stock2[closeDay]
        if(highLow == 1) {
            ## high: buy back stock1, sell stock2
            profit <- close2 - close1
        }
        if(highLow == -1) {
            ## low: buy back stock2, sell stock1
            profit <- close1 - close2
        }

        ## fees from openDay & closeDay transactions
        fees <- 0.003 * (1 + 1 + close1 + close2)
        
        return(profit - fees)
    })

    
    if (net) {
        return(sum(profits))
    }
    return(profits)

}

## 3-4

## find the k-value that maximizes profits
findOptimalK <- function(stocksDF, plot = FALSE) {
    ratio <- stocksDF$ratio
    m <- mean(ratio)
    s <- sd(ratio)

    ## get difference of ratio and mean
    kMeanDif <- sapply(ratio, function(r) {
     return (abs(r - m))
    })

    ## see how many standard deviations above the mean the biggest
    ## deviation is
    kMax <- max(kMeanDif) / s
    ## get grid from 0 to kmax
    kValues <- seq(0, kMax, length = 100)
    
    ## convert kvalues to profit values
    kProfit <- sapply(kValues, function(k) {
        positions <- findPositions(ratio, m, s, k)
        profit <- positionProfit(stocksDF, positions)
        return(profit)
    })

    optimalK <- kValues[which(kProfit == max(kProfit))][1]
    ## plot kvalues against their profit
    if (plot) {
        plot(x=kValues, y=kProfit, type="l", xlab="k Values", ylab="Profit")
        ## line of optimal k
        abline(v=optimalK, lty=2, col="red")
    }

    ## search the grid for the maximum profit
    return(optimalK)
}

## determine profits from pairs traiding
## using the first half of the data as training data to get historic values
## & the second half of the data as testing data to apply pairs traiding to
evaluatePairsTrading <- function(stocksDF, trainningFrac = 0.5, plot = FALSE) {
    days <- nrow(stocksDF)
    ## round down number of days to the day at the half-way-point
    split <- floor(days * trainningFrac)

    ## get all days before the half-way-point
    train <- stocksDF[1:split,]
    ## get all the days after the half-way-point
    test <- stocksDF[(split+1):days,]

    ## train gives historic mean, standard devation, optimal k
    trainRatio <- train$ratio
    m <- mean(trainRatio)
    s <- sd(trainRatio)
    k <- findOptimalK(train)

    ## test gives ratio
    testRatio <- test$ratio

    ## find profits
    positions <- findPositions(testRatio, m, s, k)
    profit <- positionProfit(test, positions)

    ## plot train data w/ m, s and optimal k
    ## then plot test data with positions
    ## put them next to each other
    if (plot) {
        ## plot ratio
        plot(1:days, stocksDF$ratio, type='l', xlab="Days", ylab="Ratio")
        ## plot mean of ratio
        abline(h = m, col="red")
        ## plot standard deviations above and below ratio
        abline(h = m + k * s, lty=2, col="blue")
        abline(h = m - k * s, lty=2, col="blue")

        ## add line that divides test days and train days
        abline(v = split, lty=1, col="black")

        ## add positions, but starting after the split 
        addPositions(testRatio, positions, start=split+1)
    }

    return(profit)
}

## simulate two correlated stocks
simulateStockPair <- function(n=1000, sigma1=1, sigma2=1, rho=1, psi=0, b1=0, b2=0, plot=FALSE) {
    stock1 <- c()
    stock2 <- c()
    
    ## values for intercepts of x and y
    a1 <- 20
    a2 <- 30

    ## inital values for x1, x2 (always changing)
    x1 <- 10
    x2 <- 10

    ## stock prices for the first values
    stock1[1] <- a1 + b1*1 + 10 + rnorm(1,sd=sigma1)
    stock2[1] <- a2 + b2*1 + 10 + rnorm(1,sd=sigma2)
    
    ## set index of previous
    x1PreIndex <- x1 
    x2PreIndex <- x2
    
    for( index in 2:n ) {
        ## get hidden process for every value using previous index
        x1 <- rho * x1PreIndex + (1-rho) * psi * x2PreIndex + rnorm(1,sd=sigma1)
        x2 <- rho * x2PreIndex + (1-rho) * psi * x1PreIndex + rnorm(1,sd=sigma2)

        ## then add to the index
        stock1[index] <- a1 + b1*index + x1
        stock2[index] <- a2 + b2*index + x2

        ## set previous index
        x1PreIndex <- x1
        x2PreIndex <- x2
    }

    ## get ratio
    ratio <- c(stock1 / stock2)

    ## put in data frame
    stocksDF <- data.frame(stock1, stock2, ratio)

    ## just plot two stocks together
    if(plot) {
        plotStocks(stocksDF)
    }

    return(stocksDF)
}

## simulate the distribution of nreps of some simulation with parameters that are user-defined ...
simulateDistribution <- function(nrep=100, returnCorrelation=FALSE, returnTF=FALSE, ...) {
    ## simluate nrep times
    profits <- sapply(1:nrep, function(n) {
        df <- simulateStockPair(...)
        ## get each stock pair
        ## if you want to get correlation
        if (returnCorrelation) {
            return(cor(df$stock1, df$stock2))
        }
        ## if you want to get optimal training fractions
        if (returnTF) {
            return(findOptimalTF(df))
        }
        ## fi you want to get profits
        return(evaluatePairsTrading(df))
    })
    return(profits)
}

library(ggplot2)

## get a heatmap of profits at different rho, psi values
## plotCorrelation to get heatmap of correlations
## count to change the # of rho and psi values
rhoPsiHeatmap <- function(plotCorrelation=FALSE, plotTF=FALSE, count=5, ...) {
    ## make a data frame with every combination of n  values of rho & psi
    ## where rho in [0, 1] and psi [-1, 1]
    rho <- seq(0.1, 0.8, length.out=count)
    psi <- seq(-0.8, 0.8, length.out=count)
    rhoPsi <- expand.grid(rho=rho, psi=psi)

    ## calcualte the mean of every rho, psi
    means <- sapply(1:nrow(rhoPsi), function(n) {
        ## rho, psi at that n
        rhoN <- rhoPsi$rho[n]
        psiN <- rhoPsi$psi[n]
        ## simulate the distribution with those rho, psi values
        dist <- simulateDistribution(returnCorrelation=plotCorrelation, returnTF=plotTF, rho=rhoN, psi=psiN, ...)
        ## then get the mean
        return(mean(dist))
    })

    ## add it to the data frame
    rhoPsi$mean <- means

    ## make a heat map 
    ggplot(rhoPsi, aes(x=rho, y=psi, fill = mean)) +
        geom_tile() +
        scale_fill_gradient2(low = "red", mid = "white", high = "blue")
}

## rhoPsiHeatmap, but with sigma1 and sigma2
sigmaHeatmap <- function(plotCorrelation=FALSE, count=5, ...) {
    sigma1 <- seq(0.1, 0.8, length.out=count)
    sigma2 <- seq(0.1, 0.8, length.out=count)
    sigma12 <- expand.grid(sigma1=sigma1, sigma2=sigma2)

    means <- sapply(1:nrow(sigma12), function(n) {
        sigma1N <- sigma12$sigma1[n]
        sigma2N <- sigma12$sigma2[n]
        dist <- simulateDistribution(returnCorrelation=plotCorrelation, sigma1=sigma1N, sigma2=sigma2N, ...)
        return(mean(dist))
    })

    sigma12$mean <- means

    ggplot(sigma12, aes(x=sigma1, y=sigma2, fill = mean)) +
        geom_tile() +
        scale_fill_gradient2(low = "red", mid = "white", high = "blue")
}



## find the training fraction that maximizes profits
findOptimalTF <- function(stocksDF, plot = FALSE, returnProfit=FALSE) {
    tfValues <- seq(0.1, 0.9, length = 81)
    
    ## convert kvalues to profit values
    tfProfit <- sapply(tfValues, function(tf) {
        profit <- evaluatePairsTrading(stocksDF, trainningFrac=tf)
        return(profit)
    })

    optimalTF <- tfValues[which(tfProfit == max(tfProfit))][1]
    
    ## plot kvalues against their profit
    if (plot) {
        plot(x=tfValues, y=tfProfit, type="l", xlab="Training Fraction Values", ylab="Profit")
        ## line of optimal k
        abline(v=optimalTF, lty=2, col="red")
    }

    if (!returnProfit) {
        ## search the grid for the maximum profit
        return( optimalTF )
    }
    return( max(tfProfit) )
}

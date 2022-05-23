## Functions for Global Demographics

library(XML)
library(dplyr)
library(ggplot2)

## Infant / Population Data Frames
factbook <- xmlParse("data/factbook.xml")
root <- xmlRoot(factbook)

## get infant rank nodes
infant <- getNodeSet(root, "//field[@id='f2091']/rank")
## get country code
countryCode <- sapply(infant, function(node) xmlGetAttr(node, "country"))
## get infant mortality rate as numeric
mortality <- sapply(infant, function(node) as.numeric(xmlGetAttr(node, "number")) )

infantDF <- data.frame(countryCode, mortality)

## do the same thing, but this time with the popualtion census
pop <- getNodeSet(root, "//field[@id='f2119']/rank")
countryCode <- sapply(pop, function(node) xmlGetAttr(node, "country"))
population <- sapply(pop, function(node) as.numeric(xmlGetAttr(node, "number")) )

populationDF <- data.frame(countryCode, population)


## Cross-Referencing Data Frame
## data frame of nodes
cross <- getNodeSet(root, "//appendix[@letter='d']/table/row")
cells <- lapply(cross, xmlChildren)
country <- sapply(cells, function(cell) {
    xmlGetAttr(cell[[1]], "content")
})
cia <- sapply(cells, function(cell) {
    xmlGetAttr(cell[[1]], "country")
})
iso3166 <- sapply(cells, function(cell) {
    xmlGetAttr(cell[[3]], "content")
})

## data frame to cross-reference cia, iso3166 code
crossDF <- data.frame(country, cia, iso3166)


## Coordinates Data Frame
library(readr)
## read coordinates from csv
data <- read.csv("coordinates.csv")

## get correct columns & change the names
coordinates <- data[c("Alpha.2.code", "Latitude..average.", "Longitude..average.")]
colnames(coordinates) <- c("iso3166", "latitude", "longitude")
coordinates <- unique(coordinates)

## make sure both columns are numeric
coordinates$latitude <- as.numeric(coordinates$latitude)
coordinates$longitude <- as.numeric(coordinates$longitude)

## remove the first char of the iso3166 code (blank space)
coordinates$iso3166 <- sapply(coordinates$iso3166, function(x) {
    substr(x, 2, 3)
})


## Merging Data Frames
## get data frame with country data for iso3166 code, cia code,
## population, mortaltiy rate ,latitude, longitude
countries <- merge(crossDF, infantDF, by.x="cia", by.y="countryCode")
countries <- merge(countries, populationDF, by.x="cia", by.y="countryCode")

countries <- merge(countries, coordinates, by="iso3166")
countries <- select(countries, country, iso3166, cia, population, mortality, latitude, longitude)

## World Map
library(RColorBrewer)
## get 8 colors for each quantile
palette <- brewer.pal(8, "YlOrRd")


kmeans <- function(k, X) {
    ## # of rows & columns
    p <- ncol(X)
    n <- nrow(X)
    
    ## standardize n matrix
    for(j in 1:p) {
        ## get z-scores of each column X[,j]
        ## how many sds each score is from mean
        X[,j] <- sapply(X[,j], function(val) {
            (val - mean(X[,j])) / sd(X[,j])
        })
    }

    ## get inital centroids: random points
    centroids <- lapply(1:k, function(v) {
        row <- sample(1:n, 1)
        X[row,]
    })

    repeat{
        ## 1. recluster points
        ## for each point, compare to each centroid
        ## get the index of which grouping each point belongs to
        whichGroup <- sapply(1:n, function(i) {
            pointToCentroid <- sapply(centroids, function(c) {
                
                ## get distance of each feature
                featureDistance <- sapply(1:p, function(j) {
                    X[i, j] - c[j]
                })
                ## get euclidan distance to centroid
                sqrt( sum( featureDistance^2 ) )
            })

            ## add point to centorid group
            closestCentroid <- which(pointToCentroid == min(pointToCentroid))
            return(closestCentroid)
        })

        ## get new grouping based on index
        groups <- lapply(1:k, function(centroid) {
            X[which(whichGroup == centroid), ]
        })

        ## 2. recalculate centroids
        ## save old centroids for comparsion
        previousCentroids <- centroids 

        ## get new centroids
        ## for every group, get mean of every feature
        centroids <- lapply(groups, function(g) {
            means <- sapply(1:p, function(j) {
                mean(g[, j])                 
            })
        })

        ## compare to previous centroids
        ## if no change, then we're done
        if( identical(centroids, previousCentroids) ) {
            break
        }
    }
        
    ## finally return the current (stable) classication vector easy-peasy
    return(whichGroup)
}
    
regionalMap <- function(k=4) {
    world <- map_data("world")
    ## MORTALITY
    ## 9 different break points for 8 quantiles
    percent <- sapply(0:8, function(n) n/8)
    mortalityBreakPoints <- quantile(countries$mortality, probs=percent)
    ## make the 1st breakpoint inclusive
    mortalityBreakPoints[1] <- mortalityBreakPoints[1] - 0.01

    ## cut it using the quantiles
    mortalityEights <- cut(countries$mortality, breaks = mortalityBreakPoints)
    ## get range of each quantile
    mortalityLevels <- levels(mortalityEights)

    ## assign every mortality rate to a color
    mortalityColor <- sapply(mortalityEights, function(lvl) {
        palette[which(mortalityLevels == lvl)]
    })
    countries$mortalityColor <- mortalityColor
    mortalityLabels <- c("1.8-4.1", "4.1-6.2", "6.2-10.2", "10.2-14.2", "14.2-21.5",
                         "21.5-38.7", "38.7-55.9", "55.9-117.2")

    ## POPULATION
    percent <- sapply(0:8, function(n) n/8)

    populationBreakPoints <- quantile(countries$population, probs=percent)
    populationBreakPoints[1] <- populationBreakPoints[1] - 0.01

    populationEights <- cut(countries$population, breaks = populationBreakPoints)
    populationLevels <- levels(populationEights)

    populationLabels <- c("5-9", "91-593", "593-2,910", "2,910-5,960",
                          "5,960-10,400", "10,400-21,800", "21,800-46,800",
                          "46,800-1,360,000")

    ## MAP
    ## base map
    map <- ggplot() +
        geom_map(
            data = world, map = world,
            aes(long, lat, map_id = region),
            ## customize
            color = "black", fill = "lightgray", size = 0.2
        ) +
        geom_point(
            data = countries,
            ## color is mortality, size is population
            aes(longitude, latitude, color = factor(mortalityColor, labels = rev(mortalityLabels)), size=sqrt(population))
        ) +
        ## minimum size
        scale_size_continuous(range=c(1,8)) +
        labs(color = "Infant mortality\n(per 1000 births)", size = "Population size\n(in thousands)")

    ## K-MEANS
    ## matrix
    X <- matrix(c(countries$latitude, countries$longitude, countries$mortality), ncol=3)
    ## sort into k groups
    whichGroup <- kmeans(k, X)

    ## create chulls
    chulls <- lapply(1:k, function(x) {
        ## get all countries belonging to group
        g <- countries[ which(whichGroup == x), ]
        ## parts of map that are chull
        g %>% slice(chull(g$longitude, g$latitude))

    })
  
    colors <- rainbow(k) ## list of k colors
    colorIndex <- 1 ## starting color index
    ## for every chull, add on one polygon
    for(c in chulls) {
        map <- map + geom_polygon(data=c, aes(x=longitude, y=latitude), fill=colors[colorIndex], alpha=0.3)
        colorIndex <- colorIndex + 1
    }

    ## create map
    map
}

## EXTNESION
## 1. get regions
world <- map_data("world")
africa <- c("Algeria","Angola","Benin","Botswana","Burkina Faso","Burundi",
            "Cabo Verde","Cameroon","Central African Republic","Chad","Comoros",
            "Democratic Republic of the Congo","Republic of Congo","Ivory Coast",
            "Djibouti","Egypt","Equatorial Guinea","Eritrea","Swaziland","Ethiopia",
            "Gabon","Gambia","Ghana","Guinea","Guinea-Bissau","Kenya","Lesotho","Liberia",
            "Libya","Madagascar","Malawi","Mali","Mauritania","Mauritius","Morocco",
            "Mozambique","Namibia","Niger","Nigeria","Rwanda","Sao Tome and Principe",
            "Senegal","Seychelles","Sierra Leone","Somalia","South Africa","South Sudan",
            "Sudan","Tanzania","Togo","Tunisia","Uganda","Zambia","Zimbabwe")

asia <- c("Afghanistan", "Armenia", "Azerbaijan", "Bahrain", "Bangladesh", "Bhutan", 
          "Brunei", "Cambodia", "China", "Cyprus", "Georgia", "India", "Indonesia", 
          "Iran", "Iraq", "Israel", "Japan", "Jordan", "Kazakhstan", "Kuwait", 
          "Kyrgyzstan", "Laos", "Lebanon", "Malaysia", "Maldives", "Mongolia", "Myanmar", 
          "Nepal", "North Korea", "Oman", "Pakistan", "Palestine", "Philippines", "Qatar", 
          "Russia", "Saudi Arabia", "Singapore", "South Korea", "Sri Lanka", "Syria", 
          "Taiwan", "Tajikistan", "Thailand", "Timor-Leste", "Turkey", "Turkmenistan", 
          "United Arab Emirates", "Uzbekistan", "Vietnam", "Yemen")

europe <- c("Bulgaria","Cyprus","Estonia","Finland","Greece","Ireland","Latvia",
            "Lithuania","Luxembourg","Malta","Romania" ,"Sweden","Portugal", 
            "Spain", "France", "Switzerland", "Germany","Austria", "Belgium", 
            "UK", "Netherlands", "Denmark", "Poland", "Italy", "Croatia", 
            "Slovenia", "Hungary", "Slovakia", "Czech republic")

continentMap <- function(continent, k=3) {
    fourPalette <- brewer.pal(4, "YlOrRd")
    regionCountries <- subset(countries, country %in% continent)
    mapData <- subset(world, region %in% continent)
    
    ## MORTALITY
    ## 5 different break points for 4 quantiles
    percent <- sapply(0:4, function(n) n/4)
    mortalityBreakPoints <- quantile(regionCountries$mortality, probs=percent)
    ## make the 1st breakpoint inclusive
    mortalityBreakPoints[1] <- mortalityBreakPoints[1] - 0.01

    ## cut it using the quantiles
    mortalityQuads <- cut(regionCountries$mortality, breaks = mortalityBreakPoints)
    ## get range of each quantile
    mortalityLevels <- levels(mortalityQuads)

    ## assign every mortality rate to a color
    mortalityColor <- sapply(mortalityQuads, function(lvl) {
        fourPalette[which(mortalityLevels == lvl)]
    })
    regionCountries$mortalityColor <- mortalityColor

    map <- ggplot() +
        geom_map(
            data = mapData, map = mapData,
            aes(long, lat, map_id = region),
            ## customize
            color = "black", fill = "lightgray", size = 0.2
        ) +
        geom_point(
            data = regionCountries,
            ## color is mortality, size is population
            aes(longitude, latitude, color = mortalityColor, size=sqrt(mortality))
        ) +
        ## minimum size
        scale_size_continuous(range=c(1,8)) +
        theme(legend.position="none")

    ## K-MEANS
    ## matrix
    X <- matrix(c(regionCountries$latitude, regionCountries$longitude, regionCountries$mortality), ncol=3)
    ## sort into k groups
    whichGroup <- kmeans(k, X)

    ## create chulls
    chulls <- lapply(1:k, function(x) {
        ## get all regionCountries belonging to group
        g <- regionCountries[ which(whichGroup == x), ]
        ## parts of map that are chull
        g %>% slice(chull(g$longitude, g$latitude))

    })
  
    colors <- rainbow(k) ## list of k colors
    colorIndex <- 1 ## starting color index
    ## for every chull, add on one polygon
    for(c in chulls) {
        map <- map + geom_polygon(data=c, aes(x=longitude, y=latitude), fill=colors[colorIndex], alpha=0.3)
        colorIndex <- colorIndex + 1
    }

    ## create map
    map
}

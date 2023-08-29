## Deliverables for Global Demographics 

## 1.
## a. no iso3166
crossDF <- data.frame(country, cia, iso3166)
noIso3166 <- crossDF[which(crossDF$iso3166=="-"),]
nrow(noIso3166)

## b. no iso3166 or cia
noCodes <- crossDF[which(crossDF$iso3166=="-" & crossDF$cia=="-"),]
nrow(noCodes)


## 2. 
## a. infant mortality hist
hist(infantDF$mortality, main="Infant mortality", xlab="Infant mortality rate (per 1000 births)")

## b. top 10 infant mortality
## merge into with the crossDF to get the country 
mortality <- merge(crossDF, infantDF, by.x="cia", by.y="countryCode")
## get the countries with the highest mortality
highestMortality <- mortality %>% arrange(desc(mortality)) %>% head(n=10)
## select the country, mortality info (x deaths per 10000 births)
highestMortality[c("country", "mortality")]

## 5.
lt10 <- countries[which(countries$population < 10000000),]
mean(lt10$mortality)
gt50 <- countries[which(countries$population > 50000000),]
mean(gt50$mortality)

##6.
intervalCount <- sapply(mortalityLevels, function(lvl) {
    length( which(mortalityEights == lvl) )
})
intervalCount

## 7. mortality map
world <- map_data("world")
# 9 different break points for 8 quantiles
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
                     
ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    ## customize look
    color = "black", fill = "lightgray", size = 0.2
  ) +
    ## input values
    geom_point(
        data = countries,
        aes(longitude, latitude, color = factor(mortalityColor, labels = rev(mortalityLabels)), size=sqrt(mortality))
    ) +
    ## limit point size to range
    scale_size_continuous(range=c(1,6)) +
    guides(size = "none") +
    labs(color = "Infant mortality\n(per 1000 births)")

## 8. population map
percent <- sapply(0:8, function(n) n/8)

populationBreakPoints <- quantile(countries$population, probs=percent)
populationBreakPoints[1] <- populationBreakPoints[1] - 0.01

populationEights <- cut(countries$population, breaks = populationBreakPoints)
populationLevels <- levels(populationEights)
populationLevels

populationColor <- sapply(populationEights, function(lvl) {
    palette[which(populationLevels == lvl)]
})
countries$populationColor <- populationColor

populationLabels <- c("5-9", "91-593", "593-2,910", "2,910-5,960",
                      "5,960-10,400", "10,400-21,800", "21,800-46,800",
                      "46,800-1,360,000")

ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "black", fill = "lightgray", size = 0.2
  ) +
    geom_point(
        data = countries,
        aes(longitude, latitude, color = factor(populationColor, labels = rev(populationLabels)), size=sqrt(population))
    ) +    guides(size = "none") +
    scale_size_continuous(range=c(1,8)) +
    labs(color = "Population size\n(in thousands)")


## 9. k-means
X <- matrix(c(countries$latitude, countries$longitude, countries$mortality), ncol=3)
kmeans(4, X)

## 10. regional map
regionalMap(4)

## extension
continentMap(africa)
continentMap(asia)
continentMap(europe, k=2)
continentMap( c(europe, asia), k=4)

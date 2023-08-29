## Code for SQL Baseball 

library(RSQLite)
driver <- dbDriver("SQLite")
con <- dbConnect(driver, dbname = "lahman2016.sqlite")
dbListTables(con)

## 1. salaries df
salaries <- dbGetQuery(con, "SELECT * FROM SALARIES")
## how much salary info
dbGetQuery(con, "SELECT COUNT(*) FROM SALARIES")
## min, max year
dbGetQuery(con, "SELECT MIN(yearID), MAX(yearID) FROM SALARIES")

## 2. plot salaries vs year
plot( jitter(salaries$yearID, 8) , salaries$salary, xlab="Year", ylab="Salary", pch=".")

## 3. smooth scatter plot
smoothScatter( jitter(salaries$yearID, 8), salaries$salary, xlab="Year", ylab="Salary")
nn

## 4.
salReg <- lm( salary ~ yearID + lgID, data=salaries)
salReg

## 5.
## added 1 to salary, since log(0) is invalid
salLog <- lm( log(salary + 1) ~ lgID + yearID, data=salaries)

## 6.
summary(salReg)
summary(salLog)

## 7.
teamsalaries <- dbGetQuery(con, "
SELECT Teams.teamID as team, Teams.yearID as year, SUM(Salaries.salary) as salary
FROM Teams JOIN Salaries
ON year = Salaries.yearID AND team = Salaries.teamID
WHERE year = 2016
GROUP BY team
ORDER BY salary
")

barplot(teamsalaries$salary, names.arg = teamsalaries$team, xlab="Teams", ylab="Salary", main="Team salaries in 2016")
##min salary
teamsalaries[1,]

##max salary
teamsalaries[19,]

## 8.
salariesByYear <- dbGetQuery(con, "
SELECT Teams.teamID as team, Teams.yearID as year,
SUM(Salaries.salary) as salary, Teams.lgID as league
FROM Teams JOIN Salaries
ON year = Salaries.yearID AND team = Salaries.teamID
GROUP BY year, team
ORDER BY league
")

nrow(salariesByYear)

## 9.
## max salary
max(salariesByYear$salary)
million <- 1000000
maxSalary <- max(salariesByYear$salary) / million
boxplot(salariesByYear$salary / million ~ salariesByYear$year, xlab="Year", ylab="Total salary of team (in millions of dollars)", subset = salariesByYear$league ==  "AL", main = "American League Salaries", ylim = c(0, maxSalary))

boxplot(salariesByYear$salary / million ~ salariesByYear$year, xlab="Year", ylab="Total salary of team (in millions of dollars)", subset = salariesByYear$league ==  "NL", main = "National League Salaries", ylim = c(0, maxSalary))

dbGetQuery(con, "
SELECT *
FROM Salaries
WHERE teamID = 'KCA' AND yearID = 1985
LIMIT 100
")

## 10.
worldSeries <- dbGetQuery(con, "
SELECT SeriesPost.yearID as year, SeriesPost.teamIDwinner as teamID,
SeriesPost.lgIDwinner as league, SUM(Salaries.salary) as total_salary
FROM SeriesPost JOIN Salaries
ON SeriesPost.yearID = Salaries.yearID AND SeriesPost.teamIDwinner = Salaries.teamID AND SeriesPost.round = 'WS'
GROUP BY year
")

alws <- worldSeries[ worldSeries$league == "AL", ] 
nlws <- worldSeries[ worldSeries$league == "NL", ]
nrow(alws)
nrow(nlws)

## avg salary
mean(alws$total_salary)
mean(nlws$total_salary)

## 11.
plot(x = jitter(salariesByYear$year), y = salariesByYear$salary / million, pch = 4, col = 'grey', ylab = "Total salaries of teams", xlab = "Year", main = "Salaries of teams (incl. salary of world series winner)")
points(x = alws$year, y = alws$total_salary / million, pch=16, col='blue')
points(x= nlws$year, y = nlws$total_salary / million, pch=16, col='red')

## 12.
maxByYear <- dbGetQuery(con, "
SELECT year, MAX(salary) as max_salary, league
FROM (
SELECT Teams.teamID as team, Teams.yearID as year,
SUM(Salaries.salary) as salary, Teams.lgID as league
FROM Teams JOIN Salaries
ON year = Salaries.yearID AND team = Salaries.teamID
GROUP BY year, team
ORDER BY league
) 
GROUP BY year
")

almax <- maxByYear[ worldSeries$league == "AL", ]
nlmax <- maxByYear[ worldSeries$league == "NL", ]

plot( x = almax$year, y = almax$max_salary / million, col = "blue", pch = 20, xlab = "Year", ylab = "Maximum team salary", main = "Maximum team salary of year" )
points( x = nlmax$year, y = nlmax$max_salary / million, col = "red", pch = 20 )

## 13.
allstar <- dbGetQuery(con, "
SELECT AllstarFull.yearID AS year, COUNT(*) AS allstars, teamID
FROM AllstarFull JOIN SeriesPost
ON SeriesPost.yearID = AllstarFull.yearID AND SeriesPost.teamIDwinner = AllstarFull.teamID AND SeriesPost.round = 'WS'
GROUP BY year
")
tail(allstar[order(allstar$allstars), ], 5)

## 14.
homeruns <- dbGetQuery(con, "
SELECT yearID AS year, SUM(HR) AS runs
FROM Batting
GROUP BY year
")

plot(x = homeruns$year, y = homeruns$runs, xlab = "Year", ylab = "# of home runs")
linear <- lm(homeruns$runs ~ homeruns$year)
abline(linear)
summary(linear)

players <- dbGetQuery(con, "
SELECT Batting.playerID, Batting.yearID, Batting.R as homeruns
FROM BATTING JOIN
(SELECT playerID, COUNT(*) AS years
FROM Batting
GROUP BY playerID
HAVING years > 10
) players
ON Batting.playerID = players.playerID
ORDER BY Batting.playerID
")

perPlayer <- split(players, players$playerID)
reg <- sapply(perPlayer, function(player) {
    linear <- lm( homeruns ~ yearID, data = player)
    ## extract slope of line
    slope <- summary(linear)$coefficients[[2]]
    return( slope )
})

## summary of data from improvement in homeruns
summary(reg)
hist(reg, breaks=40, main="Improvement of homeruns", xlab="# of homeruns per year")

## 15.
## For the average pitcher, is there a correlation between the total number of shutouts a player has achieved and how much they're paid at the peak of their career? What about for the non-average player?

## for each player, # of shutouts & max pay
shutouts <- dbGetQuery(con, "
SELECT SUM( Pitching.SHO ) AS shutouts, Pitching.playerID, MAX( Salaries.salary ) AS average_salary
FROM Pitching JOIN Salaries
WHERE Pitching.playerID = Salaries.playerID AND Pitching.yearID = Salaries.yearID
GROUP BY Pitching.playerID
ORDER BY shutouts
")

## how many players have shutouts
hist( shutouts$shutouts, breaks=40, main="Shutouts", xlab="# of shutouts in career", ylab="# of players" )
summary( shutouts$shutouts )

## see how much % of the population is < 10
table(cut(shutouts$shutouts, breaks=c(-Inf, 11, Inf)))/ length(shutouts$shutouts)

## for each # of shutouts, average max pay
totalShutouts <- dbGetQuery(con, "
SELECT shutouts, AVG( average_salary ) AS average_salary
FROM (
SELECT SUM( Pitching.SHO ) AS shutouts, Pitching.playerID, MAX( Salaries.salary ) AS average_salary
FROM Pitching JOIN Salaries
WHERE Pitching.playerID = Salaries.playerID AND Pitching.yearID = Salaries.yearID
GROUP BY Pitching.playerID
ORDER BY shutouts
)
GROUP BY shutouts
")

## barplot for shutouts <10
barplot( totalShutouts[0:11,]$average_salary / million, beside=TRUE, names.arg=totalShutouts[0:11,]$shutouts, xlab="# of shutouts", ylab = "Average of max salaries" )

## plot for all shutouts
plot( totalShutouts$average_salary / million  ~ totalShutouts$shutouts, xlab="# of shutouts", ylab="Average of max salaries")
linear <- lm( average_salary / million  ~ shutouts, totalShutouts )
abline(linear)
summary(linear)

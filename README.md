# rwindow.baseball

The goal of rwindow.baseball is to pull data from baseball-reference so that we can examine the relative length of competitive window for each team in MLB over time. 

## Installation

You can install rwindow.baseball from github with:


``` r
# install.packages("devtools")
devtools::install_github("williazo/rwindow.baseball")
```

## Team Abbreviations
Below is a table that lists the appropriate team abbreviations to use

|Team |Full Name             |Division                 |
|:----|:---------------------|:------------------------|
|BOS  |Boston Red Sox        |American League: East    |
|NYY  |New York Yankees      |American League: East    |
|TOR  |Toronto Blue Jays     |American League: East    |
|BAL  |Baltimore Orioles     |American League: East    |
|TBR  |Tampa Bay Rays        |American League: East    |
|HOU  |Houston Astros        |American League: West    |
|LAA  |Los Angeles Angels    |American League: West    |
|SEA  |Seattle Mariners      |American League: West    |
|TEX  |Texas Rangers         |American League: West    |
|OAK  |Oakland Athletics     |American League: West    |
|CLE  |Cleveland Indians     |American League: Central |
|MIN  |Minnesota Twins       |American League: Central |
|KCR  |Kansas City Royals    |American League: Central |
|CHW  |Chicago White Sox     |American League: Central |
|DET  |Detroit Tigers        |American League: Central |
|WSN  |Washington Nationals  |National League: East    |
|MIA  |Miami Marlins         |National League: East    |
|ATL  |Atlanta Braves        |National League: East    |
|NYM  |New York Mets         |National League: East    |
|PHI  |Philadelphia Phillies |National League: East    |
|SFG  |San Francisco Giants  |National League: West    |
|SDP  |San Diego Padres      |National League: West    |
|COL  |Colorado Rockies      |National League: West    |
|ARI  |Arizona Diamondbacks  |National League: West    |
|LAD  |Los Angeles Dodgers   |National League: West    |
|CHC  |Chicago Cubs          |National League: Central |
|MIL  |Milwaukee Brewers     |National League: Central |
|STL  |St Louis Cardinals    |National League: Central |
|PIT  |Pittsburgh Pirates    |National League: Central |
|CIN  |Cincinnati Reds       |National League: Central |


## Example


The first example shows how to pull basic contract information from baseball-reference. We will pull the current contract status of the Boston Red Sox as an example.

The first data.frame in the list provides player specific contract information as of 2017, while the second data.frame provides a general team summary information.

``` r
library(rwindow.baseball)
bos_sal <- tm_contract("BOS")
player_salary <- bos_sal[[1]]
head(player_salary)

bos_summary <- bos_sal[[2]]
bos_summary
```

We can also pull the game by game results for every team starting from the year 2000. This time we will try pulling the game results for the Colorado Rockies from 2007, the year the team advanced to play the Boston Red Sox in the World Series.

```r
col_game <- tm_standings_schedule("COL", 2009)
#find the maximum winning streak
max(lengths(regmatches(col_game$Streak, gregexpr("[+]", col_game$Streak))))
#longest losing streak
max(lengths(regmatches(col_game$Streak, gregexpr("[-]", col_game$Streak))))
```

Also, you can pull a range of years for a specific team using the `start_year` and `end_year` options in `tm_standings_schedule()`. Below I pull game results for the Detroit Tigers from the 2000-2010 seasons.
```r
tiger_games <- tm_standings_schedule("DET", start_year = 2000, end_year = 2010)
tiger_games <- tiger_games[order(tiger_games$Gm.), ]
#which years did the Tigers win the opening game
opening_day <-  tiger_games[tiger_games$Gm.==1, ]
tiger_games[which(opening_day$W.L=="W"), "Year"]
```

# rwindow.baseball

The goal of rwindow.baseball is to pull data from baseball-reference so that we can examine the relative length of competitive window for each team in MLB over time. 

## Installation

You can install rwindow.baseball from github with:


``` r
# install.packages("devtools")
devtools::install_github("williazo/rwindow.baseball")
```

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

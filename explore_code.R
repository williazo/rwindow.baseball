# How long is our window?
library(devtools)
install_github("williazo/rwindow.baseball")
library(rwindow.baseball)

rockies_game_dat <- tm_standings_schedule("COL", start_year = 2007, end_year = 2008)

# How long is our window?
library(devtools)
install_github("williazo/rwindow.baseball")
library(rwindow.baseball)

#list of all teams
al_east <- c("BOS", "NYY", "TOR", "BAL", "TBD")
al_west <- c("HOU", "LAA", "SEA", "TEX", "OAK")
al_cent <- c("CLE", "MIN", "KC", "CHW", "DET")

nl_east <- c("WSN", "MIA", "ATL", "NYM", "PHI")
nl_west <- c("SF", "SD", "COL", "ARI", "LAD")
nl_cent <- c("CHC", "MIL", "STL", "PIT", "CIN")

mlb <- c(al_east, al_west, al_cent, nl_east, nl_west, nl_cent)
proper=function(s) gsub("(?<=\\b)([a-z])", "\\U\\1", tolower(s), perl=TRUE)
full_tbl <- NULL
for(i in mlb){
  tm_info  <- team_specific_fill(i)
  Division <- paste(paste0(tm_info[[1]],":"), tm_info[[2]])
  full_name <- proper(gsub("-", " ",tm_info[[3]]))
  row_info <- cbind(i, full_name, Division)
  full_tbl <- rbind(full_tbl, row_info)
}
full_tbl <- as.data.frame(full_tbl)

rockies_game_dat <- tm_standings_schedule("COL", start_year = 2007, end_year = 2008)

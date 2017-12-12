# How long is our window?
library(devtools)
install_github("williazo/rwindow.baseball")
library(rwindow.baseball)

#list of all teams
al_east <- c("BOS", "NYY", "TOR", "BAL", "TBR")
al_west <- c("HOU", "LAA", "SEA", "TEX", "OAK")
al_cent <- c("CLE", "MIN", "KC", "CHW", "DET")

nl_east <- c("WSN", "MIA", "ATL", "NYM", "PHI")
nl_west <- c("SFG", "SDP", "COL", "ARI", "LAD")
nl_cent <- c("CHC", "MIL", "STL", "PIT", "CIN")

mlb <- c(al_east, al_west, al_cent, nl_east, nl_west, nl_cent)
proper=function(s) gsub("(?<=\\b)([a-z])", "\\U\\1", tolower(s), perl=TRUE)
full_tbl <- NULL
full_result <- NULL
team_ref_tbl <- NULL
for(i in mlb){
  tm_info  <- team_specific_fill(i)
  Division_w_league <- paste(paste0(tm_info[[1]],":"), tm_info[[2]])
  division <- tm_info[[2]]
  league <- tm_info[[1]]
  full_name <- proper(gsub("-", " ",tm_info[[3]]))
  row_info <- cbind(i, full_name, Division_w_league)
  full_tbl <- rbind(full_tbl, row_info)

  team_ref_row <- cbind(i, full_name, league, division)
  team_ref_tbl <- rbind(team_ref_tbl, team_ref_row)

  #pulling data from 2000 to 2017 on the game results of each team
  result <- tm_standings_schedule(i, start_year = 2000, end_year = 2017)
  full_result <- rbind(full_result, result)
}
full_tbl <- as.data.frame(full_tbl)
names(full_tbl) <- c("Team", "Full Name", "Division")
knitr::kable(full_tbl)

team_ref_tbl <- as.data.frame(team_ref_tbl)
names(team_ref_tbl) <- c("ABRV", "Full Name", "League", "Division")

num_vars <- c("Gm.", "R", "RA", "Rank")
full_result[num_vars] <- apply(full_result[num_vars], 2, as.numeric)

full_result$attnd <- as.numeric(gsub(",", "", full_result$Attendance))
home_games <- subset(full_result, home_gm == 1)
home_games$home_counter <- ave(home_games$Gm., list(home_games$Year, home_games$Tm), FUN = seq_along)
home_games <- dplyr::left_join(home_games, team_ref_tbl, by = c("Tm"="ABRV"))
home_games <- subset(home_games, is.na(League)==F)

cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
install_github("williazo/ggplot.spaghetti")
library(ggplot.spaghetti)
for(i in unique(home_games$League)){
  for(j in unique(home_games$Division)){
    bball_plot <- ggplot_spaghetti(y = subset(home_games, League == i & Division == j)$attnd,
                     id = subset(home_games, League == i & Division == j)$Tm,
                     time = subset(home_games, League == i & Division == j)$home_counter,
                     wrap = subset(home_games, League == i & Division == j)$Year,
                     group = subset(home_games, League == i & Division == j)$Tm)+
      scale_color_manual(name = "Team", values = cbPalette)+
      ggtitle(paste0("League: ", i, " Division: ", j))+
      guides(linetype = F)
  ggsave(bball_plot, file = paste0("~/Desktop/", i, "_", j, ".jpeg"), dpi = 600, units = "in",
                                   height = 8, width = 10)
  }
}


rockies_game_dat <- tm_standings_schedule("COL", start_year = 2007, end_year = 2008)


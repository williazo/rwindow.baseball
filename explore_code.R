# How long is our window?
library(devtools)
install_github("williazo/rwindow.baseball")
library(rwindow.baseball)
#loading in the MLB color palette
data("MLB_colors")

#list of all teams
al_east <- c("BOS", "NYY", "TOR", "BAL", "TBR")
al_west <- c("HOU", "LAA", "SEA", "TEX", "OAK")
al_cent <- c("CLE", "MIN", "KCR", "CHW", "DET")

nl_east <- c("WSN", "MIA", "ATL", "NYM", "PHI")
nl_west <- c("SFG", "SDP", "COL", "ARI", "LAD")
nl_cent <- c("CHC", "MIL", "STL", "PIT", "CIN")

mlb <- c(al_east, al_west, al_cent, nl_east, nl_west, nl_cent)
proper = function(s) gsub("(?<=\\b)([a-z])", "\\U\\1", tolower(s), perl=TRUE)
full_tbl <- NULL
full_result <- NULL
team_ref_tbl <- NULL
#pulling all of the game results for mlb teams from in 2000-2018
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

  #pulling data from 2000 to 2018 on the game results of each team
  result <- tm_standings_schedule(i, start_year = 2000, end_year = 2018)
  #if the last year is the current season then the remaining games have NA's and so we want to drop all the rows with missing run values
  result <- subset(result, is.na(R) == FALSE)
  full_result <- rbind(full_result, result)
}

#code to create the README reference table
full_tbl <- as.data.frame(full_tbl, stringsAsFactors = F)
names(full_tbl) <- c("Team", "Full Name", "Division")
#knitr::kable(full_tbl)

#alternative reference table with league specified as its own variable
team_ref_tbl <- as.data.frame(team_ref_tbl, stringsAsFactors = F)
names(team_ref_tbl) <- c("ABRV", "Full Name", "League", "Division")

#cleaning up some of the game results data
num_vars <- c("Gm.", "R", "RA", "Rank")
full_result[num_vars] <- apply(full_result[num_vars], 2, as.numeric)

#comparing home attendance by division and league from 2000-2017
full_result$attnd <- as.numeric(gsub(",", "", full_result$Attendance))
home_games <- subset(full_result, home_gm == 1)
home_games$home_counter <- ave(home_games$Gm., list(home_games$Year, home_games$Tm), FUN = seq_along)
home_games <- dplyr::left_join(home_games, team_ref_tbl, by = c("Tm"="ABRV"))
home_games <- subset(home_games, is.na(League)==F)

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
  #ggsave(bball_plot, file = paste0("~/Desktop/", i, "_", j, ".jpeg"), dpi = 600, units = "in",
  #                                 height = 8, width = 10)
  }
}

#extracting league abbreviations
lg_abrv <- unlist(lapply(regmatches(unique(team_ref_tbl$League),
                  gregexpr("[[:upper:]]", unique(team_ref_tbl$League))), function(n) paste0(n, collapse = "")))
#pulling player batting value data
value_mlb <- NULL
for(i in lg_abrv){
  value_pull <- lg_tm_value(i, start_year = 2000, end_year = 2017)
  value_pull <- data.frame(League = rep(i, nrow(value_pull)), value_pull)
  value_mlb <- rbind(value_mlb, value_pull)
}
#converting salary to numeric value
value_mlb$salary <- as.numeric(gsub("[[:punct:]]", "", value_mlb$Salary))
value_mlb <- value_mlb[order(value_mlb$Year, value_mlb$Tm), ]
#excluding teams that changed names
current_value <- subset(value_mlb, Tm%in%mlb)
current_value$Tm <- factor(current_value$Tm, levels = MLB_colors$team[order(MLB_colors$team)])
current_value <- dplyr::left_join(current_value, team_ref_tbl[, -3], by = c("Tm" = "ABRV"))

#trends in salary for each team by league
with(current_value, ggplot_spaghetti(y = salary, id = Tm, time = Year, group = Tm, wrap = League))+
  scale_color_manual(name = "Team", values = MLB_colors$color[order(MLB_colors$team)])+
  scale_y_continuous(labels = scales::dollar_format())+
  guides(linetype = F)+
  scale_x_continuous(breaks = seq(2000, 2017, 5))+
  xlab("Year")+
  ylab("Team Salary")+
  theme(legend.position = "bottom", legend.key.width = unit(5, "cm"))

#plotting trends by division for each respective league
with(current_value, ggplot_spaghetti(y = salary, id = Tm, time = Year, group = Division, wrap = League))+
  scale_color_manual(name = "Team", values = cbPalette)+
  scale_y_continuous(labels = scales::dollar_format())+
  guides(linetype = F)+
  scale_x_continuous(breaks = seq(2000, 2017, 5))+
  xlab("Year")+
  ylab("Team Salary")+
  theme(legend.position = "bottom", legend.key.width = unit(5, "cm"))

#plot relative changes in team salary
with(current_value, ggplot_spaghetti(y = salary, id = Tm, time = Year, group = Tm, wrap = Tm,
                                     scales = "free"))+
  scale_color_manual(name = "Team", values = MLB_colors$color[order(MLB_colors$team)])+
  scale_y_continuous(labels = scales::dollar_format())+
  guides(linetype = F, color = F)+
  scale_x_continuous(breaks = seq(2000, 2017, 5))+
  xlab("Year")+
  ylab("Team Salary")+
  theme(legend.position = "bottom", legend.key.width = unit(5, "cm"))



#pulling data from Fangraphs on all Red Sox players who played multiple seasons and graphing their WAR trajectory
bos_batting <- tm_plyr_batting("BOS", start_year = 1991, end_year= 2017, min_pa = "y")
bos_batting$Season <- as.numeric(bos_batting$Season)
seasons_played <- reshape2::melt(table(bos_batting$Name))
mlt_seasons <- subset(seasons_played, value > 1)
with(subset(bos_batting, Name%in%mlt_seasons$Var1), ggplot_spaghetti(y = WAR, id = Name,
                                                                     time = Season, group = Name,
                                                                     wrap = Name, scales = "free"))+
  scale_x_continuous(breaks = seq(1991, 2017, 1))+
  guides(linetype = F, color = F)

### practice pulling prospect data
col_prospects <- prospect_cube(team = "COL", src = "BA")
library(dplyr)
top_prospects <- col_prospects %>%
  group_by(Year)%>%
  summarise(num_top_100 = sum(MLB!=""), avg_age = mean(as.numeric(Age)))
top_prospects <- top_prospects[order(top_prospects$num_top_100, decreasing = TRUE), ]
top_prospects
plot(x = top_prospects$num_top_100, y = top_prospects$avg_age, xlab = "Number of Top 100 Prospects",
                                   ylab = "Average Age of Top 10 Prospects (yrs.)", main = "Colorado Rockies",
     col = MLB_colors[MLB_colors$team=="COL","color"], pch = 19)
abline(lm(avg_age~num_top_100, data = top_prospects), col = MLB_colors[MLB_colors$team=="COL","color"], lty = 2)
lines(loess.smooth(x = top_prospects$num_top_100, y = top_prospects$avg_age), lty = 3)

plot(x = top_prospects$Year, y = top_prospects$num_top_100)


#'Scraping Fangraphs batter statistics
#'
#'Allows the user to scrape batting statistics by team and season from fangraphs.com
#'
#' @param team Specify the team abbreviation
#' @param year Specify numeric year from which to pull
#' @param min_pa Minimum number of plate appearances for batters. Default is 0 and takes on values up to 1000. To see only qualified hitters use `y`.
#' @param start_year Numeric value indicating the first year from which you want to pull data
#' @param end_year Numeric value indicating the last year to pull for a year range.
#'
#'
tm_plyr_batting <- function(team, year, min_pa = 0, start_year = NULL, end_year = NULL){
  team_info <- team_specific_fill(team)
  data("MLB_colors")
  lg_abrv <- unlist(lapply(regmatches(team_info[[1]],
                                      gregexpr("[[:upper:]]", team_info[[1]])), function(n) paste0(n, collapse = "")))
  lg <- tolower(lg_abrv)
  #Pulling data from fangraphs
  base_url <- "https://www.fangraphs.com/leaders.aspx?pos=all&stats=bat&"
  url <- paste0(base_url,"lg=", lg,"&type=8&season=", start_year, "&month=0&season1=", end_year, "&ind=0&team=", tm_numeric, "&rost=&age=&filter=&players=")
}

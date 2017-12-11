#'Pull the team standings and game by game schedule with results
#'
#'By default the system uses the current year
#'
#' @param team Team abbreviation
#' @param year Numeric year
#'
#' @import xml2
#' @import rvest
#'
#'
#' @export

tm_standings_schedule <- function(team, year = as.numeric(format(Sys.Date(), "%Y"))){
  #using this as a check to make sure that the team abbrev was specified correctly
  if(is.numeric(year) == F){
    stop("year must be specified as a numeric value", call. = F)
  } else if (year> as.numeric(format(Sys.Date(), "%Y"))){
    stop("year is misspecified. Cannot be greater than the current year")
  } else if(year < 2000){
    stop("year is too far back. Only pulling from the year 2000.")
  }

  team_info <- team_specific_fill(team)
  base_url <- "https://www.baseball-reference.com/teams/"

  url <- paste0(base_url, team,"/", year, "-schedule-scores.shtml")
  html_page <- xml2::read_html(url)
  tables <- rvest::html_nodes(html_page, "table")
  game_table <- rvest::html_table(tables)


}

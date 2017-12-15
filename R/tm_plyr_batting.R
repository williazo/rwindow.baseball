#'Scraping Fangraphs batter statistics
#'
#'Allows the user to scrape batting statistics by team and season from fangraphs.com
#'
#' @param team Specify the team abbreviation
#' @param year Specify numeric year from which to pull. Default is to take the current year if the season has started. Or the most recent year if the season is over.
#' @param min_pa Minimum number of plate appearances for batters. Default is 0 and takes on values up to 1000. To see only qualified hitters use `y`.
#' @param start_year Numeric value indicating the first year from which you want to pull data
#' @param end_year Numeric value indicating the last year to pull for a year range.
#'
#'
tm_plyr_batting <- function(team, year , min_pa = 0, start_year = NULL, end_year = NULL){
  #will need to update this each season since opening day changes every year.
  opening_day <- as.Date("03-29-2018", format = "%m-%d-%Y")
    if(missing(year)){
      if(((opening_day - Sys.Date()) >= 0) == T){
        year = as.numeric(format(opening_day, "%Y")) - 1
      } else{
        year = as.numeric(format(Sys.Date(), "%Y"))
      }
    }

  team_info <- team_specific_fill(team)
  fg_numeric <- MLB_colors[MLB_colors$team==team, "fg_id"]

  lg_abrv <- unlist(lapply(regmatches(team_info[[1]],
                                      gregexpr("[[:upper:]]", team_info[[1]])), function(n) paste0(n, collapse = "")))
  lg <- tolower(lg_abrv)

  if(is.null(start_year) == T & is.null(start_year) == T){
    start_year = year; end_year = year
  }

  #Pulling data from fangraphs
  base_url <- "https://www.fangraphs.com/leaders.aspx?pos=all&stats=bat&"
  url <- paste0(base_url,"lg=", lg, "&qual=", min_pa,"&type=8&season=", end_year, "&month=0&season1=", start_year, "&ind=1&team=", fg_numeric,
                "&rost=0&age=0&filter=&players=0&page=1_10000000")
  html_page <- xml2::read_html(url)
  url_nodes <- rvest::html_nodes(html_page, "table")
  team_tbl <- rvest::html_table(url_nodes[[12]], fill = T, header = NA)

  #extracting and cleaning the column names
  col_names <- team_tbl[2, ]
  col_names <- gsub("%", "_pct", col_names)
  col_names <- gsub("#", "n", col_names)
  col_names <- gsub("\\+", "_plus", col_names)

  #removing the extra top messy part
  team_tbl <- team_tbl[4:nrow(team_tbl),]
  names(team_tbl) <- col_names

  return(team_tbl)
}

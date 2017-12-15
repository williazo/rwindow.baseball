#' Scraping Team Specific Fangraphs batter statistics
#'
#' Allows the user to scrape leaderboard batting statistics by team and season from \url{http://www.fangraphs.com}.
#'
#' @param team Specify the team abbreviation as character value
#' @param year Specify numeric year from which to pull. Default is to take the current year if the season has started. Or the most recent year if the season is over.
#' @param min_pa Minimum number of plate appearances for batters. Default is 0 and takes on values up to 1000. To see only qualified hitters use `y`.
#' @param start_year Numeric value indicating the first year from which you want to pull data
#' @param end_year Numeric value indicating the last year to pull for a year range.
#'
#' @import xml2
#' @import rvest
#'
#' @return Returns table with data.frame object on the team hitters with the main statistics referenced on the fangraphs leaderboard.
#'
#' @examples #reading in batters with at least 200 PA for the Boston Red Sox from 2003 to 2010
#' bos_batting <- tm_plyr_batting("BOS", start_year = 2003, end_year= 2010, min_pa = 200)
#' numeric_vars <- c("AVG", "wOBA", "Season")
#' bos_batting[, numeric_vars] <- apply(bos_batting[, numeric_vars, 2, as.numeric)
#' head(bos_batting)
#'
#' @seealso \code{\link{team_specific_fill}} for details on appropriate team values
#'
#' @export
tm_plyr_batting <- function(team, year, min_pa = 0, start_year = NULL, end_year = NULL){

  #checking to make sure all three parameters are not specified
  if(missing(year) == FALSE & is.null(start_year) == FALSE & is.null(end_year) == FALSE){
    warning("Should not specify year with both start_year and end_year. Only pulling the range specified by start_year and end_year", call. = FALSE)
    year <- NULL
    }

  #will need to update this each season since opening day changes every year.
  opening_day <- as.Date("03-29-2018", format = "%m-%d-%Y")
  #setting the default year based on when the data is being querried.
  if(missing(year)){
      if(((opening_day - Sys.Date()) >= 0) == T){
        year = as.numeric(format(opening_day, "%Y")) - 1
      } else{
        year = as.numeric(format(Sys.Date(), "%Y"))
      }
    }

  #creating a check for the min_pa variable
  if(is.numeric(min_pa) == FALSE){
    if((min_pa == "y") == FALSE){
      stop("Must be numeric value for min_pa, or 'y' indicating to limit the search to only qualified hitters", call. = FALSE)
    }
  }

  #using the team_specific_fill function as a way to check that this variable is properly specified.
  team_info <- team_specific_fill(team)
  fg_numeric <- rwindow.baseball::MLB_colors[rwindow.baseball::MLB_colors$team==team, "fg_id"]

  lg_abrv <- unlist(lapply(regmatches(team_info[[1]],
                                      gregexpr("[[:upper:]]", team_info[[1]])), function(n) paste0(n, collapse = "")))
  lg <- tolower(lg_abrv)

 #adding in the check to see if start_year oe end_year is specified
  #neither start_year or end_year is specified
  if(is.null(start_year) == T & is.null(end_year) == T){
    start_year = year; end_year = year
  } #only one of the values is specified which should give an error
  else if((is.null(start_year) == F & is.null(end_year) == T) | (is.null(start_year) == T & is.null(end_year) == F)){
    stop("to specify a range start_year and end_year must both be entered", call. = F)
  } #both values are properly specified
  else if(is.null(start_year) == F & is.null(end_year) == F){
    if(is.numeric(start_year) == F | is.numeric(end_year) == F){
      stop("Both start_year and end_year must be numeric", call. = F)
    }
    else if(is.numeric(start_year) == T & is.numeric(end_year) == T){
      if(start_year>end_year){
        stop("Improperly specified range. start_year must be less than or equal to end_year", call. = F)
        }
    }
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

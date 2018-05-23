#'Pull FanGraphs Prospect Rankings from THE BOARD
#'
#'Scrape data from \url{https://www.fangraphs.com} to look at the top prospects based on ratings from FanGraphs prospect analysts
#'Can use this information to approximate the relative strength of each teams farm system.
#'
#' @param team Specify the team abbreviation as character value. Use this value to pull the top prospects by team
#' @param pos Character value specifying a specific group of positions. By default position is set to all. See \url{https://www.fangraphs.com/scoutboard.aspx?draft=2018prospect&type=0&pos=all&team=all} for a list of all positions available.
#' @param year Numeric year. Currently fangraphs has data only from 2017 and 2018.
#' @param start_year Numeric value that identifies the beginning year to pull a range of data for the team of interest. This is an optional parameter.
#' @param end_year Numeric value that identifies the ending year to pull a range of data for the team of interest. This is an optional parameter.
#'
#' @import xml2
#' @import rvest
#'
#' @examples
#'
#' @export

prospect_fg <- function(team, year, start_year = NULL, end_year = NULL, pos){
  if(missing(pos)){
    pos <- "all"
  }
  base_url <- "https://www.fangraphs.com/scoutboard.aspx?"
  url <- paste0(base_url, "draft=2018prospect&type=0&pos=all&team=all")
}

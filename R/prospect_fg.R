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
#' @param scout_grades Indicator to pull the scout grades for each prospect. Based on the 20-80 scale. Default is not to pull the grades and only pull the value. Options are `TRUE` or `FALSE`.
#'
#' @import xml2
#' @import rvest
#'
#' @examples #pulling prospect rankings from the 2017 Colorado Rockies for all hitters
#' prospect_fg(year = 2017, team = "COL", pos = "BAT")
#'
#' #pulling tool grades for 2017-2018 Los Angeles Dodgers
#' prospect_fg(start_year = 2017, end_year = 2018, team = "LAD", scout_grades = TRUE)
#'
#' @export

prospect_fg <- function(team, year, start_year = NULL, end_year = NULL, pos, scout_grades = FALSE){
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

  #code for checking that start_year and end_year are properly specified
  if(is.null(start_year) == T & is.null(end_year) == T){
    start_year = year; end_year = year
    #if not specified then use year value for both values
  } else if((is.null(start_year) == F & is.null(end_year) == T) | (is.null(start_year) == T & is.null(end_year) == F)){
    stop("to specify a range start_year and end_year must both be entered", call. = F)
    #only specified one of the two values
  } else if(is.null(start_year) == F & is.null(end_year) == F){
    #both are specified
    if(is.numeric(start_year) == F | is.numeric(end_year) == F){
      stop("Both start_year and end_year must be numeric", call. = F)
      #stopping if they are both not numeric values
    } else if(is.numeric(start_year) == T & is.numeric(end_year) == T){
      #checking again to make sure they are both numeric
      if(start_year > end_year){
        stop("Improperly specified range. start_year must be less than or equal to end_year", call. = F)
        #making sure that the start year is always lower than the end year
      }else if(start_year < 2017 | end_year > 2018){
        stop("Earliest available year is 2017 and latest is 2018", call. = FALSE)
      }
    }
  }

  #using the team_specific_fill function as a way to check that this variable is properly specified.
  if(missing(team)==FALSE){
    team_info <- team_specific_fill(team)
    fg_tm <- tolower(team)
  }else if(missing(team) == TRUE){
    fg_tm <- "all"
  }

  #if position is not specified pull all positions
  if(missing(pos)){
    pos <- "all"}else if(missing(pos)==FALSE){
      #converting this to lowercase in case the user specifies it using upper case
      pos <- tolower(pos)
      pos_check <- pos%in%c("bat", "c", "1b", "2b", "ss", "3b", "rf", "cf", "lf", "of", "pit", "rhp", "lhp")
      #adding in a stop if the specified position is not in the allowable set
      if(pos_check == FALSE){
        stop("Position misspecified. See help document for link to acceptable position choices.", call. = F)
      }
    }
  #scouting grades
  if(scout_grades == FALSE){
    #type = 0 pulls the value table from THE BOARD
    type = 0}else{
      #type = 1 pulls the scounting grades from THE BOARD
    type = 1}

  base_url <- "https://www.fangraphs.com/scoutboard.aspx?"
  #applying the function over multiple years
  prop_list <- lapply(start_year:end_year, function(yr){
    url <- paste0(base_url, "draft=", yr, "prospect&type=", type, "&pos=", pos, "&team=", fg_tm)
    html_page <- xml2::read_html(url)
    url_nodes <- rvest::html_nodes(html_page, "table")
    prop_tbl <- rvest::html_table(url_nodes[[3]], fill = T, header = T)
    if(fg_tm == "all"){
      #the column names appear in the first row
      colnames(prop_tbl) <- prop_tbl[1,]
      #removing the first two rows which do not accurate info
      prop_tbl <- prop_tbl[-c(1:2),]
    }else{
      prop_tbl <- prop_tbl
    }

    if(scout_grades == FALSE){
    #removing the video column from the value table since these are not useful for data analysis
    prop_tbl <- data.frame(prop_tbl[, -which(names(prop_tbl)=="Video")])
    }
    #adding in year of ranking
    prop_tbl <- data.frame(prop_tbl, Year = yr)
  })
  prop_full <- do.call(rbind, prop_list)
  return(prop_full)
}

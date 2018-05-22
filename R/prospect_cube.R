#'Pull Prospect Rankings
#'
#'Scrape data from \url{http://www.thebaseballcube.com} to look at the top 100 prospects list for each team or the top prospects by team.
#'Can use this information to approximate the relative strength of each teams farm system.
#'
#' @param team Specify the team abbreviation as character value. Use this value to pull the top prospects by team
#' @param year Numeric year. Specifying the year will pull only the top 100 prospects. Default is to take the past full season if pulled in the offseason or the current season if pulled after opening day.
#' @param start_year Numeric value that identifies the beginning year to pull a range of data for the team of interest. This is an optional parameter.
#' @param end_year Numeric value that identifies the ending year to pull a range of data for the team of interest. This is an optional parameter.
#' @param src Character value specifying the source to use for the prospect rankings. Options are `"BA"` for Baseball America or `"PIPE"` for MLB Pipeline. Default is Baseball America
#'
#' @import xml2
#' @import rvest
#'
#' @examples #pulling top 100 prospectes from 2017
#' 2017_prospects <- prospect_cube(year = 2017, src = "PIPE")
#' table(2017_prospects$current_lvl)
#'
#' #pulling prospect rankings from Colorado Rockies from Baseball America
#' col_prospects <- prospect_cube(team = "COL", src = "BA")
#' library(dplyr)
#' top_prospects <- col_prospects %>%
#'                    group_by(Year)%>%
#'                    summarise(num_top_100 = sum(MLB!=""), avg_age = mean(as.numeric(Age)))
#' top_prospects <- top_prospects[order(top_prospects$num_top_100, decreasing = TRUE),]
#' top_prospects
#'
#' @export

prospect_cube <- function(team, year, src = "BA", start_year = NULL, end_year = NULL){
  #checking to make sure all three parameters are not specified
  if(missing(year) == FALSE & is.null(start_year) == FALSE & is.null(end_year) == FALSE){
    warning("Should not specify year with both start_year and end_year. Only pulling the range specified by start_year and end_year", call. = FALSE)
    year <- NULL
  }

  #checking to make sure only year or team is specified
  if((missing(year)==FALSE | is.null(start_year) == FALSE | is.null(end_year) == FALSE) & missing(team)==FALSE){
    stop("Must specify only one of team or year", call. = F)
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


  #using the team_specific_fill function as a way to check that this variable is properly specified.
  if(missing(team)==FALSE){
    team_info <- team_specific_fill(team)
    cube_numeric <- rwindow.baseball::MLB_colors[rwindow.baseball::MLB_colors$team==team, "cube_id"]
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
      if(start_year>end_year){
        stop("Improperly specified range. start_year must be less than or equal to end_year", call. = F)
        #making sure that the start year is always lower than the end year
      }
    }
  }

  #Pulling data from the baseballcube
  if(missing(team)==TRUE){
    yr_tbls <- lapply(start_year:end_year, function(yr){
      base_url <- "http://www.thebaseballcube.com/prospects/byYear.asp?Y="
      url <- paste0(base_url, yr, "&Src=", src)
      html_page <- xml2::read_html(url)
      url_nodes <- rvest::html_nodes(html_page, "table")
      top_tbl <- rvest::html_table(url_nodes[[15]], fill = T, header = T)
      #splitting the batting and throwing handedness into two separate variables
      top_tbl$bat_hand <- substr(top_tbl$`B-T`, start = 1, stop = 1)
      top_tbl$throw_hand <- substr(top_tbl$`B-T`, start = 3, stop = 3)
      #removing the combined variable
      top_tbl <- top_tbl[,-which(names(top_tbl)=="B-T")]

      #extracting and cleaning the column names
      names(top_tbl) <- gsub("\\s", "_", names(top_tbl))
      #splitting the current organization and current level into two separate variables
      top_tbl$current_org <- gsub("\\s*\\([^\\)]+\\)$", "", top_tbl$`Current_Org_(Level)`)
      top_tbl$current_lvl <- gsub("(?<=\\()[^()]*(?=\\))(*SKIP)(*F)|.", "", top_tbl$`Current_Org_(Level)`, perl=T)
      #removing the combined variable
      top_tbl <- top_tbl[,-which(names(top_tbl)=="Current_Org_(Level)")]
      top_tbl$Year <- yr
      return(top_tbl)
    })
    #using lapply we have a list object for the range of years specified by start and end year so now I bind this together into one data.frame
    full_top <- data.frame(do.call(rbind, yr_tbls))
    return(full_top)
  }else if(missing(team)==FALSE){
    base_url <- "http://www.thebaseballcube.com/prospects/byTeam.asp?T="
    if(src == "BA"){
      tm_url <- paste0(base_url, cube_numeric)
    }else if(src == "PIPE"){
      tm_url <- paste0(base_url, cube_numeric, "&Src=PIPE")
    }
    html_page <- xml2::read_html(tm_url)
    url_nodes <- rvest::html_nodes(html_page, "table")
    tm_tbl <- rvest::html_table(url_nodes[[15]], fill = T, header = T)
    #removing the extra rows with the header
    tm_tbl <- tm_tbl[-which(tm_tbl$Year=="Year"),]
    #adding in an identifier of the source for the rankings
    tm_tbl$src <- src

    #splitting the batting and throwing handedness into two separate variables
    tm_tbl$bat_hand <- substr(tm_tbl$`B-T`, start = 1, stop = 1)
    tm_tbl$throw_hand <- substr(tm_tbl$`B-T`, start = 3, stop = 3)
    #removing the combined variable
    tm_tbl <- tm_tbl[,-which(names(tm_tbl)=="B-T")]

    #extracting and cleaning the column names
    names(tm_tbl) <- gsub("\\s", "_", names(tm_tbl))

    #splitting the current organization and current level into two separate variables
    tm_tbl$current_org <- gsub("\\s*\\([^\\)]+\\)$", "", tm_tbl$`Current_Org_(Level)`)
    tm_tbl$current_lvl <- gsub("(?<=\\()[^()]*(?=\\))(*SKIP)(*F)|.", "", tm_tbl$`Current_Org_(Level)`, perl=T)
    #removing the combined variable
    tm_tbl <- tm_tbl[,-which(names(tm_tbl)=="Current_Org_(Level)")]
    tm_tbl$Team <- team
    return(tm_tbl)
  }
}

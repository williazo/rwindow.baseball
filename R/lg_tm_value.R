#'Pull the value by team and season
#'
#'By default the system uses the current year
#'
#' @param league Character league abbreviation. Either AL or NL.
#' @param batting Indicator variable to specify whether to pull batting or pitching value. By default it is set to batting.
#' @param year Numeric year. Default is to take the past full season if pulled in the offseason or the current season if pulled after opening day.
#' @param start_year Numeric value that identifies the beginning year to pull a range of data for the team of interest. This is an optional parameter.
#' @param end_year Numeric value that identifies the ending year to pull a range of data for the team of interest. This is an optional parameter.
#'
#' @import xml2
#' @import rvest
#'
#' @examples #pulling AL team batting values from the years 2009 to 2012
#' lg_tm_value("AL", start_year = 2009, end_year = 2012)
#' #pulling NL team values from most recent season
#' nl_pitching <- lg_tm_value("NL", batting = FALSE)
#'
#' #team with the highest and lowest pitching WAR from 2017 season in the National League
#' rbind(nl_pitching[which(nl_pitching$WAR == max(nl_pitching$WAR)), c("Tm", "WAR")],
#' nl_pitching[which(nl_pitching$WAR == min(nl_pitching$WAR)), c("Tm", "WAR")])
#'
#' @export

lg_tm_value <- function(league, year, batting = T, start_year = NULL, end_year = NULL){

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

  #adding in check to make sure that batting is a T/F variable
  if(is.logical(batting) == FALSE){
    stop("batting must be specified as a T/F variable", call. = FALSE)
  }
  #Added a league check. Must be capital for this function
  if(league %in% c("AL", "NL") == F){
    stop("Must specify league as either AL or NL", call. = F)
  }

  base_url <- "https://www.baseball-reference.com/leagues/"

  #specifying whether to pull team batting war or pitching war
  if(batting == T){
    value_pull <- "-value-batting"
  } else if (batting == F){
    value_pull <- "-value-pitching"
  } else{
    stop("batting parameter only takes on T/F values.", call. = F)
  }

  #if just year is specified then we want to pull a single year
  if(is.null(start_year)==T & is.null(end_year) == T){
    #checking if a numeric value is specified
    if(is.numeric(year) == F){
      stop("year must be specified as a numeric value", call. = F)
    } else if (year> as.numeric(format(Sys.Date(), "%Y"))){
      stop("year is misspecified. Cannot be greater than the current year", call. = F)
    } else if(year < 2000){
      stop("year is too far back. Only pulling from the year 2000.")
    }

    url <- paste0(base_url, league,"/", year, value_pull, ".shtml")
    html_page <- xml2::read_html(url)
    tables <- rvest::html_nodes(html_page, "table")
    value_table <- rvest::html_table(tables)

    value_dat <- as.data.frame(value_table)
    #removing unnecessary league summary statistics and the G column which has no values
    value_dat <- value_dat[- which(value_dat$Tm == ""), -2]
    #converting salary to numeric value
    value_dat$Salary <- as.numeric(gsub("[[:punct:]]", "", value_dat$Salary))
    #fixing some of the table names
    names(value_dat)[c(13:14)]<-c("waaWL_pct", "WL_avgteam_pct")
    return(value_dat)
  }
  else if((is.null(start_year) == F & is.null(end_year) == T) | (is.null(start_year) == T & is.null(end_year) == F)){
    stop("to specify a range start_year and end_year must both be entered", call. = F)
  }
  else if(is.null(start_year) == F & is.null(end_year) == F){
    if(is.numeric(start_year) == F | is.numeric(end_year) ==F){
      stop("Both start_year and end_year must be numeric", call. = F)
    }
    else if(is.numeric(start_year) == T & is.numeric(end_year) == T){
      if(start_year>end_year){
        stop("Improperly specified range. start_year must be less than or equal to end_year", call. = F)
      }
      else{
        yr_range <- start_year:end_year
        #now we are going to loop over each year and attach them together
        value_final <- NULL
        for(i in yr_range){
          url <- paste0(base_url, league,"/", i, value_pull, ".shtml")
          html_page <- xml2::read_html(url)
          tables <- rvest::html_nodes(html_page, "table")
          value_table <- rvest::html_table(tables)

          #value table
          value_dat <- as.data.frame(value_table)
          #removing unnecessary league summary statistics
          value_dat <- value_dat[- which(value_dat$Tm == ""), ]
          #converting salary to numeric value
          value_dat$Salary <- as.numeric(gsub("[[:punct:]]", "", value_dat$Salary))
          #fixing some of the table names
          names(value_dat)[c(13:14)]<-c("waaWL_pct", "WL_avgteam_pct")
          #adding the year onto the table for reference
          value_dat <- data.frame(Year = rep(i, nrow(value_dat)), value_dat)
          value_final <- rbind(value_final, value_dat)
        }
        return(value_final)
      }
    }
  }
}

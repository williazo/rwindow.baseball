#'Pull the value by team and season
#'
#'By default the system uses the current year
#'
#' @param league Character league abbreviation. Either AL or NL.
#' @param batting Indicator variable to specify whether to pull batting or pitching value. By default it is set to batting.
#' @param year Numeric year
#' @param start_year Numeric value that identifies the beginning year to pull a range of data for the team of interest. This is an optional parameter.
#' @param start_year Numeric value that identifies the ending year to pull a range of data for the team of interest. This is an optional parameter.
#'
#' @import xml2
#' @import rvest
#'
#' @examples #pulling AL team batting values from the years 2009 to 2012
#' lg_tm_value("AL", start_year = 2009, end_year = 2012)
#' #pulling NL team values from most recent season
#'lg_tm_value("NL", batting = F)
#'
#' @export

lg_tm_value <- function(league, year = as.numeric(format(Sys.Date(), "%Y")), batting = T,
                            start_year = NULL, end_year = NULL){
  #need to be a league check here
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
    #removing unnecessary league summary statistics
    value_dat <- value_dat[- which(value_dat$Tm == ""), ]
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
          value_dat <- data.frame(Year = rep(i, nrow(value_dat)), value_dat)
          value_final <- rbind(value_final, value_dat)
        }
        return(value_final)
      }
    }
  }
}

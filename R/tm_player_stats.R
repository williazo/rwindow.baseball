#'Pull the team stats for pitching and fielding
#'
#'By default the system uses the current year
#'
#' @param team Team abbreviation
#' @param year Numeric value representing the year. Default is to take the current year if the season has started. Or the most recent year if the season is over.
#' @param start_year Numeric value that identifies the beginning year to pull a range of data for the team of interest. This is an optional parameter.
#' @param start_year Numeric value that identifies the ending year to pull a range of data for the team of interest. This is an optional parameter.
#'
#' @import xml2
#' @import rvest
#'
#'
#' @export

tm_player_stats <- function(team, year, start_year = NULL, end_year = NULL){
  #using this as a check to make sure that the team abbrev was specified correctly
  team_info <- team_specific_fill(team)
  base_url <- "https://www.baseball-reference.com/teams/"

  #will need to update this each season since opening day changes every year.
  opening_day <- as.Date("03-29-2018", format = "%m-%d-%Y")
  if(missing(year)){
    if(((opening_day - Sys.Date()) >= 0) == T){
      year = as.numeric(format(opening_day, "%Y")) - 1
    } else{
      year = as.numeric(format(Sys.Date(), "%Y"))
    }
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
    #The Tampa Bay Rays were the Tampa Bay Devil Rays prior to 2007
    if(team == "TBR" & year < 2008){
      team = "TBD"
    }
    #The Los Angeles Angels were the Anaheim Angels before 2004
    if(team == "LAA" & year < 2005){
      team = "ANA"
    }
    #Washington Nationals were the Montreal Expos previously
    if(team == "WSN" & year < 2005){
      team = "MON"
    }
    #Miami Marlins were the Florida Marlins
    if(team == "MIA" & year < 2012){
      team = "FLA"
    }

    url <- paste0(base_url, team,"/", year, ".shtml")
    html_page <- xml2::read_html(url)
    tables <- rvest::html_nodes(html_page, "table")
    player_tables <- rvest::html_table(tables)

    hitting_dat <- as.data.frame(player_tables[[1]])
    pitching_dat <- as.data.frame(player_tables[[2]])
    tm_stats_dat <- list(hitting_dat, pitching_dat)
    return(tm_stats_dat)
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
        pitching_final <- NULL
        hitting_final <- NULL
        player_final <- NULL
        for(i in yr_range){
          tm_current = team
          #making name abbreviation adjustments for earlier years
          if(team == "TBR" & i < 2008){
            tm_current = "TBD"
          }
          if(team == "LAA" & i < 2005){
            tm_current = "ANA"
          }
          if(team == "WSN" & i < 2005){
            tm_current = "MON"
          }
          if(team == "MIA" & i < 2012){
            tm_current = "FLA"
          }
          url <- paste0(base_url, tm_current ,"/", i, ".shtml")
          html_page <- xml2::read_html(url)
          tables <- rvest::html_nodes(html_page, "table")
          player_tables <- rvest::html_table(tables)

          #hitting specific data
          hitting_dat <- as.data.frame(player_tables[[1]])
          hitting_dat <- data.frame(Year = rep(i, nrow(hitting_dat)), hitting_dat)
          hitting_final <- rbind(hitting_final, hitting_dat)

          #pitching specific data
          pitching_dat <- as.data.frame(player_tables[[2]])
          pitching_dat <- data.frame(Year = rep(i, nrow(pitching_dat)), pitching_dat)
          pitching_final <- rbind(pitching_final, pitching_dat)
        }
        final_player <- list(hitting_final, pitching_final)
        return(final_player)
      }
    }
  }
}

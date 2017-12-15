#'Pull the team standings and game by game schedule with results
#'
#'By default the system uses the current year
#'
#' @param team Team abbreviation, three leter character value. See the data object \code{MLB_colors} for approved abbreviations.
#' @param year Numeric year. Default is to take the past full season if pulled in the offseason or the current season if pulled after opening day.
#' @param start_year Numeric value that identifies the beginning year to pull a range of data for the team of interest. This is an optional parameter.
#' @param end_year Numeric value that identifies the ending year to pull a range of data for the team of interest. This is an optional parameter.
#'
#' @import xml2
#' @import rvest
#'
#' @return
#'
#' @export

tm_standings_schedule <- function(team, year, start_year = NULL, end_year = NULL){

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

  #using this as a check to make sure that the team abbrev was specified correctly
  team_info <- team_specific_fill(team)
  base_url <- "https://www.baseball-reference.com/teams/"
  #changing the tampa bay rays reference

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

    url <- paste0(base_url, team,"/", year, "-schedule-scores.shtml")
    html_page <- xml2::read_html(url)
    tables <- rvest::html_nodes(html_page, "table")
    game_table <- rvest::html_table(tables)

    game_dat <- as.data.frame(game_table)
    #removing rows that have unnecessary headers
    game_dat <- game_dat[-which(game_dat$Var.3 == ""), ]
    #creating a home indicator variable
    game_dat$home_gm <- ifelse(game_dat$Var.5 == "", 1, 0)
    #removing unnecessary columns
    game_dat <- game_dat[, -c(3,5)]
    game_dat
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
          final_game <- NULL
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
            url <- paste0(base_url, tm_current ,"/", i, "-schedule-scores.shtml")
            html_page <- xml2::read_html(url)
            tables <- rvest::html_nodes(html_page, "table")
            game_table <- rvest::html_table(tables)

            game_dat <- as.data.frame(game_table)
            #removing rows that have unnecessary headers
            game_dat <- game_dat[-which(game_dat$Var.3 == ""), ]
            #creating a home indicator variable
            game_dat$home_gm <- ifelse(game_dat$Var.5 == "", 1, 0)
            #removing unnecessary columns
            game_dat <- game_dat[, -c(3,5)]
            game_dat <- data.frame(Year = rep(i, nrow(game_dat)), game_dat)
            final_game <- rbind(final_game, game_dat)
          }
          return(final_game)
        }
      }
    }
}

#' Finding league, division, and team specific extension to read data from Baseball-Reference
#'
#' This function takes in a specific team abbreviation and returns the corresponding league, division, and their full name
#'
#' @param team Team abbreviation
#'
#' @return list that includes `league`, `division`, and `ext`
#'
#' @export
#'
#'


team_specific_fill <- function(team){
  al_east <- c("BOS", "NYY", "TOR", "BAL", "TBR")
  al_west <- c("HOU", "LAA", "SEA", "TEX", "OAK")
  al_cent <- c("CLE", "MIN", "KC", "CHW", "DET")

  nl_east <- c("WSN", "MIA", "ATL", "NYM", "PHI")
  nl_west <- c("SFG", "SDP", "COL", "ARI", "LAD")
  nl_cent <- c("CHC", "MIL", "STL", "PIT", "CIN")

  #checking to see if the team abbreviation was properly specified
  if(!team %in% c(al_east, al_west, al_cent, nl_east, nl_west, nl_cent)){
    stop(paste("Team abbrevation misspecified. Must be one of the following/n",
               paste(c(al_east, al_west, al_cent, nl_east, nl_west, nl_cent), collapse = ", ")),
         call. = FALSE)
  }
  if(team %in% c(al_east, al_west, al_cent)){
    #Checking for American League
    league = "American League"
    if(team %in% al_east){
      #checking for AL East Teams
      division = "East"
      if(team == "BOS"){
        ext = "boston-red-sox"
      } else if(team == "NYY"){
        ext = "new-york-yankees"
      } else if(team == "TOR"){
        ext = "toronto-blue-jays"
      } else if(team == "BAL"){
        ext = "baltimore-orioles"
      } else{
        ext = "tampa-bay-rays"
      }
    }
    else if(team %in% al_west){
      division = "West"
      #checking AL West teams
      if(team == "HOU"){
        ext = "houston-astros"
      } else if(team == "LAA"){
        ext = "los-angeles-angels"
      } else if(team == "SEA"){
        ext = "seattle-mariners"
      } else if(team == "TEX"){
        ext = "texas-rangers"
      } else{
        ext = "oakland-athletics"
      }
    }
    else{
      division = "Central"
      if(team == "CLE"){
        ext = "cleveland-indians"
      } else if(team == "MIN"){
        ext = "minnesota-twins"
      } else if(team == "KC"){
        ext = "kansas-city-royals"
      } else if(team == "CHW"){
        ext = "chicago-white-sox"
      } else{
        ext = "detroit-tigers"
      }
    }
  }
  else if (team %in% c(nl_east, nl_west, nl_cent)){
    league = "National League"
    if(team %in% nl_east){
      division = "East"
      if(team == "WSN"){
        ext = "washington-nationals"
      } else if(team == "MIA"){
        ext = "miami-marlins"
      }else if (team == "ATL"){
        ext = "atlanta-braves"
      }else if(team == "NYM"){
        ext = "new-york-mets"
      } else{
        ext = "philadelphia-phillies"
      }
    }
    else if(team %in% nl_west){
      division = "West"
      if(team == "SFG"){
        ext = "san-francisco-giants"
      } else if (team == "SDP"){
        ext = "san-diego-padres"
      } else if (team == "COL"){
        ext = "colorado-rockies"
      } else if (team == "ARI"){
        ext = "arizona-diamondbacks"
      } else{
        ext = "los-angeles-dodgers"
      }
    }
    else{
      division = "Central"
      if (team == "CHC"){
        ext = "chicago-cubs"
      } else if (team == "MIL"){
        ext = "milwaukee-brewers"
      } else if (team == "STL"){
        ext = "st-louis-cardinals"
      } else if (team == "PIT"){
        ext = "pittsburgh-pirates"
      } else{
        ext = "cincinnati-reds"
      }
    }
  }
  return(list(league, division, ext))
}

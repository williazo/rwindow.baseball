#'Reference table for MLB Color Palette as well as Team Abbreviation, League, Division, Full Name, and Short-hand Name.
#'
#'This data set consists the current MLB teams as of 2017.
#'Information on the team colors was pulled from \url{http://jim-nielsen.com/teamcolors/}.
#'
#' @format A data frame with 30 rows and 7 variables
#' \describe{
#'   \item{team}{official team abbreviation}
#'   \item{color}{hexcolor for each team}
#'   \item{League}{American League or National League}
#'   \item{Division}{West, Central, or East}
#'   \item{short_name}{Common team reference}
#'   \item{Full_Name}{Include team name and city/state}
#'   \item{fg_id}{Numeric reference for pulling team-specific data from \url{http://www.fangraphs.com}}
#'   }
"MLB_colors"

#' Pulling the contract information from Baseball-Reference
#'
#' Takes in the team abbreviation and returns their current and projected salary information based on 2017 data
#'
#' @param team Official team abbreviation
#'
#' @return Data.frame object that contains the salary information
#' @import xml2
#' @import rvest
#'
#' @examples #reading in the Boston Red Sox salary
#' library(rwindow.baseball)
#' tm_contract("BOS")
#'
#' @export

tm_contract <- function(team){
  base_url <- "https://www.baseball-reference.com/teams/"
  team_info <- team_specific_fill(team)
  if(team == "LAA"){
    #the contract info for the Los Angeles Angeles still uses the Anaheim abbreviation
    team = "ANA"
  }else if(team == "MIA"){
    #contract info for Miami Marlins is listed under FLA
    team = "FLA"
  } else if(team == "TBR"){
    team = "TBD"
  }
  url <- paste0(base_url, team,"/",team_info[[3]], "-salaries-and-contracts.shtml")
  html_page <- xml2::read_html(url)
  tables <- rvest::html_nodes(html_page, "table")
  salary_table <- rvest::html_table(tables)
  sal_dat <- as.data.frame(salary_table)
  sal_dat <- data.frame(Team = rep(team, nrow(sal_dat)), Division = rep(team_info[[2]], nrow(sal_dat)),
                        League = rep(team_info[[1]], nrow(sal_dat)), sal_dat)


  #player specific data
  pla_dat <- sal_dat[1:(which(sal_dat$Name == "Name")-1), ]
  #team summary data
  #for the summary statistics the description spans multiple columns so I am removing that
  sum_dat <- sal_dat[(which(sal_dat$Name == "Name") + 1):nrow(sal_dat), -c(6:10)]
  names(sum_dat)[c(4,5)] <- c("Variable", "Description")
  #removing extra blank rows
  sum_dat <- sum_dat[-which(sum_dat$Variable==""),]
  return(list(pla_dat, sum_dat))
}

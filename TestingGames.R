#File: TestingGames.R
#Author: zecellomaster
#Date: 03/24/21 (v1.0)
#Description: This program will take the name of two teams, home and away, then
#             either return the chances of win/loss/draw or a dataframe of simulated
#             match scores. This requires the SoccerEloForecast package
#Notes: Be sure to set the working directory to the same place as where all the
#       files are.

rm(list = ls())

library(SoccerEloForecast)

#Here, enter the home and away team names as they appear on the history files.
#Use the 
home_name <- "Real Salt Lake"
away_name <- "Vancouver Whitecaps"
home_status <- TRUE #Whether or not the game is at the home team's stadium
match_date_raw <- "09/19/2020"
return_what <- "chances"
bos <- FALSE #Set if this is the first game of the season
starting_elo <- 1500 #The value used as the starting/average Elo

match_date <- as.Date(match_date_raw,format ="%m/%d/%Y")
match_year <- as.numeric(format(match_date, format="%Y"))

#Import previous games for both teams
team_data <- list(read.csv(paste("./MLS Elo Histories/", home_name," History.csv", sep = ""),
         header = TRUE),read.csv(paste("./MLS Elo Histories/", away_name," History.csv", sep = "")))

#We only count the games that have occurred prior to the match date

marks <- matrix(c(1, dim(team_data[[1]])[1], 1, dim(team_data[[2]])[1]), ncol = 2, 
                nrow = 2)

for (i in 1:length(team_data)){
  for (j in 1:dim(team_data[[i]])[1]){
    current_date <- as.Date(team_data[[i]][j,"Date"],format ="%m/%d/%Y")
    current_year <- team_data[[i]][j,"Year"]
    
    
    #Ingests the last 2 seasons of matches (if available)
    if (current_year == (match_year - 2) && marks[1,i] == 1){  
      marks[1,i] <- j
      next
      
    }else if (current_date >= match_date){
      marks[2,i] <- j - 1
      break
    }
  }
}

if ((marks[1,1] - marks[2,1]) == 1){ #This detects if there are no previous games
  home_elo <- starting_elo
  away_elo <- team_data[[2]][marks[2,2],"A_Elo_After"]
  
  home_data = data.frame()
  away_data <-  data.frame(team_elo = team_data[[2]][marks[3]:marks[4],"A_Elo_Before"], 
                           opp_elo = team_data[[2]][marks[3]:marks[4],"B_Elo_Before"],
                           team_score = team_data[[2]][marks[3]:marks[4],"A_Score"],
                           opp_score = team_data[[2]][marks[3]:marks[4],"B_Score"],
                           team_home = team_data[[2]][marks[3]:marks[4],"A_Home"],
                           team_away = team_data[[2]][marks[3]:marks[4],"B_Home"],
                           weight = team_data[[2]][marks[3]:marks[4],"Weight"])
}else if ((marks[1,2] - marks[2,2]) == 1){
  away_elo <- starting_elo
  home_elo <- team_data[[1]][marks[2,1],"A_Elo_After"]
  
  home_data <- data.frame(team_elo = team_data[[1]][marks[1]:marks[2],"A_Elo_Before"], 
                          opp_elo = team_data[[1]][marks[1]:marks[2],"B_Elo_Before"],
                          team_score = team_data[[1]][marks[1]:marks[2],"A_Score"],
                          opp_score = team_data[[1]][marks[1]:marks[2],"B_Score"],
                          team_home = team_data[[1]][marks[1]:marks[2],"A_Home"],
                          team_away = team_data[[1]][marks[1]:marks[2],"B_Home"],
                          weight = team_data[[1]][marks[1]:marks[2],"Weight"])
  
  away_data <-  data.frame()
  
}else{
  home_data <- data.frame(team_elo = team_data[[1]][marks[1]:marks[2],"A_Elo_Before"], 
                          opp_elo = team_data[[1]][marks[1]:marks[2],"B_Elo_Before"],
                          team_score = team_data[[1]][marks[1]:marks[2],"A_Score"],
                          opp_score = team_data[[1]][marks[1]:marks[2],"B_Score"],
                          team_home = team_data[[1]][marks[1]:marks[2],"A_Home"],
                          opp_home = team_data[[1]][marks[1]:marks[2],"B_Home"],
                          weight = team_data[[1]][marks[1]:marks[2],"Weight"])
  
  away_data <-  data.frame(team_elo = team_data[[2]][marks[3]:marks[4],"A_Elo_Before"], 
                           opp_elo = team_data[[2]][marks[3]:marks[4],"B_Elo_Before"],
                           team_score = team_data[[2]][marks[3]:marks[4],"A_Score"],
                           opp_score = team_data[[2]][marks[3]:marks[4],"B_Score"],
                           team_home = team_data[[2]][marks[3]:marks[4],"A_Home"],
                           opp_home = team_data[[2]][marks[3]:marks[4],"B_Home"],
                           weight = team_data[[2]][marks[3]:marks[4],"Weight"])
  home_elo <- team_data[[1]][marks[2,1],"A_Elo_After"]
  away_elo <- team_data[[2]][marks[2,2],"A_Elo_After"]
}


if (bos == TRUE){
  home_elo <- round((home_elo*(2/3)) + (1500/3))
  away_elo <- round((away_elo*(2/3)) + (1500/3))
}

elo <- c(home_elo,away_elo)

if (home_elo >= away_elo){
  odds <- gameprediction(return_what,c(home_elo,away_elo), c(home_status,FALSE), list(home_data, away_data))
  a_name <- home_name
  
  if (return_what == "chances"){
    chances <-c(odds[[1]],odds[[2]],odds[[3]])
    print(chances)
  }
  
  }else{
  odds <- gameprediction(return_what, c(away_elo,home_elo), c(FALSE,home_status), list(away_data, home_data))
  a_name <- away_name

  if (return_what == "chances"){
    chances <-c(odds[[2]],odds[[1]],odds[[3]])
    print(chances)
  }
  
}
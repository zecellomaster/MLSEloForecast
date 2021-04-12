#File: MLSElo.R
#Author: zecellomaster
#Date: 04/06/21 (v1.0)
#Description: This program will update the Elo ratings according to my the conventions
#             in MLSElo.R
rm(list = ls())

library(dplyr)

reg_loc <- "./Data/RegScores.csv"
post_loc <- "./Data/PostScores.csv"
name_location <-"./Data/MLS Names.csv"
team_format_location <- "./Data/Team Data Format.csv"
elo_loc <- "./Data/Current Elo.csv"


#Change to the date format used in the results
date_format = "%Y-%m-%d"

#Staring elo used elsewhere
starting_elo <- 1500

teams <-  read.csv(name_location,header = TRUE, fileEncoding="UTF-8-BOM")
reg_results <- read.csv(reg_loc,header = TRUE, fileEncoding="UTF-8-BOM")
post_results <- read.csv(post_loc,header = TRUE, fileEncoding="UTF-8-BOM")
current_elo <- read.csv(elo_loc,header = TRUE)


#Setting up the list that gets the histories of every team
ref_names <- teams[-c(10,12,13),c("Alt_Names","Team_Name")]
current_elo <- current_elo[-c(10,12,13),]

elo_history <- vector(mode= "list", length= length(ref_names$Alt_Names))
names(elo_history) <- ref_names$Alt_Names

for (i in 1:dim(ref_names)[1]){
  elo_history[[i]] <- read.csv(paste("./MLS Elo Histories/",ref_names$Team_Name[i]
                                     ," History.csv", sep=""),header = TRUE, 
                               fileEncoding="UTF-8-BOM", row.names = 1)
}
#For the regular season, the game weight (k_value) is 20
reg_results$Weight <- 20

#For the post season, the weight scheme follows the one used in MLSElo.R. In 2020
#the First Round is equivalent to the Conference Quarter finals
game_type <- data.frame(level = c("Round One","Conference Semifinals", "Conference Finals",
                                  "MLS Cup 2021"), #CHANGE BACK TO 2021 AFTER TESTING
                        game_rating = c(30,40,50,60))

if (dim(post_results)[1] > 0){
  for (i in dim(post_results)[1]){
    post_results$Weight <- (game_type$game_rating[grep(post_results$Round[i], game_type$level)])
  }
}

results <- rbind(reg_results,post_results)

for (i in 1:dim(results)[1]){
  if (results$Final[i] == "False"){
    next
  }
  
  current_date <- as.character(as.Date(results$Date[i], format= date_format), format = "%m/%d/%Y")
  current_year <- format(as.Date(current_date, format= "%m/%d/%Y"), format= "%Y")
  home_name <- results$Home[i]
  away_name <- results$Away[i]
  
  #We assume that if a date is already in a team's elo ratings, the match already happened
  if (current_date %in% elo_history[[home_name]]$Date || 
      current_date %in% elo_history[[away_name]]$Date){
    next
  }
  
  #The last row of each team
  home_row <- length(elo_history[[home_name]]$Date)
  away_row <- length(elo_history[[away_name]]$Date)
  
  home_index <- grep(home_name, ref_names$Alt_Names)
  away_index <- grep(away_name, ref_names$Alt_Names)
  
  home_elo <- current_elo$Elo[home_index]
  away_elo <- current_elo$Elo[away_index]
  
  #In the future, implement a system that checks the stadium venue. For now, 
  #it is assumed that all matches will be played on non neutral fields
  home_win_exp <- 1/((10^((-(home_elo - away_elo + 100))/400)+1))
  away_win_exp <- 1 - home_win_exp
  
  k_value <- results$Weight[i]
  
  home_goals <- results$Home_Score[i]
  away_goals <- results$Away_Score[i]
  
  goal_diff <- abs(home_goals - away_goals)
  
  #This value depends on the number of goals scored
  if (goal_diff <= 1){
    g_value <- 1
  }else if(goal_diff == 2){
    g_value <- 1.5
  } else if(goal_diff >= 3){
    g_value <- (11+goal_diff)/8
  }
  
  if (results$Winner[i] == "H"){
    current_elo$Elo[home_index] <- round(k_value * g_value * (1 - home_win_exp)) + home_elo
    current_elo$Elo[away_index] <- round(k_value * g_value * (0 - away_win_exp)) + away_elo
    
  }else if (results$Winner[i] == "A"){
    current_elo$Elo[home_index] <- round(k_value * g_value * (0 - home_win_exp)) + home_elo
    current_elo$Elo[away_index] <- round(k_value * g_value * (1 - away_win_exp)) + away_elo
    
  }else if (results$Winner[i] == "T"){
    current_elo$Elo[home_index] <- round(k_value * g_value * (0.5 - home_win_exp)) + home_elo
    current_elo$Elo[away_index] <- round(k_value * g_value * (0.5 - away_win_exp)) + away_elo 
  }
  
  #Storing all of the necessary values. Notice how this updates the current_elo values and
  #leave the respective team elos alone

  elo_history[[home_index]][home_row+1,] <- c(current_year, current_date,home_elo,away_elo,
                                              current_elo$Elo[home_index], 
                                              current_elo$Elo[away_index], home_goals, 
                                              away_goals, TRUE, FALSE, k_value)
  
  elo_history[[away_index]][away_row+1,] <- c(current_year, current_date,away_elo, 
                                              home_elo,current_elo$Elo[away_index], 
                                             current_elo[home_index,"Elo"],away_goals,
                                             home_goals, FALSE,TRUE, k_value)
  
}

for (i in 1:length(elo_history)){
  write.csv(elo_history[[i]],paste("./MLS Elo Histories/",current_elo$Team_Name[i],
                                   " History", ".csv", sep = ""))
}

write.csv(current_elo, "./Data/Current Elo.csv")



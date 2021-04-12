#File: MLSElo.R
#Author: zecellomaster
#Date: 03/07/21 (v1.3)
#Description: This program will utilize MLS Score data to formulate an Elo Score
#             for each team. This will include defunct teams as well. New teams 
#             always start with 1,500 points, and after every season, the points 
#             regress back to the mean by 33%.
#Notes: Be sure to set the working directory to the same place as where all the
#       files are. 
#Updates: 
#(v1.1) - Fixed goal difference calculation; added the team's goals instead of 
#         subtracting them. 
#       - Changed k_constant weights
#(v1.2) - Changed end of season regression (1/3 -> 1/4)
#       - Changed k_constant weights
#(v1.3) - Optimization
rm(list = ls())

#First, we input the files
match_data_location <- "./Data/MLSmatchesRefined.csv"
name_location <-"./Data/MLS Names.csv"
team_format_location <- "./Data/Team Data Format.csv"

raw_match_data <- read.csv(match_data_location,header = TRUE, fileEncoding="UTF-8-BOM")
data_format <- read.csv(team_format_location,header = TRUE, fileEncoding="UTF-8-BOM")
team_names <-  read.csv(name_location,header = TRUE, fileEncoding="UTF-8-BOM")

#You can set the starting Elo to whatever value you would like. The default is 
#1500
starting_elo <- 1500

#Here, we store the current Elo ratings for all the teams. 
current_elo <- team_names[,c("Team_Name","Look_For","Alt_Names")]
names(current_elo)[2] <- "Abbr"
current_elo[,"Elo"] <- starting_elo #Elo
  
#This part will store a team's Elo ratings. We form a list using the names of
#the teams
elo_history <- vector(mode= "list", dim(team_names)[1])
names(elo_history) <- team_names[,1]

#We set the list where team Elos and dates can be stored
for (i in 1:dim(team_names)[1]){
  elo_history[[i]] <- data_format
}

#Set up the identifying variables
game_type <- data.frame(level = c("RS","PI","C-QF", "C-SF", "C-F", "Cup"), 
                        game_rating = c(20,25,30,40,50,60))

#game_rating = c(30,35,45,45,60,60)
#game_rating = c(30,35,40,45,50,60))
#c(20,25,35,45,55,60)
#c(20,25,30,40,50,60))


for (i in 1:dim(raw_match_data)[1]){
  current_year <- raw_match_data$year[i]
  
  if (grepl("All-Stars", raw_match_data$home[i], fixed = TRUE) |
      grepl("All-Stars", raw_match_data$away[i], fixed = TRUE)){
        next 
      }
  
  #This part is because the dates after this point put the years in a different column
  if (i >= 974){
    current_date <- as.Date(paste(raw_match_data$date[i], current_year
                                  , sep = ","),format= "%A, %B %d,%Y")
  }else{
    current_date <- raw_match_data$date[i]
    
  }
  
  #This part deals with the post-season readjustment
  if (i != 1){
    if (current_year != raw_match_data$year[i-1] ){
      current_elo$Elo <- round((current_elo$Elo*3/4) + (starting_elo/4))
    }
  }
  
  
  #Items are as follows: Team name, Team score
  home_index <- grep(raw_match_data$home[i],current_elo$Abbr)
  away_index <- grep(raw_match_data$away[i],current_elo$Abbr)
  
  
  home_elo <- current_elo$Elo[home_index]
  away_elo <- current_elo$Elo[away_index]
  
  #This part calculates the win percentage and if the game is a neutral arena
  if (raw_match_data$neutral[i] == FALSE){
    if_home <- TRUE
    home_win_exp <- 1/((10^((-(home_elo - away_elo + 100))/400)+1))
    
  } else{
    if_home <- FALSE
    home_win_exp <- 1/((10^((-(home_elo - away_elo))/400)+1))
    
  }
  
  away_win_exp <- 1 - home_win_exp
  
  home_stats <- c(raw_match_data$home[i], raw_match_data$home_score[i])
  away_stats <- c(raw_match_data$away[i], raw_match_data$away_score[i])

  #Here we use the type of match to determine its significance
  k_value <- game_type[grep(raw_match_data$part_of_competition[i],game_type[,1]),2]
  goal_diff <- abs(as.numeric(home_stats[2]) - as.numeric(away_stats[2]))
  
  #This value depends on the number of goals scored
  if (goal_diff <= 1){
    g_value <- 1
  }else if(goal_diff == 2){
    g_value <- 1.5
  } else if(goal_diff >= 3){
    g_value <- (11+goal_diff)/8
  }
    
  #This calculates how many points will be added or subtracted to the team's 
  #rating
  if (home_stats[2] > away_stats[2]){
    current_elo$Elo[home_index] <- round(k_value * g_value * (1 - home_win_exp)) + home_elo
    current_elo$Elo[away_index] <- round(k_value * g_value * (0 - away_win_exp)) + away_elo
    
  }else if (home_stats[2] < away_stats[2]){
    current_elo$Elo[home_index] <- round(k_value * g_value * (0 - home_win_exp)) + home_elo
    current_elo$Elo[away_index] <- round(k_value * g_value * (1 - away_win_exp)) + away_elo
    
  }else if (home_stats[2] == away_stats[2]){
    current_elo$Elo[home_index] <- round(k_value * g_value * (0.5 - home_win_exp)) + home_elo
    current_elo$Elo[away_index] <- round(k_value * g_value * (0.5 - away_win_exp)) + away_elo 
  }
  
  #Storing all of the necessary values. Notice how we update the current_elo values and
  #leave the respective team elos alone
  elo_history[[home_index]][dim(elo_history[[home_index]])[1]+1,] <- c(current_year, 
                                      as.character(current_date, format = "%m/%d/%Y"),
                                      home_elo,away_elo,current_elo[home_index,"Elo"], 
                                      current_elo[away_index,"Elo"], home_stats[2], 
                                      away_stats[2], if_home, FALSE, k_value)
  
  elo_history[[away_index]][dim(elo_history[[away_index]])[1]+1,] <- c(current_year, 
                                        as.character(current_date, format = "%m/%d/%Y")
                                       ,away_elo, home_elo,current_elo[away_index,"Elo"], 
                                       current_elo[home_index,"Elo"],away_stats[2],
                                       home_stats[2], FALSE,if_home, k_value)
}

#Saving all the Elo histories. This makes sure you have a folder named 
#"MLS Elo Histories" in the directory
dir.create("./MLS Elo Histories")

for (i in 1:length(elo_history)){
  write.csv(elo_history[[i]],paste("./MLS Elo Histories/",current_elo$Team_Name[i],
                                   " History", ".csv", sep = ""))
}

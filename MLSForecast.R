#File: MLSForecast.R
#Author: zecellomaster
#Date: 03/31/21 (v0.5)
#Description: This program will use the Elo ratings/histories of the MLS Teams
#             and their schedules to forecast the season results. For most accurate
#             results, run this after MLSEloUpdater.R
rm(list = ls())
start_time <- proc.time()

library(SoccerEloForecast)

name_location <-"./Data/MLS Names.csv"
con_format <- "./Data/Conference Format.csv"
reg_loc <- "./Data/RegScores.csv"
post_loc <- "./Data/PostScores.csv"
elo_loc <- "./Data/Current Elo.csv"
reg_odds_loc <-"./Data/Reg Odds.csv"
post_odds_loc <-"./Data/Post Odds.csv"
season_format_loc <- "./Data/Season Odds Format.csv"
date_format <- "%Y-%m-%d"
num_sim <- 15000

#Set the number of years before 
years_before <- 3


teams <-  read.csv(name_location,header = TRUE, fileEncoding="UTF-8-BOM")
teams <- teams[-c(10,12,13),]
reg_results <- read.csv(reg_loc,header = TRUE, fileEncoding="UTF-8-BOM")
post_resuts <- read.csv(post_loc,header = TRUE, fileEncoding="UTF-8-BOM")
reset_elo <- read.csv(elo_loc,header = TRUE, row.names = 1)


comp_standings <- read.csv(con_format,header = TRUE, fileEncoding="UTF-8-BOM")
season_odds <- read.csv(season_format_loc,header = TRUE, fileEncoding="UTF-8-BOM")
  
  
comp_standings <- teams[,c("Alt_Names", "Team_Name","Conference")]
comp_standings[,c("Points","Num_Wins","Goal_Diff","Goals_For")] <- 0

#Getting the histories of every team
ref_names <- teams[,c("Alt_Names","Team_Name")]
reset_history <- vector(mode= "list", length= length(ref_names$Alt_Names))
names(reset_history) <- ref_names$Alt_Names

for (i in 1:dim(ref_names)[1]){
  reset_history[[i]] <- read.csv(paste("./MLS Elo Histories/",ref_names$Team_Name[i]
                                     ," History.csv", sep=""),header = TRUE, 
                               fileEncoding="UTF-8-BOM", row.names = 1)
}

#Import the previous match odds 
reg_odds <- read.csv(reg_odds_loc, header = TRUE, fileEncoding="UTF-8-BOM")

#Calculating season standings
for (i in 1:dim(reg_results)[1]){
  if (reg_results$Final[i] == "False"){
    reg_odds[i,c("Home_Wins", "Away_Wins", "Ties")] <- 0 #This is for later
    next
  }
  
  #Locations of the teams on the complete standings
  home_pos <- grep(reg_results$Home[i],comp_standings$Team) 
  away_pos <- grep(reg_results$Away[i],comp_standings$Team)
  
  #Points rules: Winner gets 3 points, loser gets 0, both teams receive 1 for a draw
  if (reg_results$Winner[i] == "H"){ #Home Team Wins
    comp_standings$Points[home_pos] <- comp_standings$Points[home_pos] + 3
    comp_standings$Num_Wins[home_pos] <- comp_standings$Num_Wins[home_pos] + 1
    
    score_diff <- abs(reg_results$Home_Score[i] - reg_results$Away_Score[i])
    
    comp_standings$Goal_Diff[home_pos] <- comp_standings$Goal_Diff[home_pos] + score_diff
    comp_standings$Goals_For[home_pos] <- comp_standings$Goals_For[home_pos] + reg_results$Home_Score[i]
    
    comp_standings$Goal_Diff[away_pos] <- comp_standings$Goal_Diff[away_pos] - score_diff
    comp_standings$Goals_For[away_pos] <- comp_standings$Goals_For[away_pos] + reg_results$Away_Score[i]
    
  } else if (reg_results$Winner[i] == "A"){ #Away Team wins
    comp_standings$Points[away_pos] <- comp_standings$Points[away_pos] + 3
    comp_standings$Num_Wins[away_pos] <- comp_standings$Num_Wins[away_pos] + 1
    
    score_diff <- abs(reg_results$Away_Score[i] - reg_results$Home_Score[i])
    
    comp_standings$Goal_Diff[home_pos] <- comp_standings$Goal_Diff[home_pos] - score_diff
    comp_standings$Goals_For[home_pos] <- comp_standings$Goals_For[home_pos] + reg_results$Home_Score[i]
    
    comp_standings$Goal_Diff[away_pos] <- comp_standings$Goal_Diff[away_pos] + score_diff
    comp_standings$Goals_For[away_pos] <- comp_standings$Goals_For[away_pos] + reg_results$Away_Score[i]
  
  } else if (reg_results$Winner[i] == "T"){ #Draw
    comp_standings$Points[away_pos] <- comp_standings$Points[away_pos] + 1
    comp_standings$Points[home_pos] <- comp_standings$Points[home_pos] + 1
    
    comp_standings$Goals_For[home_pos] <- comp_standings$Goals_For[home_pos] + reg_results$Home_Score[i]
    comp_standings$Goals_For[away_pos] <- comp_standings$Goals_For[away_pos] + reg_results$Away_Score[i]
    
  }
}


#Simulating regular season games

#For the regular season, the game weight (k_value) is 20. This is importand later
reg_results$Weight <- 20

#To do: repeat this 15000 times. Help me
#Reset variables
elo_history <- reset_history
current_elo <- reset_elo
reset_standings <- comp_standings

for (j in 1:num_sim){
  rm(elo_history,current_elo,comp_standings)
  
  elo_history <- reset_history
  current_elo <- reset_elo
  comp_standings <- reset_standings
  
    for(i in 1:dim(reg_results)[1]){
      if (reg_results$Final[i] == "True"){
        next
      }
      
      match_date <- as.Date(reg_results$Date[i], date_format)
      match_year <- as.numeric(format(match_date, format = "%Y"))
      
      home_name <- reg_results$Home[i]
      away_name <- reg_results$Away[i]
      
      #Locations of the teams on the complete standings
      home_pos <- grep(home_name,comp_standings$Alt_Names) 
      away_pos <- grep(away_name,comp_standings$Alt_Names)
      
      #Location of each team on the other tables (e.g. current_elo, teams)
      home_loc <- grep(home_name,current_elo$Alt_Names)
      away_loc <- grep(away_name,current_elo$Alt_Names)
      
      home_elo <- current_elo$Elo[home_loc]
      away_elo <- current_elo$Elo[away_loc]
      elo <- c(home_elo,away_elo)
      
      #First column is home team, second column is away team
      marks <- matrix(c(1, dim(elo_history[[home_name]])[1], 1, dim(elo_history[[away_name]])[1]), ncol = 2, 
                      nrow = 2)
      
      home_year_marks <- grep(as.character(match_year- years_before),elo_history[[home_name]]$Date)
      away_year_marks <- grep(as.character(match_year- years_before),elo_history[[away_name]]$Date)
      
      if (length(home_year_marks) > 0){
        marks[1,1] <- home_year_marks[1]
      }
      
      if (length(away_year_marks) > 0){
        marks[1,2] <- away_year_marks[1]
      }
      
      #This preps the data to be sent to the SoccerEloForecast Packages
      if ((marks[1,1] - marks[2,1]) == 1){  #This detects if there are no previous 
                                            #games for the home team
        home_data <- data.frame()
        away_data <-  data.frame(team_elo = elo_history[[away_name]][marks[3]:marks[4],"A_Elo_Before"], 
                                 opp_elo = elo_history[[away_name]][marks[3]:marks[4],"B_Elo_Before"],
                                 team_score = elo_history[[away_name]][marks[3]:marks[4],"A_Score"],
                                 opp_score = elo_history[[away_name]][marks[3]:marks[4],"B_Score"],
                                 team_home = elo_history[[away_name]][marks[3]:marks[4],"A_Home"],
                                 opp_home = elo_history[[away_name]][marks[3]:marks[4],"B_Home"],
                                 weight = elo_history[[away_name]][marks[3]:marks[4],"Weight"])
      
        }else if ((marks[1,2] - marks[2,2]) == 1){   #This detects if there are no previous games for 
                                                   #the away team
        home_data <- data.frame(team_elo = elo_history[[home_name]][marks[1]:marks[2],"A_Elo_Before"], 
                                opp_elo = elo_history[[home_name]][marks[1]:marks[2],"B_Elo_Before"],
                                team_score = elo_history[[home_name]][marks[1]:marks[2],"A_Score"],
                                opp_score = elo_history[[home_name]][marks[1]:marks[2],"B_Score"],
                                team_home = elo_history[[home_name]][marks[1]:marks[2],"A_Home"],
                                opp_home = elo_history[[home_name]][marks[1]:marks[2],"B_Home"],
                                weight = elo_history[[home_name]][marks[1]:marks[2],"Weight"])
        
        away_data <-  data.frame()
        
      }else{
        home_data <- data.frame(team_elo = elo_history[[home_name]][marks[1]:marks[2],"A_Elo_Before"], 
                                opp_elo = elo_history[[home_name]][marks[1]:marks[2],"B_Elo_Before"],
                                team_score = elo_history[[home_name]][marks[1]:marks[2],"A_Score"],
                                opp_score = elo_history[[home_name]][marks[1]:marks[2],"B_Score"],
                                team_home = elo_history[[home_name]][marks[1]:marks[2],"A_Home"],
                                opp_home = elo_history[[home_name]][marks[1]:marks[2],"B_Home"],
                                weight = elo_history[[home_name]][marks[1]:marks[2],"Weight"])
        
        away_data <-  data.frame(team_elo = elo_history[[away_name]][marks[3]:marks[4],"A_Elo_Before"], 
                                 opp_elo = elo_history[[away_name]][marks[3]:marks[4],"B_Elo_Before"],
                                 team_score = elo_history[[away_name]][marks[3]:marks[4],"A_Score"],
                                 opp_score = elo_history[[away_name]][marks[3]:marks[4],"B_Score"],
                                 team_home =elo_history[[away_name]][marks[3]:marks[4],"A_Home"],
                                 opp_home = elo_history[[away_name]][marks[3]:marks[4],"B_Home"],
                                 weight = elo_history[[away_name]][marks[3]:marks[4],"Weight"])
      }
      
      
      if (home_elo >= away_elo){
        sim_match <- gameprediction(1,c(home_elo,away_elo), c(TRUE,FALSE), list(home_data, away_data))
        names(sim_match) <- c("home_goals", "away_goals")
        
      }else if (home_elo < away_elo){
        sim_match <- gameprediction(1, c(away_elo,home_elo), c(FALSE,TRUE), list(away_data, home_data))
        names(sim_match) <- c("away_goals", "home_goals")
      }
      #Quick clean up
      rm(home_data,away_data)
      
      #Calculating Elo values to update current_elo and updating the current standings
      home_win_exp <- 1/((10^((-(home_elo - away_elo + 100))/400)+1))
      away_win_exp <- 1 - home_win_exp
      
      k_value <- reg_results$Weight[i]
      
      score_diff <- abs(sim_match$home_goals[1] - sim_match$away_goals[1])
    
      #This value depends on the number of goals scored
      if (score_diff <= 1){
        g_value <- 1
      }else if(score_diff == 2){
        g_value <- 1.5
      } else if(score_diff >= 3){
        g_value <- (11+score_diff)/8
      }
      
      
      if (sim_match$home_goals[1] > sim_match$away_goals[1]){
        current_elo$Elo[home_loc] <- round(k_value * g_value * (1 - home_win_exp)) + home_elo
        current_elo$Elo[away_loc] <- round(k_value * g_value * (0 - away_win_exp)) + away_elo
        
        comp_standings$Points[home_pos] <- comp_standings$Points[home_pos] + 3
        comp_standings$Num_Wins[home_pos] <- comp_standings$Num_Wins[home_pos] + 1
        
        comp_standings$Goal_Diff[home_pos] <- comp_standings$Goal_Diff[home_pos] + score_diff
        comp_standings$Goals_For[home_pos] <- comp_standings$Goals_For[home_pos] + sim_match$home_goals[1]
        
        comp_standings$Goal_Diff[away_pos] <- comp_standings$Goal_Diff[away_pos] - score_diff
        comp_standings$Goals_For[away_pos] <- comp_standings$Goals_For[away_pos] + sim_match$away_goals[1]
        
        reg_odds$Home_Wins[i] <- reg_odds$Home_Wins[i] + 1
        
      }else if (sim_match$home_goals[1] < sim_match$away_goals[1]){
        current_elo$Elo[home_loc] <- round(k_value * g_value * (0 - home_win_exp)) + home_elo
        current_elo$Elo[away_loc] <- round(k_value * g_value * (1 - away_win_exp)) + away_elo
        
        comp_standings$Points[away_pos] <- comp_standings$Points[away_pos] + 3
        comp_standings$Num_Wins[away_pos] <- comp_standings$Num_Wins[away_pos] + 1
        
        comp_standings$Goal_Diff[home_pos] <- comp_standings$Goal_Diff[home_pos] - score_diff
        comp_standings$Goals_For[home_pos] <- comp_standings$Goals_For[home_pos] + sim_match$home_goals[1]
        
        comp_standings$Goal_Diff[away_pos] <- comp_standings$Goal_Diff[away_pos] + score_diff
        comp_standings$Goals_For[away_pos] <- comp_standings$Goals_For[away_pos] + sim_match$away_goals[1]
        
        reg_odds$Away_Wins[i] <- reg_odds$Away_Wins[i] + 1
        
        
      }else if (sim_match$home_goals[1] == sim_match$away_goals[1]){
        current_elo$Elo[home_loc] <- round(k_value * g_value * (0.5 - home_win_exp)) + home_elo
        current_elo$Elo[away_loc] <- round(k_value * g_value * (0.5 - away_win_exp)) + away_elo 
        
        comp_standings$Points[away_pos] <- comp_standings$Points[away_pos] + 1
        comp_standings$Points[home_pos] <- comp_standings$Points[home_pos] + 1
        
        comp_standings$Goals_For[home_pos] <- comp_standings$Goals_For[home_pos] + sim_match$home_goals[1]
        comp_standings$Goals_For[away_pos] <- comp_standings$Goals_For[away_pos] + sim_match$away_goals[1]
        
        reg_odds$Ties[i] <- reg_odds$Ties[i] + 1
  
    }
    
    #Storing all of the necessary values. Notice how this updates the current_elo values and
    #leave the respective team elos alone
    
    elo_history[[home_name]][dim(elo_history[[home_name]])[1]+1,] <- c(match_year, match_date,home_elo,away_elo,
                                                current_elo$Elo[home_loc], 
                                                current_elo$Elo[away_loc], sim_match$home_goals[1], 
                                                sim_match$away_goals[1], TRUE, FALSE, k_value)
    
    elo_history[[away_name]][dim(elo_history[[away_name]])[1]+1,] <- c(match_year, match_date,away_elo, 
                                                home_elo,current_elo$Elo[away_loc], 
                                                current_elo$Elo[home_loc],sim_match$away_goals[1],
                                                sim_match$home_goals[1], FALSE,TRUE, k_value)
    
    
  }
  
  
  #To do: Calculate the season standings and start postseason simulations
  dfs <- split(comp_standings, comp_standings$Conference)
  west_standings <- dfs[["W"]]
  east_standings <- dfs[["E"]]
  
  west_order <-  order(-west_standings$Points, -west_standings$Num_Wins, 
                       -west_standings$Goal_Diff, -west_standings$Goals_For)
  
  east_order <- order(-east_standings$Points, -east_standings$Num_Wins, 
                      -east_standings$Goal_Diff, -east_standings$Goals_For)
  
  season_odds$Avg_Points <- season_odds$Avg_Points + comp_standings$Points
  season_odds$Avg_Goal_Diff <- season_odds$Avg_Goal_Diff + comp_standings$Goal_Diff
}

print(proc.time() - start_time)


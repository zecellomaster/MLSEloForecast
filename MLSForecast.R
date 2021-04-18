#File: MLSForecast.R
#Author: zecellomaster
#Date: 03/31/21 (v0.5)
#Description: This program will use the Elo ratings/histories of the MLS Teams
#             and their schedules to forecast the season results. For most accurate
#             results, run this after MLSEloUpdater.R. Takes ~24 secoonds per 100 
#             simulations
rm(list = ls())

source("./MLSFunctions.R")

library(SoccerEloForecast)
library(dplyr)
library(doParallel)

cl <- makeCluster(detectCores(), type='PSOCK')
registerDoParallel(cl)

save_season_loc <- "./Output/RefinedScoreTable.csv"
save_odds_loc <- "./Output/SeasonOdds.csv"

total_dur <- 0

name_location <-"./Data/MLS Names.csv"
con_format <- "./Data/Conference Format.csv"
reg_season_loc <- "./Data/RegScores.csv"
post_loc <- "./Data/PostScores.csv"
elo_loc <- "./Data/Current Elo.csv"
reg_tally_loc <-"./Data/Reg Tally.csv"
post_tally_loc <-"./Data/Post Tally.csv"
season_format_loc <- "./Data/Season Odds Format.csv"
date_format <- "%Y-%m-%d"
num_sim <- 15000 #number of simulations to be run

teams <-  read.csv(name_location,header = TRUE, fileEncoding="UTF-8-BOM")
teams <- filter(teams, teams$Alt_Names != "N/A")


reg_results <- read.csv(reg_season_loc,header = TRUE, fileEncoding="UTF-8-BOM")
post_results <- read.csv(post_loc,header = TRUE, fileEncoding="UTF-8-BOM")
current_elo <- read.csv(elo_loc,header = TRUE, row.names = 1)

#Team regressions
reg_loc <- "./Data/Team Regs.csv"
team_regs <- read.csv(reg_loc,row.names = 1)

comp_standings <- read.csv(con_format,header = TRUE, fileEncoding="UTF-8-BOM")
  
comp_standings <- teams[,c("Alt_Names","Conference")]
comp_standings[,c("Points","Num_Wins","Goal_Diff","Goals_For")] <- 0

#Names of the teams
ref_names <- filter(teams[,c("Alt_Names","Team_Name")],Alt_Names != "N/A")

#End of season odds
season_odds <- data.frame(Alt_Names = ref_names$Alt_Names)#read.csv(season_format_loc,header = TRUE, fileEncoding="UTF-8-BOM")
season_odds$Elo <- current_elo$Elo

#Import the previous match tally (# of times home wins, away wins, or both draw) 
reg_tally <- read.csv(reg_tally_loc, header = TRUE, fileEncoding="UTF-8-BOM",row.names = 1)
post_tally <- read.csv(post_tally_loc, header = TRUE, fileEncoding="UTF-8-BOM")

#separate completed regular season games from future ones 
comp_reg_games <- filter(reg_results, reg_results$Final == TRUE | reg_results$Final == "True")
future_reg_games <- filter(reg_results, reg_results$Winner == "N/A")

comp_reg_tally <- filter(reg_tally, reg_results$Final == TRUE  | reg_results$Final == "True") 
future_reg_tally <- matrix(0, nrow = nrow(future_reg_games), ncol = 3)
colnames(future_reg_tally) <- c("Home_Wins", "Away_Wins", "Ties")

#separate completed post season games from future ones
comp_post_games <- filter(post_results, post_results$Final == TRUE | reg_results$Final == "True")
future_post_games <- filter(post_results, post_results$Winner == "N/A")

comp_post_tally <- filter(post_tally, post_results$Final == TRUE | reg_results$Final == "True") 
future_post_tally <- matrix(0, nrow = nrow(future_post_games), ncol = 3)
colnames(future_post_tally) <- c("Home_Wins", "Away_Wins", "Ties")

"
##Calculate current season standings (uncomment when completed results exist)
lapply(seq(1:nrow(comp_reg_games)), FUN = function(x){
  home_pos <- grep(comp_reg_games$Home[x],ref_names$Alt_Names) 
  away_pos <- grep(comp_reg_games$Away[x],ref_names$Alt_Names)
  
  home_score <-  comp_reg_games$Home_Score[x]
  away_score <-  comp_reg_games$Away_Score[x]
  score_diff <- home_score - away_score
  
  standings_update <- standingsadjust(score_diff)
  
  comp_standings$Points[home_pos] <<- comp_standings$Points[home_pos] + standings_update[1]
  comp_standings$Num_Wins[home_pos] <<- comp_standings$Num_Wins[home_pos] +  standings_update[2]
  comp_standings$Goal_Diff[home_pos] <<- comp_standings$Goal_Diff[home_pos] + score_diff 
  comp_standings$Goals_For[home_pos] <<- comp_standings$Goals_For[home_pos] +  home_score
  
  comp_standings$Points[away_pos] <<- comp_standings$Points[away_pos] + standings_update[3]
  comp_standings$Num_Wins[away_pos] <<- comp_standings$Num_Wins[away_pos] +  standings_update[4]  
  comp_standings$Goal_Diff[away_pos] <<- comp_standings$Goal_Diff[away_pos] - score_diff 
  comp_standings$Goals_For[away_pos] <<- comp_standings$Goals_For[away_pos] + away_score

  
})
"

##Simulating regular season games

#For the regular season, the game weight (k_value) is 20. This is important later
future_reg_games$Weight <- 20

#To do: repeat this 15000 times. Help me
avg_stats <- matrix(0, nrow = nrow(comp_standings), ncol = 4)
colnames(avg_stats) <- colnames(comp_standings[,3:6])

make_playoffs <- integer(nrow(comp_standings))
make_semis <- integer(nrow(comp_standings))
make_champ <- integer(nrow(comp_standings))
make_cup <- integer(nrow(comp_standings))
win_shield <-integer(nrow(comp_standings))
bye <- integer(nrow(comp_standings))
win_cup <- integer(nrow(comp_standings))

start_time <- proc.time()
lapply(seq(1:num_sim), FUN = function(y){
  elo <- current_elo$Elo
  team_points <- comp_standings$Points
  num_wins <- comp_standings$Num_Wins
  goal_diff <- comp_standings$Goal_Diff
  goals_for <- comp_standings$Goals_For
  
  lapply(seq(1:nrow(future_reg_games)), 
  FUN = function(x){
    home_pos <- grep(future_reg_games$Home[x],ref_names$Alt_Names) 
    away_pos <- grep(future_reg_games$Away[x],ref_names$Alt_Names)

    home_elo <- elo[home_pos] + 100
    away_elo <-  elo[away_pos]
    
    home_regs <- team_regs[home_pos,]
    away_regs <- team_regs[away_pos,]
    
    k_con <- future_reg_games$Weight[x]
    
    if(home_elo >= away_elo){
      score <- gamesim(home_elo,away_elo,home_regs,away_regs, TRUE)
      
    }else{
      score <- gamesim(away_elo,home_elo,away_regs,home_regs, FALSE)
      
    }
    score_diff <- score$home_goals[1]-score$away_goals[1]
    
    adjust_elo <- eloadjust(home_elo,away_elo,score_diff,k_con)
    elo[home_pos] <<- elo[home_pos] + adjust_elo
    elo[away_pos] <<- elo[away_pos] - adjust_elo
    
    standings_update <- standingsadjust(score_diff)
    
    team_points[home_pos] <<- team_points[home_pos] + standings_update[1]
    num_wins[home_pos] <<- num_wins[home_pos] +  standings_update[2]
    goal_diff[home_pos] <<- goal_diff[home_pos] + score_diff 
    goals_for[home_pos] <<- goals_for[home_pos] +  score$home_goals[1]
    
    team_points[away_pos] <<- team_points[away_pos] + standings_update[3]
    num_wins[away_pos] <<- num_wins[away_pos] +  standings_update[4]  
    goal_diff[away_pos] <<- goal_diff[away_pos] - score_diff 
    goals_for[away_pos] <<- goals_for[away_pos] + score$away_goals[1]
    
    
    if (score_diff > 0){
      future_reg_tally[x,1] <<- future_reg_tally[x,1] + 1
    }else if (score_diff < 0){
      future_reg_tally[x,2] <<- future_reg_tally[x,2] + 1
    }else{
      future_reg_tally[x,3] <<- future_reg_tally[x,3] + 1
    }
    
    return()})
  
  avg_stats <<- avg_stats + c(team_points, num_wins, goal_diff, goals_for)
  
  #Calculate the season standings and start postseason simulations
  #Rank the teams using  tiebreaker rules (points -> # of wins -> goal difference ->
  #goals for)
  comp_order <- order(-team_points, -num_wins, 
                      -goal_diff, -goals_for)
  
  
  
  ranked_standings <- comp_standings$Conference[comp_order]
  
  #Winner of the overall table wins the Supporters' Shield
  win_shield[comp_order[1]] <<- win_shield[comp_order[1]] + 1
  
  #Table rankings for each conference
  west_order <- comp_order[ranked_standings == "W"]
  
  east_order <- comp_order[ranked_standings == "E"]
  
  #conference winners get  first round bye
  bye[west_order[1]] <<- bye[west_order[1]] + 1
  bye[east_order[1]] <<- bye[east_order[1]] + 1
  
  #The top 7 teams make it to the post season, so here we take those teams and
  #indicate they made it to the playoffs
  make_playoffs[west_order[1:7]] <<- make_playoffs[west_order[1:7]] + 1
  make_playoffs[east_order[1:7]] <<- make_playoffs[east_order[1:7]] + 1
  
  #seed the teams
  west_seeds <- cbind(west_order[1:7],seq(1:7))
  east_seeds <- cbind(east_order[1:7],seq(1:7))
  
  
  #Simulates entire post season (1 seed bye)
  #First round
  vs <- matrix(c(2,7,3,6,4,5), nrow = 2, ncol = 3)
  
  west_seeds <- round_sim(elo, west_seeds,vs,team_regs, 30)
  east_seeds <- round_sim(elo, east_seeds,vs,team_regs, 30)
  
  #Winners are semi-finalists
  make_semis[west_seeds[,1]] <<- make_semis[west_seeds[,1]] + 1
  make_semis[east_seeds[,1]] <<- make_semis[east_seeds[,1]] + 1
  
  #Semifinal Round
  vs <- matrix(c(1,4,2,3), nrow = 2, ncol = 2)
  west_seeds <- round_sim(elo, west_seeds,vs,team_regs, 40)
  east_seeds <- round_sim(elo, east_seeds,vs,team_regs, 40)
  
  #Winners are into the conference championship
  make_champ[west_seeds[,1]] <<- make_champ[west_seeds[,1]] + 1
  make_champ[east_seeds[,1]] <<- make_champ[east_seeds[,1]] + 1
  
  #Conf. Champ Round
  vs <- matrix(c(1,2), nrow = 2, ncol = 1)
  west_seeds <- round_sim(elo, west_seeds,vs,team_regs, 50)
  east_seeds <- round_sim(elo, east_seeds,vs,team_regs, 50)
  
  #Winners go to MLS Cup
  make_cup[west_seeds[1]] <<- make_cup[west_seeds[1]] + 1
  make_cup[east_seeds[1]] <<- make_cup[east_seeds[1]] + 1
  
  #Simulate MLS Cup (Western Conference will host)
  home_loc <- west_seeds[1] 
  away_loc <- east_seeds[1]
  
  home_elo <- elo[home_loc] + 100
  away_elo <- elo[away_loc]
  
  if (home_elo >= away_elo){
    result  <- gamesim(home_elo, away_elo, team_regs[home_loc,], 
                       team_regs[away_loc,], TRUE)
  }else{
    result  <-  gamesim(away_elo, home_elo,team_regs[away_loc,], 
                        team_regs[home_loc,], FALSE)
  }
  
  home_goals <- result$home_goals[1]
  away_goals <- result$away_goals[1]
  
  if (home_goals > away_goals){
    win_cup[home_loc] <<- win_cup[home_loc] + 1
    
  }else if (home_goals > away_goals){
    win_cup[away_loc] <<- win_cup[away_loc] + 1
    
  }else{
    home_win_exp <- 1/((10^((-(home_elo - away_elo))/400)+1))
    
    if (runif(1, min = 0 , max = 1) >= home_win_exp){
      win_cup[away_loc] <<- win_cup[away_loc] + 1
      
    }else{
      win_cup[home_loc] <<- win_cup[home_loc] + 1
      
    }
  }
  
})

  #Match the simulated games to the schedule
  #future_reg_games[,c("Home_Score","Away_Score")] <- sim_season
  avg_stats <- round(avg_stats/num_sim)
  
#  season_odds[,c("Avg_Points","Avg_Goal_Diff","Make_Playoffs", "First_Round_Bye",
#                 "Win_Shield", "Make_Semis","Make_Champ", "Make_Cup")] <-
#  c()
  
  season_odds$Conference <- teams$Conference
  season_odds$Avg_Points <- avg_stats[,"Points"]
  season_odds$Avg_Goal_Diff <- avg_stats[,"Goal_Diff"]
  season_odds$Make_Playoffs <- round(make_playoffs/num_sim,3)
  season_odds$First_Round_Bye <- round(bye/num_sim, 3)
  season_odds$Win_Shield <- round(win_shield/num_sim,3)
  season_odds$Make_Semis <- round(make_semis/num_sim,3)
  season_odds$Make_Champ <- round(make_champ/num_sim,3)
  season_odds$Make_Cup <- round(make_cup/num_sim,3)
  season_odds$Win_Cup <- round(win_cup/num_sim,3)
  
  refined_results <- subset(reg_results, select = -c(Round,Winner,Score))
  
  refined_tally <- round(rbind(comp_reg_tally,future_reg_tally)/num_sim,3)
  
  refined_results[,c("Home_Chance", "Away_Chance", "Tie")] <-  refined_tally
  
  write.csv(refined_results, save_season_loc)
  
  combined_tally <- rbind(comp_reg_tally,future_reg_tally)
  write.csv(combined_tally, reg_tally_loc)
  write.csv(season_odds, save_odds_loc)
  
  end_time <- proc.time()
  dur_time <- end_time - start_time
  print(dur_time)

registerDoSEQ()
  
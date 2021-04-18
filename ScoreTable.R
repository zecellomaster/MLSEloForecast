#File: ScoreTable.R
#Author: zecellomaster
#Date: 04/17/21 (v1.0)
#Description: This program will take the name of two teams, home and away, then
#             create a score table using their *current elo ratings*
#Notes: Be sure to set the working directory to the same place as where all the
#       files are.
rm(list = ls())

library(dplyr)
library(ggplot2)
source("./MLSFunctions.R")

num_sims <- 15000

#Team regressions
reg_loc <- "./Data/Team Regs.csv"
name_location <-"./Data/MLS Names.csv"
elo_loc <- "./Data/Current Elo.csv"

team_regs <- read.csv(reg_loc,row.names = 1)
teams <-  read.csv(name_location,header = TRUE, fileEncoding="UTF-8-BOM")
teams <- filter(teams, teams$Alt_Names != "N/A")
current_elo <- read.csv(elo_loc,header = TRUE, row.names = 1)

home_name <- "NYCFC"
away_name <- "FC Cincinnati"
match_date_raw <- "4/17/2021"

home_loc <- grep(home_name,teams$Alt_Names)
away_loc <- grep(away_name,teams$Alt_Names)

home_elo <- current_elo$Elo[home_loc]
away_elo <- current_elo$Elo[away_loc]

home_reg <- team_regs[home_loc,]
away_reg <- team_regs[away_loc,]

match_results <- lapply(seq(1:num_sims), FUN = function(x){
  if (home_elo >= away_elo){
    score <- gamesim(home_elo,away_elo,home_reg, away_reg, TRUE)
    
  }else{
    score <- gamesim(away_elo,home_elo,away_reg, home_reg, FALSE)
    
  }
  
  return(score)
})

match_results <- do.call("rbind", match_results)

match_results[match_results > 5] <- 5 

goal_probs <- as.data.frame(table(match_results))
goal_probs$Freq <- (goal_probs$Freq/num_sims)*100

plot_data <- ggplot(goal_probs, aes(x = home_goals,y = away_goals, fill = Freq)) +
  geom_tile() +  xlab(paste(home_name, "Goals Scored")) + ylab(paste(away_name, " Goals Scored")) +
  geom_text(aes(label=Freq))+
  ggtitle(paste("Projected Scores for ", home_name, " vs ", away_name, " (", match_date_raw, ") ", sep = "")) +
  scale_fill_gradient2(low = "white", high = "red", name = "% Chance") +
  theme_minimal() #+ guide_legend()

score_plot <- plot_data + stat_bin2d()
score_plot 


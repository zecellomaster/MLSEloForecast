#File: TeamRegressions.R
#Author: zecellomaster
#Date: 04/14/21 (v1.0)
#Description: This program will use the Elo ratings/histories of the MLS Teams
#             and their schedules to fit a Possion distribution for each team
rm(list = ls())

source("./MLSFunctions.R")
library(dplyr)

name_location <-"./Data/MLS Names.csv"
con_format <- "./Data/Conference Format.csv"
date_format <- "%Y-%m-%d"
expansion_loc <- "./Data/Avg Starting Team.csv"
save_loc <- "./Data/Team Regs.csv"

teams <-  read.csv(name_location,header = TRUE, fileEncoding="UTF-8-BOM")
years_before <- 2
last_year <- 2020

#Getting the histories of every team
ref_names <- filter(teams[,c("Alt_Names","Team_Name")],Alt_Names != "N/A")
elo_history <- vector(mode= "list", length= length(ref_names$Alt_Names))
names(elo_history) <- ref_names$Alt_Names


for (i in 1:nrow(ref_names)){
  elo_history[[i]] <- read.csv(paste("./MLS Elo Histories/",ref_names$Team_Name[i]
                                       ," History.csv", sep=""),header = TRUE, 
                                 fileEncoding="UTF-8-BOM", row.names = 1)
}

team_regs <- vector(mode= "list", length= length(elo_history)-1)

for (i in (1:length(elo_history))){
  if (nrow(elo_history[[i]]) == 0){
    next
  }

  marks <- matrix(c(1, dim(elo_history[[i]])[1], ncol = 1,nrow = 2))
  
  year_marks <- grep(as.character(last_year- years_before),elo_history[[i]]$Year)
  
  if (length(year_marks) > 0){
    marks[1,1] <- year_marks[1]
  }

  team_data <- data.frame(team_elo = elo_history[[i]][marks[1]:marks[2],"A_Elo_Before"], 
                          opp_elo = elo_history[[i]][marks[1]:marks[2],"B_Elo_Before"],
                          team_score = elo_history[[i]][marks[1]:marks[2],"A_Score"],
                          opp_score = elo_history[[i]][marks[1]:marks[2],"B_Score"],
                          team_home = elo_history[[i]][marks[1]:marks[2],"A_Home"],
                          opp_home = elo_history[[i]][marks[1]:marks[2],"B_Home"])
  
  team_name <- ref_names$Alt_Names[i]
  team_regs[[i]] <- pregess(team_name,team_data)
  }

team_regs <-  do.call('rbind',team_regs)
avg_expansion_data <- read.csv(expansion_loc,fileEncoding="UTF-8-BOM",)



expansion_data <-  data.frame(team_elo = avg_expansion_data[,"A_Elo_Before"], 
                              opp_elo = avg_expansion_data[,"B_Elo_Before"],
                              team_score = avg_expansion_data[,"A_Score"],
                              opp_score = avg_expansion_data[,"B_Score"],
                              team_home = avg_expansion_data[,"A_Home"],
                              opp_home = avg_expansion_data[,"B_Home"])

average_reg <- pregess("Austin FC",expansion_data)

if(nrow(elo_history[["Austin FC"]]) < 25 && nrow(elo_history[["Austin FC"]]) > 0){
  weight <-  nrow(elo_history[["Austin FC"]])/25
  team_regs["Austin FC",]  <- ((team_regs["Austin FC",] * weight) + (average_reg *(1-weight)))/2
  
} else{
  team_regs <- rbind(team_regs,average_reg)
}

write.csv(team_regs,save_loc)



#File: OverTime.R
#Author: zecellomaster
#Date: 4/17/21
#Description: This program will import and save the chances of various scenarios
#             occuring over time.
rm(list = ls())
categories <- c("Make_Playoffs", "First_Round_Bye", "Win_Shield", "Make_Semis",
                "Make_Champ","Make_Cup", "Win_Cup")
season_odds <- read.csv( "./Output/SeasonOdds.csv", row.names = 1)

current_date <- Sys.Date()
date_format <- "%m/%d/%Y"

#old_cats <- vector(mode= "list", length(categories))

for (i in 1:length(categories)){
  cat_loc <- paste("./Output/",categories[i], " Over Time.csv", 
                   sep = "")
  
  current_cat <-read.csv(cat_loc, header = FALSE)
  end_col <- ncol(current_cat)
  last_date <- as.Date(current_cat[1,end_col], date_format)
  
  if (last_date < current_date){
    current_cat[, end_col + 1] <- c(as.character(current_date, format = date_format),
                                    season_odds[,categories[i]])
  }else if(last_date == current_date){
    current_cat[, end_col] <- c(as.character(current_date, format = date_format),
                                season_odds[,categories[i]])
  }
  
  write.table(current_cat,cat_loc,sep = ",",col.names = FALSE, row.names = FALSE)
  
}


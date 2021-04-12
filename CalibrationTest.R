#File: CalibrationTest.R
#Author: zecellomaster
#Date: 03/15/21 (v1.0)
#Description: This program will  test the calibration of the MLS Elo Scores.
#Notes: Be sure to set the working directory to the same place as where all the
#       files are. This is meant to supplement the MLSElo.R script, so run that first
#       (or have the variables in the environment).
library(ggplot2)

bins <- data.frame(centers = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100))
bins[,c("Total_Events","Occured_Events","Combined_Per")] <- 0
lowest_chance <- c(100,0,0)
bounds <- 2.5
ties <- data.frame()


for (i in 1:(length(elo_history)-1)){
  for(j in 1:dim(elo_history[[i]])[1]){
    a_goals <- as.numeric(elo_history[[i]][j,7])
    b_goals <- as.numeric(elo_history[[i]][j,8])
    
    if(a_goals == b_goals){
      next
    }
    
    a_elo <- as.numeric(elo_history[[i]][j,3])
    b_elo <- as.numeric(elo_history[[i]][j,4])

    if (elo_history[[i]][j,9] == TRUE){
      a_win_exp <- (1/((10^((-(a_elo - b_elo + 100))/400)+1))) *100
      
    } else{
      a_win_exp <- (1/((10^((-(a_elo - b_elo))/400)+1))) * 100
    }
    
    b_win_exp <- 100 - a_win_exp
    
    for(k in 1:dim(bins)[1]){
      a_diff <- abs(a_win_exp - bins[k,1])
      b_diff <- abs(b_win_exp - bins[k,1])
      
      if (a_diff <= bounds){
        bins$Total_Events[k] <-  bins$Total_Events[k] + 1
        bins$Combined_Per[k] <- bins$Combined_Per[k] + a_win_exp
        
        if (a_goals > b_goals){
          bins$Occured_Events[k] <- bins$Occured_Events[k] + 1
          if (lowest_chance[1] > a_win_exp){
            lowest_chance[1] <- a_win_exp
            lowest_chance[2:3] <- c(i,j)
          }
        }
        #If the prediction is in the 50% bin, allow the other team's result to be acounted for
        if (a_win_exp - 50 > bounds){ 
          next
        }
      }
      
      if (b_diff <= bounds){
        bins$Total_Events[k] <- bins$Total_Events[k] + 1
        bins$Combined_Per[k] <- bins$Combined_Per[k] + b_win_exp
        
        if (a_goals < b_goals){
          bins$Occured_Events[k] <- bins$Occured_Events[k] + 1
          if (lowest_chance[1] > b_win_exp){
            lowest_chance[1] <- b_win_exp
            lowest_chance[2:3] <- c(i,j)
          }
        }
        next
      }
    }
  }
}

bins[,"Avg_Centers"] <- 0
bins[,"Occurance (%)"] <- 0


for (i in 1:dim(bins)[1]){
  bins[i,"Occurance (%)"] <- (bins$Occured_Events[i]/bins$Total_Events[i]) * 100
  bins$Avg_Centers[i] <-  (bins$Combined_Per[i]/bins$Total_Events[i])
}

write.csv(bins, "./CalibrationPlot.csv")

cal_plot <- ggplot(bins[,5:6], aes(x = bins$Avg_Centers,
                                   y = bins[,"Occurance (%)"]))+ geom_point(
                                     aes(size=bins$Total_Events)) + geom_abline(
                                       intercept = 0, slope = 1)
cal_plot


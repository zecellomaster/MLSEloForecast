library(ggplot2)

if (a_name == home_name){
  match_results <- data.frame(home_res = odds[,"a_goals"], away_res = odds[,"b_goals"])
  
}else{
  match_results <- data.frame(home_res = odds[,"b_goals"], away_res = odds[,"a_goals"])
}

match_results[match_results > 5] <- 5 

goal_probs <- as.data.frame(table(match_results))
goal_probs$Freq <- (goal_probs$Freq/return_what)*100





plot_data <- ggplot(goal_probs, aes(x = home_res,y = away_res, fill = Freq)) +
  geom_tile() +  xlab(paste(home_name, "Goals Scored")) + ylab(paste(away_name, " Goals Scored")) +
  geom_text(aes(label=Freq))+
  ggtitle(paste("Projected Scores for ", home_name, " vs ", away_name, " (", match_date_raw, ") ", sep = "")) +
  scale_fill_gradient2(low = "white", high = "red", name = "% Chance") +
  theme_minimal() #+ guide_legend()

score_matrix <- plot_data + stat_bin2d()
score_matrix


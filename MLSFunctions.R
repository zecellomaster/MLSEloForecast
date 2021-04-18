#File: MLFunctions.R
#Author: zecellomaster
#Date: 04/12/21 (v0.5)
#Description: Functions used by other scripts in MLSEloForecast

library(speedglm)

#Adjust the standings of teams based on matches. This is from the perspective of
#the home team
standingsadjust <- function(score_diff){

  #Returns home points,home_win, away_points, away_win

  if (score_diff > 0){
    return(c(3,1,0,0))

  } else if (score_diff < 0){
    return(c(0,0,3,1))

  }else{
    return(c(1,0,1,0))
  }
}

#Calculate the current season standings


#Adjust elo (from perspective of home team). Assumes 100 pt adjustment
#has already been made
eloadjust <- function(home_elo, away_elo, score_diff, k_value){

  home_win_exp <- 1/((10^((-(home_elo - away_elo))/400)+1))

  if (abs(score_diff) <= 1){
    g_value <- 1
  }else if(abs(score_diff) == 2){
    g_value <- 1.5
  } else if(abs(score_diff) >= 3){
    g_value <- (11+abs(score_diff))/8
  }

  if (score_diff > 0){
    match_result <- 1

  } else if (score_diff < 0){
    match_result <- 0

  }else{
    match_result <- 0.5
  }

  return(round(k_value * g_value * (match_result - home_win_exp)))
}

pregess <- function(team_name,team_data){
  #Adjusts data to account for home team advantage
  team_data$team_elo[team_data$team_home == TRUE] <-  team_data$team_elo[team_data$team_home == TRUE] + 100
  team_data$opp_elo[team_data$opp_home == TRUE] <- team_data$opp_elo[team_data$opp_home == TRUE] + 100
  
  #Poisson Regression of goals scored *for* the team as a function of the opponent's Elo
  mu <- speedglm(formula = team_score ~ opp_elo ,family = poisson(link = "log"),
                 data =  team_data)
  
  #Poisson regression of goals scored *against* the team as a function of the team's Elo
  nu <- speedglm(formula = opp_score ~ opp_elo ,family = poisson(link = "log"),
                 data =  team_data)
  
  #Nested Poisson regression if team has lesser elo
  nest_reg <- speedglm(formula = team_score ~ opp_elo + opp_score, family = poisson(link = "log"),
                        data = team_data)
  
  return(data.frame(team = team_name,alpha_1 = unname(coef(mu)["(Intercept)"]),alpha_2 = unname(coef(mu)["opp_elo"]), 
                    beta_1 = unname(coef(nu)["(Intercept)"]), beta_2 = unname(coef(nu)["opp_elo"]),
                    gamma_1 = unname(coef(nest_reg)["(Intercept)"]),
                    gamma_2 = unname(coef(nest_reg)["opp_elo"]), gamma_3 = unname(coef(nest_reg)["opp_score"])))
}

#Simulates game
gamesim <- function(a_elo,b_elo, a_reg,b_reg, home){
  if (a_elo < b_elo){
    warning("Error.Team A's Elo must be greater than or equal to Team B'S Elo.")
  }
  lambda_a <- exp((a_reg$alpha_1 + a_reg$alpha_2*b_elo))/2 + exp((b_reg$beta_1 + b_reg$beta_2*a_elo))/2
  a_goals <- rpois(1,lambda_a)
  
  lambda_b <- exp((b_reg$gamma_1) + (b_reg$gamma_2*a_elo) + (b_reg$gamma_3*a_goals))
  b_goals <- rpois(1,lambda_b)
  
  if(home == TRUE){
    return(data.frame(home_goals = a_goals, away_goals = b_goals))
    
  }else{
    return(data.frame(home_goals = b_goals, away_goals = a_goals))
    
  }
}

round_sim <- function(elos,seeds,schedule, regs, round){
  
  lapply(seq(1:ncol(schedule)), FUN = function(x){
    home_loc <- seeds[schedule[1,x], 1]
    away_loc <- seeds[schedule[2,x], 1]
    
    home_elo <- elos[home_loc] +100
    away_elo <- elos[away_loc]
    
    home_regs <- regs[home_loc,]
    away_regs <- regs[home_loc,]
    
    
    if (home_elo >= away_elo){
      score <- gamesim(home_elo,away_elo,home_regs,away_regs, TRUE)
  
    }else{
      score <- gamesim(away_elo,home_elo,away_regs,home_regs, FALSE)
    
    }
    score_diff <- score$home_goals - score$away_goals
    elo_points <- eloadjust(home_elo, away_elo, score_diff, round)
    
    elos[home_loc] <<- elos[home_loc] + elo_points
    elos[away_loc] <<- elos[away_loc] - elo_points
    
    if (score_diff > 0){ 
      seeds <<- seeds[-schedule[2,x],]
      
    }else if (score_diff < 0){
      seeds <<- seeds[-schedule[1,x],]
      
    }else{
      home_win_exp <- 1/((10^((-(home_elo - away_elo))/400)+1))
      if (runif(1, min = 0 , max = 1) >= home_win_exp){
        seeds <<- seeds[-schedule[1,x],]
      }else{
        seeds <<- seeds[-schedule[2,x],]
        
      }
    }
    return()})
  return(seeds)
}



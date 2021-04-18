# -*- coding: utf-8 -*-
"""
Created on Thu Apr  1 14:28:56 2021

@author: Amanze Ejiogu

Name: ScoreScraper.py 
Description: Script that retrieves schedules and scores of the MLS season from
FBref.com. The goal is to output 2 csv files: one with the regular season games,
the other (if available, with post season games)
"""

import pandas as pd
import datetime as dt

data_frames = pd.read_html("https://fbref.com/en/comps/22/11006/schedule/2021-Major-League-Soccer-Scores-and-Fixtures")
    #"https://fbref.com/en/comps/22/2798/schedule/2019-Major-League-Soccer-Scores-and-Fixtures")   
    #"https://fbref.com/en/comps/22/11006/schedule/2021-Major-League-Soccer-Scores-and-Fixtures")
    #"https://fbref.com/en/comps/22/schedule/Major-League-Soccer-Scores-and-Fixtures")

current_date = dt.date.today()

 #"https://fbref.com/en/comps/9/schedule/Premier-League-Scores-and-Fixtures               
 #https://fbref.com/en/comps/9/schedule/Premier-League-Scores-and-Fixtures#sched_10728_1")
    #
    #"https://fbref.com/en/comps/22/11006/schedule/2021-Major-League-Soccer-Scores-and-Fixtures")

if len(data_frames) == 3:
    regular_season = data_frames[1]
    post_season = data_frames[2]
    
else:
    regular_season = data_frames[0]
    #Add blank post_season if this is the case
    post_season = pd.DataFrame(columns= regular_season.columns)
    
for i in range(2):
    if i == 0:
        final_data = regular_season[["Date","Home","Away","Score"]]
        raw_data = regular_season[["Date","Home","Away","Score"]]
        final_data["Round"] = "Regular Season"
        save_loc = "./Data/RegScores.csv"
        
    elif i == 1 and len(data_frames) == 3 :
        final_data = post_season[["Date","Home","Away","Score","Round"]]
        raw_data = post_season[["Date","Home","Away","Score","Round"]]
        save_loc = "./Data/PostScores.csv"

    elif i == 1:
        final_data = post_season[["Date","Home","Away","Score"]]
        raw_data = post_season[["Date","Home","Away","Score"]]
        save_loc = "./Data/PostScores.csv"

    
    else:
        break
    
    final_data["Final"] = False
    final_data["Home_Score"],final_data["Away_Score"] = [0,0]
    final_data["Winner"] = "N/A"
    
    #Here, we clean up the data by removing the embedded table headers
    drop_list = []
    for i in range(len(raw_data)):
        selected_date = raw_data.iloc[i]["Date"]
        score = raw_data.iloc[i]["Score"]
        
        if selected_date == "Date" or pd.isnull(selected_date):
            drop_list.append(i)
            continue
            
        if pd.isnull(score) == False and score != "Score": #Mark the completed games and the future ones        
            final_data.loc[i,"Final"] = True
            
            if score[0] == "(": #Basically checks if a penalty shootout happend
                sep_scores = score.split(" ")
    
                home_pens = int(sep_scores[0].replace("(","").replace(")",""))
                away_pens = int(sep_scores[-1].replace("(","").replace(")",""))
                
                #Separate the score for shootouts
                goals = [j for j in  sep_scores if "–" in j] #NOTE: This uses "–". which is NOT "-"
                goals = goals[0].split("–")
                final_data.loc[i,"Home_Score"] = int(goals[0])
                final_data.loc[i,"Away_Score"] = int(goals[1])
                
                if home_pens > away_pens:
                    final_data.loc[i,"Winner"] = "H"
                
                elif home_pens < away_pens:
                    final_data.loc[i,"Winner"] = "A"
                
            else:
                #Separate the scores for normal games
                goals = [j for j in  score.split(" ") if "–" in j] #NOTE: This uses "–". which is NOT "-"
                goals = goals[0].split("–")
                final_data.loc[i,"Home_Score"] = int(goals[0])
                final_data.loc[i,"Away_Score"] = int(goals[1])
                
                if goals[0] > goals[1]:
                    final_data.loc[i,"Winner"] = "H"
                    
                elif goals[0] < goals[1]:
                    final_data.loc[i,"Winner"] = "A"
                    
                else:
                    final_data.loc[i,"Winner"] = "T"
                    
        if  final_data.loc[i,"Final"] == False and current_date > dt.datetime.strptime(selected_date, "%Y-%m-%d").date():
            final_data.loc[i,"Winner"] = "C"
                
    final_data = final_data.drop(drop_list)
    
    final_data.to_csv(save_loc, index = False, header=True)

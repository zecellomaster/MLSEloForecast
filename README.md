# MLSEloForecast

`MLSEloForecast` is a group of R scripts used to analyze and forecast Major League Soccer matches. 

## Installation

Simply download this repo into a location most convenient for you. It is also a good idea to install the [SoccerEloForecast](https://github.com/zecellomaster/SoccerEloForecast) package, as some scripts will require it.

## Scripts

`MLSElo.R`: Script that produces Elo ratings for all teams using `MLSMatchesRefined.csv`, which are adapted off the methodology used in the [World Football Elo Ratings](http://www.eloratings.net/about). The starting/mean Elo rating is 1500, and team ratings regress to it at the end of season (3/4 old Elo + 1/4 mean Elo).

`CalibrationTest.R`: Tests the zero-sum calibration of the produced Elo ratings. Outputs csv and can produce 45 degree line calibration plot. Note: uses environment variables, so it is best run *after* `MLSElo.R`.

`TestingGames.R`: Requires `SoccerEloForecast`. Produces the chances of a match result between to teams or simulates a user defined number of them.

`ScoreMatrix.R`: Produces likely score matrix between two teams. Note: uses environment variables, so it is best run *after* `TestingGames.R`, which should be set to return a simulated match scores (15000 recommended).

## Data

`MLS Names.csv`: A csv file that contains the full names and abbreviations used for the scripts. Also contains the 3 main colors for all active teams.

`MLSMatchesRefined.csv`: Csv file adapted from the [Major League Soccer Dataset](https://www.kaggle.com/josephvm/major-league-soccer-dataset?select=matches.csv), which was created by [Joseph Mohr](https://github.com/jvmohr). It has been adapted by removing unplayed games, sorting chronologically, and abbreviating the type of match played.

`MLS Names.csv`: Used in `MLSElo.R` to format the team Elo ratings.

`MLS Elo Histories`: Folder of csv files containing the match-by-match Elo rating of all active and defunct MLS teams. These can be changed by augmenting `MLSElo.R`.

## Future Updates

Full season + Playoff forecast script coming soon.

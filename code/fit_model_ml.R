## MODEL VERSION V1 - Literally the first try with a random forest
## trained on 2018-19

library(caret)
library(tidyverse)
library(data.table)
setwd('Documents/nba_ml_model/')
source('./code/aggregate_data.R')

version <- 'v2'
data <- aggregate_data('2018-19')

preds <- c(
  'team_days_between_games',
  'team_wins',
  'team_losses',
  'team_game_number',
  'team_points_for',
  'team_points_against',
  'team_win_perc',
  
  'opp_days_between_games',
  'opp_wins',
  'opp_losses',
  'opp_game_number',
  'opp_points_for',
  'opp_points_against',
  'opp_win_perc',
  
  'home_off_attempt_share_mid',
  'home_off_attempt_share_paint',
  'home_off_attempt_share_three',
  'home_off_made_pct_mid',
  'home_off_made_pct_paint',
  'home_off_made_pct_three',
  
  'home_def_attempt_share_mid',
  'home_def_attempt_share_paint',
  'home_def_attempt_share_three',
  'home_def_made_pct_mid',
  'home_def_made_pct_paint',
  'home_def_made_pct_three',
  
  'away_off_attempt_share_mid',
  'away_off_attempt_share_paint',
  'away_off_attempt_share_three',
  'away_off_made_pct_mid',
  'away_off_made_pct_paint',
  'away_off_made_pct_three',
  
  'away_def_attempt_share_mid',
  'away_def_attempt_share_paint',
  'away_def_attempt_share_three',
  'away_def_made_pct_mid',
  'away_def_made_pct_paint',
  'away_def_made_pct_three'
)

data$win_ind <- as.factor(data$win_ind)
to_model <- data

for (pred in preds) {
  to_model <- to_model[!is.na(to_model[, pred]), ]
  to_model <- to_model[!is.nan(to_model[, pred]), ]
}

nrow(to_model)

model <- train( 
  x = to_model[, preds],
  y = to_model$win_ind
) 

saveRDS(model, glue::glue("models/ml_rf_{version}.rds"))

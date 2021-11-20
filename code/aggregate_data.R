aggregate_data <- function(season) {
 
  source('./code/load_historical_odds.R')
  source('./code/pull_game_result_data.R')
  source('./code/pull_team_shooting_data.R')
  
  ## odds data
  odds <- load_historical_odds(season)
  
  ## game result data
  games <- pull_game_result_data(season) %>% filter(home_ind == 1)
  
  ## shooting data
  shooting <- pull_team_shooting_data(season)
  
  ## win totals
  totals <- read.csv("./data/win_totals")
  
  data <- games %>%
    inner_join(odds, by = c('team_abrev' = 'home_abrev', 'opp_abrev' = 'away_abrev', 'date' = 'date')) %>%
    inner_join(shooting, by = c('team_abrev' = 'team_abrev', 'date' = 'dateGame')) %>%
    rename(
      home_off_attempt_share_mid = off_attempt_share_mid,
      home_off_attempt_share_paint = off_attempt_share_paint,
      home_off_attempt_share_three = off_attempt_share_three,
      home_off_made_pct_mid = off_made_pct_mid,
      home_off_made_pct_paint = off_made_pct_paint,
      home_off_made_pct_three = off_made_pct_three,
      
      home_def_attempt_share_mid = def_attempt_share_mid,
      home_def_attempt_share_paint = def_attempt_share_paint,
      home_def_attempt_share_three = def_attempt_share_three,
      home_def_made_pct_mid = def_made_pct_mid,
      home_def_made_pct_paint = def_made_pct_paint,
      home_def_made_pct_three = def_made_pct_three
    ) %>%
    left_join(shooting, by = c('opp_abrev' = 'team_abrev', 'date' = 'dateGame')) %>%
    rename(
      away_off_attempt_share_mid = off_attempt_share_mid,
      away_off_attempt_share_paint = off_attempt_share_paint,
      away_off_attempt_share_three = off_attempt_share_three,
      away_off_made_pct_mid = off_made_pct_mid,
      away_off_made_pct_paint = off_made_pct_paint,
      away_off_made_pct_three = off_made_pct_three,
      
      away_def_attempt_share_mid = def_attempt_share_mid,
      away_def_attempt_share_paint = def_attempt_share_paint,
      away_def_attempt_share_three = def_attempt_share_three,
      away_def_made_pct_mid = def_made_pct_mid,
      away_def_made_pct_paint = def_made_pct_paint,
      away_def_made_pct_three = def_made_pct_three
    ) %>%
    left_join(totals, by='team_abrev') %>%
    as.data.frame()
    
  return(data)
}
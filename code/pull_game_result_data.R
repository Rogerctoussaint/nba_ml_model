
pull_game_result_data <- function(season = '2021-22', running=TRUE) {
        
  teams <- data.table::fread('./data/team_abreviations.csv')
  team_abrev <- as.character(teams$team_abrev)
  
  year <- as.integer(substring(season, 1, 4)) + 1
    
  team_results <- NULL
    
  for(i in team_abrev) {
    # have to fix cases where the basketball reference link has old names
    curr_team <- i
    if (curr_team == 'BKN') {
      curr_team <- 'BRK' 
    } 
    if (curr_team == 'CHA'){
      curr_team <- 'CHO'
    }
    if (curr_team == 'PHX') {
      curr_team <- 'PHO'
    }
    capture.output(temp <- ballr::NBASeasonTeamByYear(curr_team, year) %>% as.data.frame())
    temp$team <- i
    team_results <- team_results %>% 
      rbind(temp) %>% 
      as.data.frame()
  }
  
  ## fix dates
  team_results$date <- paste0(
    trimws(substring(team_results$date, 13, 17)),
    stringr::str_pad(match(substring(team_results$date, 6, 8), month.abb), 2, 'left', '0'),
    stringr::str_pad(trimws(gsub(',', '', substring(team_results$date, 10,12))), 2, 'left', '0') 
  )
    
  team_results <- team_results %>%
    left_join(teams, by = c('opponent' = 'team_name')) %>%
    rename(
      opp_abrev = team_abrev,
      team_abrev = team,
      team_game_number = g,
      team_score = tm,
      opp_score = opp,
      team_wins = w,
      team_losses = l,
      score_diff = diff,
      start_time = start_et,
      home_ind = away_indicator,
      team_days_between_games = daysbetweengames
    ) %>%
    mutate(
      team_abrev = as.character(team_abrev),
      opp_abrev = as.character(opp_abrev),
      home_ind = ifelse(home_ind == '@', 0, 1),
      win_ind = ifelse(team_score > opp_score, 1, 0),
      date = (date)
    )
  
  if (running == FALSE) {
    team_results <- team_results %>%
      group_by(team_abrev) %>% 
      mutate(
        team_points_for = mean(team_score, na.rm = TRUE),
        team_points_against = mean(opp_score, na.rm = TRUE)
      ) %>%
      as.data.frame()
  } else{
    team_results <- team_results %>%
      arrange(date) %>%
      group_by(team_abrev) %>% 
      mutate(
        rec = 1,
        team_points_for = (cumsum(team_score)-team_score) / (cumsum(rec)-1),
        team_points_against = (cumsum(opp_score)-opp_score) / (cumsum(rec)-1)
      ) %>%
      select(-rec) %>%
      as.data.frame()
  }
  
  team_results <- team_results %>%
    select(
      team_abrev, opp_abrev, date, start_time, home_ind, team_score, opp_score, 
      score_diff, team_days_between_games, win_ind, team_wins, team_losses, 
      team_game_number, team_points_for, team_points_against
    ) %>%
    as.data.frame()
    
  opp_results <- team_results %>%
    select(-c(home_ind, team_score, opp_score, score_diff, win_ind, start_time)) %>%
    rename(
      opp_days_between_games = team_days_between_games,
      opp_wins = team_wins,
      opp_losses = team_losses,
      opp_game_number = team_game_number,
      temp = team_abrev,
      team_abrev = opp_abrev,
      opp_points_for = team_points_for,
      opp_points_against = team_points_against
    ) %>%
    rename(opp_abrev = temp) %>%
    as.data.frame()
    
  team_results <- team_results %>%
    left_join(opp_results, by = c('team_abrev','opp_abrev','date')) %>%
    mutate(
      team_wins = ifelse(win_ind == 1, team_wins - 1, team_wins),
      team_losses = ifelse(win_ind == 1, team_losses, team_losses - 1),
      opp_wins = ifelse(win_ind == 1, opp_wins, opp_wins - 1),
      opp_losses = ifelse(win_ind == 1, opp_losses - 1, opp_losses)
    ) %>%
    group_by(team_abrev) %>% 
    mutate(
      team_wins = ifelse(is.na(team_wins), max(team_wins, na.rm = TRUE), team_wins),
      team_losses = ifelse(is.na(team_losses), max(team_losses, na.rm = TRUE), team_losses)
    ) %>% 
    as.data.frame() %>% 
    group_by(opp_abrev) %>%
    mutate(
      opp_wins = ifelse(is.na(opp_wins), max(opp_wins, na.rm = TRUE), opp_wins),
      opp_losses = ifelse(is.na(opp_losses), max(opp_losses, na.rm = TRUE), opp_losses)
    ) %>%
    as.data.frame() %>%
    mutate(
      team_win_perc = team_wins / (team_wins + team_losses),
      opp_win_perc = opp_wins / (opp_wins + opp_losses)
    ) %>%
    as.data.frame()
  
  team_results
}

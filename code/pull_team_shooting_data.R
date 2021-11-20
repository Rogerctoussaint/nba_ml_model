pull_team_shooting_data <- function(season) {

  Sys.setenv("VROOM_CONNECTION_SIZE" = 262144)
  year <- as.integer(substring(season, 1, 4)) + 1
  
  nba_team_ids <- nbastatR::nba_teams_ids(all_active_teams = TRUE)
  shot_types_base <- nbastatR::teams_shots(team_ids = nba_team_ids, seasons = year) 
    
  shot_types <- shot_types_base %>%
    mutate(
      nameTeam = ifelse(nameTeam == 'LA Clippers', 'Los Angeles Clippers', nameTeam)
    ) %>%
    left_join(df_dict_nba_teams, by = c('idTeam', 'nameTeam')) %>%
    rename(
      season = slugSeason,
      team_name = nameTeam,
      zone = zoneBasic,
      team_abrev = slugTeam
    ) %>%
    mutate(
      zone = ifelse(zone %in% c('In The Paint (Non-RA)','Restricted Area'), 
                         'paint', zone),
      zone = ifelse(zone %in% c('Above the Break 3','Left Corner 3',
                                'Right Corner 3','Backcourt'), 
                    'three', zone),
      zone = ifelse(zone %in% c('Mid-Range'), 'mid', zone)
    ) %>%
    as.data.frame()
    
  team_offense <- shot_types %>%
    group_by(team_name, team_abrev, zone, dateGame) %>%
    summarise(
      off_attempt = sum(isShotAttempted),
      off_made = sum(isShotMade)
    ) %>%
    arrange(dateGame) %>%
    mutate(
      off_attempt_cumul = cumsum(off_attempt) - off_attempt,
      off_made_cumul = cumsum(off_made) - off_made
    ) %>%
    group_by(team_name, team_abrev, dateGame) %>%
    mutate(
      off_attempt_share = off_attempt_cumul / sum(off_attempt_cumul),
      off_made_pct = off_made_cumul / off_attempt_cumul
    ) %>%
    as.data.frame()
  
  team_defense <- shot_types %>%
    mutate(
      team_abrev = ifelse(team_abrev != slugTeamHome, slugTeamHome, slugTeamAway)
    ) %>%
    group_by(team_abrev, zone, dateGame) %>%
    summarise(
      def_attempt = sum(isShotAttempted),
      def_made = sum(isShotMade)
    ) %>%
    arrange(dateGame) %>%
    mutate(
      def_attempt_cumul = cumsum(def_attempt) - def_attempt,
      def_made_cumul = cumsum(def_made) - def_made
    ) %>%
    group_by(team_abrev, dateGame) %>%
    mutate(
      def_attempt_share = def_attempt_cumul / sum(def_attempt_cumul),
      def_made_pct = def_made_cumul / def_attempt_cumul
    ) %>%
    as.data.frame()
    
  shot_types <- team_offense %>%
    left_join(team_defense, by = c('team_abrev', 'zone', 'dateGame')) %>%
    as.data.frame()
    
  ## Spread it out!
  shot_types <- shot_types %>% 
    select(-c(team_name, off_attempt, off_made, def_attempt, def_made, 
              off_attempt_cumul, off_made_cumul, def_attempt_cumul, def_made_cumul)) %>%
    pivot_wider(
      names_from = 'zone',
      values_from = c('off_attempt_share','off_made_pct','def_attempt_share','def_made_pct')
    ) %>%
    as.data.frame()
    
  return(shot_types)
}

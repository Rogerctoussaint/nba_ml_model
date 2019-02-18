pull_team_shooting_data <- function(year = 2018) {

  nba_team_ids <- nbastatR::nba_teams_ids(all_active_teams = TRUE)
  shot_types_base <- nbastatR::teams_shots(team_ids = nba_team_ids, seasons = year) %>% 
    as.data.frame()
    
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
    group_by(team_name, team_abrev, zone) %>%
    summarise(
      off_attempt = sum(isShotAttempted),
      off_made = sum(isShotMade)
    ) %>%
    group_by(team_name, team_abrev) %>%
    mutate(
      off_attempt_share = off_attempt / sum(off_attempt),
      off_made_perc = off_made / off_attempt
    ) %>%
    as.data.frame()
    
  team_defense <- shot_types %>%
    mutate(
      team_abrev = ifelse(team_abrev != slugTeamHome, slugTeamHome, slugTeamAway)
    ) %>%
    group_by(team_abrev, zone) %>%
    summarise(
      def_attempt = sum(isShotAttempted),
      def_made = sum(isShotMade)
    ) %>%
    group_by(team_abrev) %>%
    mutate(
      def_attempt_share = def_attempt / sum(def_attempt),
      def_made_perc = def_made / def_attempt
    ) %>%
    as.data.frame()
    
  shot_types <- team_offense %>%
    left_join(team_defense, by = c('team_abrev', 'zone')) %>%
    as.data.frame()
    
  ## Spread it out!
  shot_types <- shot_types %>% 
    select(-c(team_name, off_attempt, off_made, def_attempt, def_made)) %>%
    setDT() %>%
    dcast(formula = team_abrev ~ zone, 
          value.var = c('off_attempt_share','off_made_perc',
                        'def_attempt_share','def_made_perc')) %>%
    as.data.frame()
    
  shot_types
}

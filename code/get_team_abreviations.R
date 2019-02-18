df_dict_nba_teams %>% 
  filter(isNonNBATeam == 0) %>%
  select(nameTeam, slugTeam) %>%
  rename(
    team_abrev = slugTeam,
    team_name = nameTeam
  ) %>% 
  as.data.frame() %>%
  fwrite('./data/team_abreviations.csv')

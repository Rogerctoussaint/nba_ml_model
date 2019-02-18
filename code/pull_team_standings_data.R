pull_standings_data <- function(start, end) {
    
  # Helper function for pulling standings data
  standings_data_helper <- function(date) {
    
    # Pull raw standings
    temp <- NBAStandingsByDate(date) 
    
    # Set the conferences 
    temp$East$conference <- 'east'
    names(temp$East)[1] <- 'team'
    temp$West$conference <- 'west'
    names(temp$West)[1] <- 'team'
    
    standings <- temp$East %>% 
      rbind(temp$West) %>%
      mutate(date = date) %>%
      as.data.frame()
    
    standings <- standings %>% 
      mutate(
        team = ifelse(str_detect(team, '\\*'), substring(team, 1, nchar(team) - 1), team)
      ) %>%
      left_join(teams, by = c('team' = 'team_name')) %>% 
      rename(games_back = gb) %>%
      mutate(
        games_back = ifelse(games_back == '-', '0', games_back)
      ) %>%
      group_by(conference) %>% 
      mutate(
        conference_rank = row_number(),
        games_back = as.numeric(games_back),
        games_back = ifelse(is.na(games_back), 0, games_back),
        playoff_games_back = NA
      ) %>%
      select(team_abrev, date, conference, conference_rank, games_back, playoff_games_back) %>%
      as.data.frame()
    
    if(nrow(standings) == 30) {
      standings <- standings %>% 
        group_by(conference, date) %>% 
        mutate(
          playoff_games_back = games_back - games_back[conference_rank == 8]
        ) %>% 
        as.data.frame()
    }
    
    #return the standings object
    standings
  }
  
  teams <- fread('./data/team_abreviations.csv')
    
  standings <- NULL
  curr_date <- start + 1
  while(curr_date <= end) {
    temp <- standings_data_helper(curr_date)
    standings <- standings %>% rbind(temp) %>% as.data.frame()
    curr_date <- curr_date + 1
  }
  
  standings
}
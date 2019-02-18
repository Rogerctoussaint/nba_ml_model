scrape_odds <- function() {
  
  # Scrape raw odds from the website
  raw_odds <- 'http://www.donbest.com/nba/odds/money-lines/' %>%
    getURL() %>%
    read_html() %>% 
    html_nodes(xpath = '//*[@id="oddsHolder"]/div[1]/table') %>% 
    html_table(fill = TRUE) %>% 
    as.data.frame() 
  
  # Start getting meaningful data from raw odds
  info <- raw_odds[1, ]
  col_names <- raw_odds[2, ] %>% as.character()
  col_names[col_names == ''] <- 'Status'
  odds <- raw_odds[3:nrow(raw_odds), ]
  names(odds) <- col_names
    
  split_odds <- function(y) {
    if (str_count(string = y, pattern = '-')[1] == 2) {
      # Case where there are two minus signs, this is easy
      return(regexpr('-', substr(y, 2, nchar(y))))
    } else if (str_count(y, '-') == 1 & substr(y, 1, 1) != '-') {
      # Case where there is one minus sign but it's not the first character. Also easy
      return(regexpr('-', y)[1] - 1)
    } else if (str_count(y, '-') == 1 & substr(y, 1, 1) == '-') {
      # Case where there is one minus sign and it is the first character, This is a little tricky
      # as we will need to pick where one number ends and the other begins. This will be heuristic
      if (nchar(y) == 7) {
        # If the total chars is 7, we have two 3-digit odds and its easy
        return(4)
      } else if (nchar(y) == 8) {
        # This might be the hardest case. We have to decide which odds are plus, 
        # Highly unlikely, so we'll just default to NA so we don't bet this
        return(1)
      } else {
        return(1)
      }
    } else {
      # The case with double positive odds. Again, probably impossible, so this will be NA
      return(1)
    }
  }
    
  convert_odds <- function(x) {
    if (is.na(x)) {
      return(NA)
    } else {
      if (x < 0) {
        return(-x / (-x + 100))
      } else if (x > 0) {
        return(100 / (x + 100))
      } else {
        return(NA)
      }
    }
  }
  
  # Get the team abreviations from the file saved
  teams <- fread('data/team_abreviations.csv')
    
  odds_table <- odds %>% 
    rowwise() %>%
    mutate(
      team_index = regexpr('?[[:lower:]][[:upper:]]', Team),
      away_team = substr(Team, 1, team_index),
      home_team = substr(Team, team_index+1, nchar(Team)),
      open_index = split_odds(Opener),
      away_opener = ifelse(open_index > 1, as.numeric(substr(Opener, 1, open_index)), NA),
      home_opener = ifelse(open_index > 1, as.numeric(substr(Opener, open_index+1, nchar(Opener))), NA),
      away_opener_prob = convert_odds(away_opener),
      home_opener_prob = convert_odds(home_opener),
      bovada_index = split_odds(Bovada),
      away_bovada = ifelse(bovada_index > 1, as.numeric(substr(Bovada, 1, bovada_index)), NA),
      home_bovada = ifelse(bovada_index > 1, as.numeric(substr(Bovada, bovada_index+1, nchar(Bovada))), NA),
      away_bovada_prob = convert_odds(away_bovada),
      home_bovada_prob = convert_odds(home_bovada),
      bovada_margin = ifelse(!is.na(away_bovada_prob), away_bovada_prob + home_bovada_prob - 1, NA)
    ) %>% 
    select(
      away_team, home_team, away_opener, home_opener, away_opener_prob, home_opener_prob,
      away_bovada, home_bovada, away_bovada_prob, home_bovada_prob, bovada_margin
    ) %>%
    left_join(teams, by = c('away_team' = 'team_name')) %>% 
    rename(away_abrev = team_abrev) %>% 
    left_join(teams, by = c('home_team' = 'team_name')) %>%
    rename(home_abrev = team_abrev) %>%
    as.data.frame()
  
  # Return the odds table  
  odds_table
}
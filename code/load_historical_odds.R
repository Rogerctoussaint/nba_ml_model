load_historical_odds <- function(season) {
  
  ## read in data from data directory
  data <- xlsx::read.xlsx(glue::glue('./data/historical_odds/nba odds {season}.xlsx'), sheetIndex = 1)
  abbrev <- read.csv('./data/team_abreviations.csv')
  
  ## format open and closing odds to get rid of pickems for 0s
  data$Open <- ifelse(tolower(data$Open) == 'pk', '0', data$Open)
  data$Close <- ifelse(tolower(data$Close) == 'pk', '0', data$Close)
  data$Open <- as.numeric(data$Open)
  data$Close <- as.numeric(data$Close)
  
  ## handle Neutral games by just arbitrailily setting home and away
  data$neutral <- 0
  data[data$VH == 'N', 'neutral'] <- 1
  data[data$VH == 'N', 'VH'] <- rep(c('V', 'H'), nrow(data[data$VH == 'N',])/2)
  
  ## handle NAs in close by coalescing open
  data$Close <- ifelse(is.na(data$Close), data$Open, data$Close)
  
  ## create out output dataframe
  out = data.frame(
    date = data[data$VH == 'H', 'Date'],
    home_rot = data[data$VH == 'H', 'Rot'],
    away_rot = data[data$VH == 'V', 'Rot'],
    home = data[data$VH == 'H', 'Team'],
    away = data[data$VH == 'V', 'Team'],
    
    home_q1 = data[data$VH == 'H', 'X1st'],
    home_q2 = data[data$VH == 'H', 'X2nd'],
    home_q3 = data[data$VH == 'H', 'X3rd'],
    home_q4 = data[data$VH == 'H', 'X4th'],
    
    away_q1 = data[data$VH == 'V', 'X1st'],
    away_q2 = data[data$VH == 'V', 'X2nd'],
    away_q3 = data[data$VH == 'V', 'X3rd'],
    away_q4 = data[data$VH == 'V', 'X4th'],
    
    home_final = data[data$VH == 'H', 'Final'],
    away_final = data[data$VH == 'V', 'Final'],
    
    home_ml = data[data$VH == 'H', 'ML'],
    away_ml = data[data$VH == 'V', 'ML'],
  
    total = data[data$Close >= 100, 'Close'],
    spread = data[data$Close < 100, 'Close']
  )
 
  ## adjust spread to be relative to home team 
  out$spread <- ifelse(out$home_ml <= out$away_ml, out$spread * -1, out$spread)
  
  ## add home abreviation
  out <- dplyr::left_join(out, abbrev, by=c('home' = 'odds_name'))
  out$home_abrev <- out$team_abrev
  out <- out[, names(out) != 'team_name' & names(out) != 'team_abrev']
  
  ## add away abreviation
  out <- dplyr::left_join(out, abbrev, by=c('away' = 'odds_name'))
  out$away_abrev <- out$team_abrev
  out <- out[, names(out) != 'team_name' & names(out) != 'team_abrev']
  
  ## format date as a date
  out$date <- stringr::str_pad(out$date, 4, 'left', '0')
  out$month <- (substring(out$date, 1, 2))
  out$day <- (substring(out$date, 3, 4))
  out$year <- as.integer(substring(season, 1, 4))
  out$year <- ifelse(out$month < 10, out$year + 1, out$year)
  out$date <- paste0(out$year, out$month, out$day)
  out <- out[, names(out) != 'year' & names(out) != 'month' & names(out) != 'day']
  
  return(out)
}

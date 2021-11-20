back_test <- function(model_path, season, type, bankroll = 1000, kelly = 0.25, thresh = 0) {
  source('./code/aggregate_data.R')
  model <- readRDS(model_path)
  
  x <- capture.output(suppressMessages(data <- aggregate_data(season)))
  
  for (pred in predictors(model)) {
    data <- data[!is.na(data[, pred]), ]
    data <- data[!is.nan(data[, pred]), ]
  }
  
  if {type == 'ml') {
    data$model_pred <- -as.numeric(predict(model, data, type = 'prob')[[2]])
  
    out <- data %>%
      mutate(
        home_implied = ifelse(
          test = home_ml < 0,
          yes = -home_ml / (-home_ml + 100),
          no = 100 / (home_ml + 100)
        ),
        home_decimal = ifelse(
          test = home_ml < 0,
          yes = (100/-home_ml) + 1,
          no = (home_ml/100) + 1
        ),
        away_decimal = ifelse(
          test = away_ml < 0,
          yes = (100/-away_ml) + 1,
          no = (away_ml/100) + 1
        )
      ) %>%
      as.data.frame()
    
    out$bet_side <- ifelse(out$model_pred > out$home_implied, 'home', 'away')
    out$bet_odds <- ifelse(out$bet_side == 'home', out$home_decimal, out$away_decimal)
    out$bet_win_pct <- ifelse(out$bet_side == 'home', out$model_pred, 1 - out$model_pred)
    
    out$bet_win <- ifelse(
      test = (out$bet_side == 'home' & out$win_ind == 1) | (out$bet_side == 'away' & out$win_ind == 0),
      yes = 1,
      no = 0
    )
  } else if (type == 'spread') {
    data$model_pred <- -as.numeric(predict(model, data, type = 'raw'))
    out <- data
    out$bet_side <- ifelse(out$model_pred < out$spread, 'home', 'away')
    out$bet_odds <- (100/110) + 1 #-110
    out$bet_win_pct <- ifelse(
      test = out$bet_side == 'home',
      yes = pnorm(out$spread, out$model_pred, 13),
      no = 1 - pnorm(out$spread, out$model_pred, 13) 
    )
    
    out$bet_win <- ifelse(
      test = (out$bet_side == 'home' & -out$score_diff < out$spread) | 
        (out$bet_side == 'away' & -out$score_diff > out$spread),
      yes = 1,
      no = 0
    )
  }
  
  out$bet_amount <- (kelly * (out$bet_odds*out$bet_win_pct - (1-out$bet_win_pct)) / out$bet_odds) * bankroll
  out$win_amount <- out$bet_win * out$bet_amount * out$bet_odds
  
  ## apply threshold
  #out$diff <- abs(out$model_pred - out$home_implied)
  #out <- out[out$diff >= thresh, ]
  
  print(glue::glue("Back Test on {season} Season! Using a threshold of {thresh}"))
  print(paste0("          Games Bet: ", nrow(out)))
  print(paste0("Beg Account Balance: $", bankroll))
  print(paste0("End Account Balance: $", round(bankroll + sum(out$win_amount) - sum(out$bet_amount), 2)))
  print(paste0("             Return: ",
               round((((sum(out$win_amount) - sum(out$bet_amount) + bankroll) / bankroll) - 1) * 100, 2), '%'))
  
  out$curr_earnings <- cumsum(out$win_amount - out$bet_amount)
  
  return(
    out[, c('team_abrev', 'opp_abrev', 'team_score', 'opp_score', 'win_ind', 'model_pred', 'home_ml', 'away_ml', 
            'home_implied','home_decimal', 'away_decimal','bet_side', 'bet_odds', 'bet_win_pct', 'bet_amount',
            'bet_win', 'win_amount', 'curr_earnings')]
  )
}
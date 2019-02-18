#### ---- Load dependencies ---- ####
if(!require(tidyverse)) install.packages('tidyverse'); require(tidyverse)
if(!require(data.table)) install.packages('data.table'); require(data.table)
if(!require(ballr)) install.packages('ballr'); require(ballr)
if(!require(nbastatR)) install.packages('nbastatR'); require(nbastatR)
if(!require(ggplot2)) install.packages('ggplot2'); require(ggplot2)
if(!require(h2o)) install.packages('h2o'); require(h2o)
if(!require(rvest)) install.packages('rvest'); require(rvest)
if(!require(XML)) install.packages('XML'); require(XML)
if(!require(RCurl)) install.packages('RCurl'); require(RCurl)
if(!require(magrittr)) install.packages('magrittr'); require(magrittr)
if(!require(lubridate)) install.packages('lubridate'); require(lubridate)

# Set filter to the dplyr function
filter <- dplyr::filter

#### ---- Load our functions ---- ####
source('./code/scrape_odds.R')
source('./code/pull_game_result_data.R')
source('./code/pull_team_shooting_data.R')

#### ---- Load NBA dependent datasets ---- ####
nbastatR::assign_nba_teams()
nbastatR::assign_nba_players()
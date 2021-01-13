library(steamR)
library(purrr)
library(lubridate)
library(dplyr)
library(rjson)
library(magrittr)
library(tidyr)
library(XML)
library(httr)
library(stringr)

STEAM_KEY = "DE3A03C2C78332D35FAA60E469DA1C4A"

games_dfs = data.frame()
pl_summary_dfs = data.frame()

for (id in authors_gather$steamid){
  tryCatch({
    id_see = id
    owned_games = get_owned_games(STEAM_KEY, id, include_appinfo = TRUE, include_played_free_games = TRUE)
    if(!is.null(owned_games)){
      games_df = owned_games %>% map(flatten) %>% {
         tibble(appid=map(.,"appid"),
                name=map(.,"name"), 
                playtime_forever=map(.,"playtime_forever"))}
      games_df$playtime_2weeks = owned_games %>%
        map(c("playtime_2weeks")) %>%
        flatten()
      games_df$user_id = id
      games_dfs = rbind(games_dfs,games_df)
    }
    player_summary = get_player_summaries(STEAM_KEY, id)
    if(!is.null(player_summary)){
    pl_summary_df = player_summary %>% map(flatten) %>% {
       tibble(steamid=map(.,"steamid"),
             personaname=map(.,"personaname"), 
             timecreated=map(.,"timecreated"),
             personastate=map(.,"personastate"),
             communityvisibilitystate=map(.,"communityvisibilitystate"),
             primaryclanid=map(.,"primaryclanid"),
             loccountrycode=map(.,"loccountrycode"),
             locstatecode=map(.,"locstatecode"))}
    pl_summary_df$realname = player_summary %>%
      map(c("realname")) %>%
      flatten()
    pl_summary_dfs = rbind(pl_summary_dfs,pl_summary_df)
  }
    Sys.sleep(0.1)}
  , error = function(e) e)
}
      
      

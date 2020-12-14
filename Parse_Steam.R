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


STEAM_KEY = "C04E7171D6207236CA5A1D6C2B333943"


#цикл на айдишки с групп
# Задаем группу,которую собираем
group_members = data.frame()
for(i in 83:144){
  print(i)
  begin = "http://steamcommunity.com/groups/DotaX2Official/memberslistxml/?xml=1&p="
  url = str_c(begin,i)
  group_info = xmlTreeParse(rawToChar(GET(url)$content)) %>% xmlToList()
  
  df = data.frame(group_id=group_info$groupID64,user_id=map_chr(group_info$members,1))
  group_members=rbind(group_members,df)
  Sys.sleep(3)
}


# player's personal info
player_summary = get_player_summaries(STEAM_KEY, USER_ID)
pl_summary_df = player_summary %>%
  map_df(magrittr::extract,c("steamid","personaname","lastlogoff","realname","timecreated")) %>% 
  mutate(lastlogoff=as.POSIXct(lastlogoff,origin="1970-01-01"))


# player's friend list
friends_list = get_friend_list(STEAM_KEY, USER_ID,"all")
friends_df = friends_list %>%
  map_df(magrittr::extract,c("steamid","relationship","friend_since")) %>%
  mutate(friend_since = as.POSIXct(friend_since,origin="1970-01-01"))


# player's games
owned_games = get_owned_games(STEAM_KEY, USER_ID, include_appinfo = TRUE, include_played_free_games = FALSE)
games_df = owned_games %>% map_df(magrittr::extract,c("appid","name","playtime_forever","has_community_visible_stats")) %>% 
  mutate(playtime_forever=seconds_to_period(playtime_forever*60))


#цикл на друзей и игры пользователей 
friends_dfs = data.frame()
games_dfs = data.frame()
for (id in group_members$user_id[1:49000]){
  tryCatch({
    id_see = id
    friends_list = get_friend_list(STEAM_KEY, id,"all")
    if(!is.null(friends_list)){
      friends_df = friends_list %>%
        map_df(magrittr::extract,c("steamid","relationship","friend_since")) %>%
        mutate(friend_since = as.POSIXct(friend_since,origin="1970-01-01"))
      friends_df$user_id = id
      friends_dfs = rbind(friends_dfs,friends_df)
    }
    owned_games = get_owned_games(STEAM_KEY, id, include_appinfo = TRUE, include_played_free_games = FALSE)
    if(!is.null(owned_games)){
      games_df = owned_games %>% map_df(magrittr::extract,c("appid","name","playtime_forever","has_community_visible_stats")) %>% mutate(playtime_forever=seconds_to_period(playtime_forever*60))
      games_df$user_id = id
      games_dfs = rbind(games_dfs,games_df)
    }
    Sys.sleep(0.1)}
    , error = function(e) e)
}

library(purrr)
library(lubridate)
library(dplyr)
library(rjson)
library(magrittr)
library(tidyr)
library(XML)
library(httr)
library(stringr)

authors_dfs = data.frame()

for (i in meta_2017$appid){
  tryCatch({req=paste0("http://store.steampowered.com/appreviews/",i,"?json=1&day_range=9223372036854775807&filter=all&review_type=all&purchase_type=all&language=english&num_per_page=100&cursor=*")
  json = rjson::fromJSON(file=req)
  print(req)
  if(!is.null(json)){
    c = json[["cursor"]]
    c = URLencode(c, reserved = TRUE)
    df = json[[3]] %>% map(flatten) %>% {
      tibble(steamid=map(.,"steamid"),
             num_games_owned=map(.,"num_games_owned"), 
             num_reviews=map(.,"num_reviews"),
             playtime_forever=map(.,"playtime_forever"),
             playtime_last_two_weeks=map(.,"playtime_last_two_weeks"),
             last_played=map(.,"last_played"))}
    authors_dfs = rbind(authors_dfs, df)
    
    for (k in 1:19){req1 = paste0("http://store.steampowered.com/appreviews/",i,"?json=1&day_range=9223372036854775807&filter=all&review_type=all&purchase_type=all&language=english&num_per_page=100&cursor=", c)
    k <- req1
    print(req1)
    json = rjson::fromJSON(file=req1)
    c = json[["cursor"]]
    c = URLencode(c, reserved = TRUE)
    df = json[[3]] %>% map(flatten) %>% {
      tibble(steamid=map(.,"steamid"),
             num_games_owned=map(.,"num_games_owned"), 
             num_reviews=map(.,"num_reviews"),
             playtime_forever=map(.,"playtime_forever"),
             playtime_last_two_weeks=map(.,"playtime_last_two_weeks"),
             last_played=map(.,"last_played"))}
    authors_dfs = rbind(authors_dfs, df)
    }}
  Sys.sleep(0.5)
  }
  , error = function(e) e)
}

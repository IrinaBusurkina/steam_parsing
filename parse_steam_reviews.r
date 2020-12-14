reviews_dfs = data.frame()

for (i in games_id$appid){
tryCatch({req=paste0("http://store.steampowered.com/appreviews/",i,"?json=1&day_range=9223372036854775807&filter=all&review_type=all&purchase_type=all&language=english&num_per_page=100&cursor=*")
json = rjson::fromJSON(file=req)
print(req)
if(!is.null(json)){
c = json[["cursor"]]
c = URLencode(c, reserved = TRUE)
df = json[[3]] %>%
  map_df(magrittr::extract,c("recommendationid","language","review","timestamp_created","timestamp_updated","voted_up", "votes_up","votes_funny","comment_count","comment_count","steam_purchase","received_for_free","written_during_early_access" )) %>%
  mutate(timestamp_created = as.POSIXct(timestamp_created,origin="1970-01-01")) %>%
  mutate(timestamp_updated = as.POSIXct(timestamp_updated,origin="1970-01-01")) %>%
  mutate(link = req)
reviews_dfs = rbind(reviews_dfs,df)

for (k in 1:19){req1 = paste0("http://store.steampowered.com/appreviews/",i,"?json=1&day_range=9223372036854775807&filter=all&review_type=all&purchase_type=all&language=english&num_per_page=100&cursor=", c)
k <- req1
print(req1)
json = rjson::fromJSON(file=req1)
c = json[["cursor"]]
c = URLencode(c, reserved = TRUE)
  df = json[[3]] %>%
    map_df(magrittr::extract,c("recommendationid","language","review","timestamp_created","timestamp_updated","voted_up", "votes_up","votes_funny","comment_count","comment_count","steam_purchase","received_for_free","written_during_early_access" )) %>%
    mutate(timestamp_created = as.POSIXct(timestamp_created,origin="1970-01-01")) %>%
    mutate(timestamp_updated = as.POSIXct(timestamp_updated,origin="1970-01-01")) %>%
    mutate(link = req1)
  reviews_dfs = rbind(reviews_dfs,df)}}
Sys.sleep(1)
}
, error = function(e) e)
}
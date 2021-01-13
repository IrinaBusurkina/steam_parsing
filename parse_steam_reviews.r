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

#get json with the appids from the steam api
library(rjson)
json_file <- "http://api.steampowered.com/ISteamApps/GetAppList/v2/"
json_data <- fromJSON(f = json_file,unexpected.escape = "skip")
applist <- json_data$applist$apps
data <- applist %>% map_df(magrittr::extract,c("appid","name"))

#join appids with game names
# but firstly remove all punctuation 
data$name <- tolower(data$name)
data$name <- str_replace_all(data$name, "[[:punct:]]", "")
data$name <- str_replace_all(data$name, "™", "")
data$name <- str_replace_all(data$name, "®", "")
data$name <- str_replace_all(data$name, "©", "")
data$name <- str_trim(data$name, side = "both")
data$name <- str_remove_all(data$name,"[:space:]{2,}")

meta_2017$name <- tolower(meta_2017$name)
meta_2017$name <- str_replace_all(meta_2017$name, "[[:punct:]]", "")
meta_2017$name <- str_trim(meta_2017$name, side = "both")
meta_2017$name <- str_remove_all(meta_2017$name,"[:space:]{2,}")

#meta_2018$name <- str_replace_all(meta_2018$name, "guns gorecannoli 2", "guns gore and cannoli 2")
#meta_2017$name <- str_replace_all(meta_2017$name, "divinity original sin ii", "divinity original sin 2")
#meta_2017$name <- str_replace_all(meta_2017$name, "planescape tormentenhanced edition", "planescape torment enhanced edition")
#data$name <- str_replace_all(data$name, "dark souls iiithe ringed city", "dark souls iii the ringed city")

meta_2017 = meta_2017[,-9]
#colnames(meta_2019)[9] = "appid"

meta_2019 <- left_join(meta_2019, data, by = 'name')
meta_2018 <- left_join(meta_2018, data, by = 'name')
meta_2017 <- left_join(meta_2017, data, by = 'name')

#find missing appids
meta_2017$missing <- is.na(meta_2017$appid)
missing <- meta_2018 %>% filter(missing == TRUE)
meta_2017 <- meta_2017[!duplicated(meta_2017),]

meta_2019 = na.omit(meta_2019)

reviews_dfs = data.frame()

for (i in meta_2019$appid){
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
  
  df$steamid = json[[3]] %>% 
      map_chr(c("author","steamid"))
  
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
                
    df$steamid = json[[3]] %>% 
      map_chr(c("author","steamid"))
                
  reviews_dfs = rbind(reviews_dfs,df)}}
Sys.sleep(1)
}
, error = function(e) e)
}

library(rvest)
library(dplyr)
library(stringr)
library(lubridate)

# Scrape info about :
# 1. Game title
# 2. Metascore : The metascore of the game, given by critics
# 3. User Score : The score of the game, based on user ratings
# 4. Links: Links to the separate web page of the game
# 5. Release Date : The date of the game released into market
# 6. Genre

url = 'https://www.metacritic.com/game/pc/into-the-breach'

# 2019 
URL <- "https://www.metacritic.com/browse/games/score/metascore/year/pc/filtered?view=condensed&year_selected=2019"

# 2018
URL <- "https://www.metacritic.com/browse/games/score/metascore/year/pc/filtered?view=condensed&year_selected=2018"

# 2017
URL <- "https://www.metacritic.com/browse/games/score/metascore/year/pc/filtered?view=condensed&year_selected=2017"

webpage <- read_html(URL)

#create the function to scrape the top games
metacritic_scrape <- function(URL){
#get the title
game_name <- webpage %>% 
  html_nodes("a.title") %>% 
  html_text() %>% 
  str_remove_all("\n") %>% 
  str_trim(side = "both") %>% 
  str_remove("[:space:]{2,}") %>% 
  str_remove("[:digit:]{1,}\\.")

#get the critic score
critic_score <- webpage %>% 
  html_nodes("td.score") %>% 
  html_text() %>% 
  as.numeric()

# get the user score
user_score <- URL %>%
  read_html() %>% 
  html_nodes(xpath = '//*[@class="score title"]/a/div') %>%  
  html_text()

# get the links
url <- URL %>%
  read_html() %>% 
  html_nodes(xpath = '//*[@class="title"]') %>%  
  html_attr("href") %>% 
  na.omit() 

meta <- data.frame(game_name, critic_score, user_score, url)
}

# assign data to the new dataframe
meta_2017 <- metacritic_scrape(URL)
meta_2017$links <- str_c("https://www.metacritic.com", meta_2017$links)
head(meta_2017)


# loop for games in the url column
dfs = data.frame() 
for (url in meta_2018$links){
  genre <- url %>%
    read_html() %>% 
    html_nodes(xpath = '//*[@class="summary_detail product_genre"]') %>%  
    html_text() %>% 
    str_remove_all("\n") %>% 
    str_trim(side = "both") %>% 
    str_remove_all("[:space:]{2,}") %>% 
    str_remove_all("[:digit:]{1,}\\.") %>% 
    str_remove_all("Genre\\(s\\): ") 
    
  developer <- url %>% 
    read_html() %>% 
    html_nodes(xpath = '//*[@class="summary_detail developer"]') %>% 
    html_text() %>% 
    str_remove_all("\n") %>% 
    str_remove_all("Developer: ") %>%
    str_trim(side = "both")
  
  publisher <- url %>% 
    read_html() %>% 
    html_nodes(xpath = '//*[@class="summary_detail publisher"]') %>% 
    html_text() %>% 
    str_remove_all("\t\t\t\t\t\t\t\t\t\t\t") %>% 
    str_remove_all("\n") %>% 
    str_remove_all("Publisher: ") %>%
    str_trim(side = "both")
  
  release_date <- url %>% 
    read_html() %>%
    html_nodes(xpath = '//*[@class="summary_detail release_data"]') %>% 
    html_text() %>% 
    str_remove_all("\n") %>% 
    str_remove_all("Release Date: ") %>%
    str_trim(side = "both")
  
  df = data.frame(url = url, genre = genre, developer = developer, publisher = publisher, release_date = release_date)
  dfs = rbind(dfs,df)
  Sys.sleep(0.5)
}

dfs$release_date <- mdy(dfs$release_date)
colnames(meta_2018)[4] <- "url"
meta_2018 <- left_join(meta_2018, dfs, by = "url")

write_csv(meta_2018, "meta_2018.csv")
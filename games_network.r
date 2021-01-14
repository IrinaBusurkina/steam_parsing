library(dplyr)
library(stringr)
library(tidyverse)
library(tidytext)
library(readr)


load("~/Reviews/data/stm_models/meta_stm_110.rda")
meta <- dplyr::select(meta, recommendationid, text, appid)
meta$text <- str_replace_all(meta$text, "[^[:alnum:]]", " ")
meta$text <- str_squish(meta$text)

rating <- read.csv("~/Reviews/Video_Game_Sales_as_of_Jan_2017.csv")
rating <- na.omit(rating)

rating$Year_of_Release <- as.character(rating$Year_of_Release)
rating$Year_of_Release <- as.numeric(rating$Year_of_Release)

#mean score. and min year for all platforms
rating1 <- rating %>% 
  group_by(Name) %>% 
  summarise(score_mean = mean(User_Score), year = min(Year_of_Release)) 
rating1$score_mean = round(rating1$score_mean, digits = 2)


#preproc of games' names
rating1$Name <- as.character(rating1$Name)
rating1$Name <- str_to_lower(rating1$Name)
rating1$Name <- str_replace_all(rating1$Name, "[^[:alnum:]]", " ") # deleting all punctuation
rating1$Name <- str_trim(rating1$Name, side = c("both"))
rating1$Name <- str_squish(rating1$Name)

#1 word games extraction - ok, but above 2 words
new_gr1 <- meta %>% unnest_tokens(words, text)
new_gr1 <- new_gr1 %>% filter(words %in% rating1$Name)
library(qdapRegex)
new_gr1$words <- rm_nchar_words(new_gr1$words, "1,3")
new_gr1$words <- str_squish(new_gr1$words)
new_gr1$words[str_length(new_gr1$words)==0] <- NA
new_gr1 = na.omit(new_gr1)

gr = new_gr7 %>% 
  group_by(words) %>% 
  summarise(n = n())

gr = gr %>% filter(n > 10)

new_gr1 <- new_gr1 %>% filter(words %in% gr$words)

# 2 words in name
new_gr2 <- meta %>% unnest_tokens(words, text, token = "ngrams", n = 2)
new_gr2 <- new_gr2 %>% filter(words %in% rating1$Name)

# 3 words in name
new_gr3 <- meta %>% unnest_tokens(words, text, token = "ngrams", n = 3)
new_gr3 <- new_gr3 %>% filter(words %in% rating1$Name)

# 4 words in name
new_gr4 <- meta %>% unnest_tokens(words, text, token = "ngrams", n = 4)
new_gr4 <- new_gr4 %>% filter(words %in% rating1$Name)

# 5 words in name - it is max
new_gr5 <- meta %>% unnest_tokens(words, text, token = "ngrams", n = 5)
new_gr5 <- new_gr5 %>% filter(words %in% rating1$Name)

# 6 words in name 
new_gr6 <- meta %>% unnest_tokens(words, text, token = "ngrams", n = 6)
new_gr6 <- new_gr6 %>% filter(words %in% rating1$Name)

# 7 words in name 
new_gr7 <- meta %>% unnest_tokens(words, text, token = "ngrams", n = 7)
new_gr7 <- new_gr7 %>% filter(words %in% rating1$Name)

# 8 words in name 
new_gr8 <- meta %>% unnest_tokens(words, text, token = "ngrams", n = 8)
new_gr8 <- new_gr8 %>% filter(words %in% rating1$Name)


# we unite all names in one dataset
new_united <- rbind(new_gr1, new_gr2)
new_united <- rbind(new_united, new_gr3)
new_united <- rbind(new_united, new_gr4)
new_united <- rbind(new_united, new_gr5)
new_united <- rbind(new_united, new_gr6)
new_united <- rbind(new_united, new_gr7)
new_united <- rbind(new_united, new_gr8)


load("~new_untitled.rda")

#filter origin games by the dataset with ratings
g2$name <- as.character(g2$name)
g2$name <- str_to_lower(g2$name)
g2$name <- str_replace_all(g2$name, "[^[:alnum:]]", " ") # deleting all punctuation
g2$name <- str_trim(g2$name, side = c("both"))
g2$name <- str_squish(g2$name)

g3 <- g2 %>% filter(name %in% rating1$Name)

#merge with game names and other stuff
new_united1 <- left_join(new_united, g3, by = "appid")
new_united1 <- dplyr::select(new_united1, -count)
new_united1 = na.omit(new_united1)

names(new_united1)[4] = "game_origin"
names(new_united1)[3] = "game_review"
names(new_united1)[2] = "appid_game_origin"

new_united1 <- new_united1[!duplicated(new_united1), ]

occurence <- new_united1 %>% 
  group_by(game_origin, game_review) %>% 
  summarise(count = n())

occurence <- occurence %>% filter(count > 3)

#network dataframe
net = data.frame(
  game_origin = as.character(occurence$game_origin),
  game_review = as.character(occurence$game_review),
  stringsAsFactors = FALSE
)

#directed graph
g = graph_from_data_frame(net, directed = TRUE)
g = igraph::simplify(g, remove.multiple = TRUE, remove.loops = TRUE)

degree <- degree(g, v = V(g), mode = c("all"))
g=delete_vertices(g,degree < 1)
g=delete_vertices(g,degree1 > 23)


#assigning the right attributes
vertex_attr <- vertex_attr(g)
vertex_attr = as.data.frame(vertex_attr)

vertex_attr$name = as.character(vertex_attr$name)

names(rating1)[1] = "name"
rating1$name = as.character(rating1$name)

vertex_attr <- left_join(vertex_attr, rating1, by = "name")

V(g)$rating = vertex_attr$score_mean
V(g)$year = vertex_attr$year


#visualize
library(visNetwork)
netvis1 <- toVisNetworkData(g)
netvis1$nodes$title = netvis1$nodes$label

visNetwork(nodes = netvis1$nodes, edges = netvis1$edges, height = "800px", width = "1600px") %>% 
  visIgraphLayout() %>% 
  visEdges(arrows ="to")


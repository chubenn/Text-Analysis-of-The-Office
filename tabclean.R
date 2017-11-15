library(haven)
library(tidyverse)
library(tidytext)
library(Rserve)
library(tidyr)
library(ggplot2)
library(viridis)
library(widyr)
library(igraph)
library(ggraph)
library(tm)

remover <- tibble(text = stopwords("SMART")) #use this one
remover <- tibble(text = stopwords("en"))
bing <- sentiments %>% filter(lexicon == "bing") %>% select(-score, -lexicon)
nrc <- sentiments %>% filter(lexicon == "nrc") %>% select(-score, -lexicon)

#creation of OGdata
original_office <- read_csv("GitHub/office-lover/the-office-lines - scripts.csv") %>% 
  group_by(season, episode) %>%
  mutate(line_said = row_number(),
         season1 = season,
         episode1 = episode) %>%
  unite(seasepi,season1,episode1, sep =  "-")

#tidy text
tidy_words <- original_office %>% 
  unnest_tokens(text, line_text)%>% 
  anti_join(remover)


#generic counts by words
tidy_count <- tidy_words %>% 
  group_by(text,speaker,season,episode) %>% 
  count(text,sort = TRUE) %>%
  ungroup()

#sent text
tidy_sent <- tidy_words %>% 
  inner_join(bing, by = c("text" = "word")) %>%
  inner_join(nrc, by = c("text" = "word")) %>%
  group_by(speaker) %>%
  mutate(speaker_total = n()) %>%
  ungroup() %>%
  group_by(sentiment.x) %>%
  mutate(sentiment.x_total = n()) %>%
  ungroup() %>%
  group_by(sentiment.y) %>%
  mutate(sentiment.y_total = n()) %>%
  ungroup() %>%
  group_by(text) %>%
  mutate(text_total = n()) %>%
  ungroup() %>% 
  group_by(speaker) %>%
  mutate(speaker_total = n()) %>%
  ungroup() %>%
  group_by(season, speaker) %>%
  mutate(speaker_season_total = n()) %>%
  ungroup()

#sent text
tidy_sent <- tidy_words %>% 
  left_join(nrc, by = c("text" = "word")) %>%
  group_by(speaker) %>%
  mutate(speaker_total = n()) %>%
  ungroup() %>%
  group_by(text) %>%
  mutate(text_total = n()) %>%
  ungroup() %>% 
  group_by(season) %>%
  mutate(season_total = n()) %>%
  ungroup() %>%
  group_by(season, speaker) %>%
  mutate(speaker_season_total = n()) %>%
  ungroup() %>%
  group_by(season,text) %>%
  mutate(season_text = n()) %>%
  ungroup() %>%
  group_by(speaker,text) %>%
  mutate(speaker_text = n()) %>%
  ungroup() %>%
  group_by(season, speaker, text) %>%
  mutate(speaker_season_text_total = n()) %>%
  ungroup() %>%
  group_by(sentiment) %>%
  mutate(sentiment_total = n()) %>%
  ungroup() %>%
  group_by(sentiment, speaker) %>%
  mutate(sentiment_speaker_total = n()) %>%
  ungroup() %>%
  group_by(sentiment, season) %>%
  mutate(sentiment_season_total = n()) %>%
  ungroup() %>%
  group_by(sentiment, text) %>%
  mutate(sentiment_text_total = n()) %>%
  ungroup() %>%
  group_by(sentiment, speaker, season) %>%
  mutate(sentiment_speaker_season_total = n()) %>%
  ungroup() %>%
  group_by(sentiment, speaker,text) %>%
  mutate(sentiment_speaker_text_total = n()) %>%
  ungroup() %>%
  group_by(sentiment, season, text) %>%
  mutate(sentiment_season_text_total = n()) %>%
  ungroup() %>%
  group_by(sentiment, season, speaker, text) %>%
  mutate(sentiment_season_speaker_text_total = n()) %>%
  ungroup()

write_csv(tidy_sent , "officefull.csv")

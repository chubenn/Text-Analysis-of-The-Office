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

write_csv(speaker_sent, "officesentimentals.csv")
write_csv(tidy_office , "officefull.csv")
write_csv(sent_count_words , "officecounts.csv")

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
count_words <- tidy_words %>% 
  group_by(text,speaker,season, episode) %>% 
  count(text,sort = TRUE) %>%
  ungroup()

#sent text
sent_count_words <- count_words %>% 
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
  ungroup()

#just_text  
text_tidy_office <- tidy_words %>% 
  inner_join(bing, by = c("text" = "word")) %>%
  group_by(speaker) %>%
  mutate(speaker_total = n()) %>%
  ungroup() %>%
  group_by(text) %>%
  mutate(text_total = n()) %>%
  ungroup()

#lookin at words
word_count <- tidy_words %>%
  inner_join(bing, by = c("text" = "word")) %>%
  count(text, sentiment, sort = TRUE) %>%
  group_by(sentiment) %>%
  top_n(50) %>%
  ungroup() %>%
  mutate(text = reorder(text,n))

#lookin at words
word_count <- tidy_words %>%
  inner_join(bing, by = c("text" = "word")) %>%
  count(text, sentiment, sort = TRUE) %>%
  group_by(sentiment) %>%
  top_n(50) %>%
  ungroup() %>%
  mutate(text = reorder(text,n))

#plotted
ggplot(word_count, aes(text, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free") +  
  coord_flip()

#plotted
ggplot(word_count, aes(text, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free") +  
  coord_flip()

#analysis
tidy_words %>% 
  inner_join(nrc, by = c("text" = "word")) %>%
  count(season, index = line_said %/% 90, sentiment) %>%
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive - negative)

#differences by speaker
speaker_sent <- tidy_words %>%
  group_by(speaker) %>%
  mutate(speaker_total = n()) %>%
  ungroup() %>%
  inner_join(nrc, by = c("text" = "word"))
#nice people
nice_people <- speaker_sent %>%
  count(speaker, sentiment, speaker_total) %>%
  mutate(percent = n / speaker_total) %>%
  filter(n > 100) %>%
  filter(sentiment == "positive") %>%
  arrange(percent)
#naughty people
naughty_people <- speaker_sent %>%
  count(speaker, sentiment, speaker_total) %>%
  mutate(percent = n / speaker_total) %>%
  filter(n > 100) %>%
  filter(sentiment == "negative") %>%
  arrange(percent)
#what makes up emotions?
speaker_sent %>%
  count(text, sentiment) %>%
  group_by(sentiment) %>%
  top_n(10, n) %>%
  ungroup() %>%
  mutate(text = reorder(text, n)) %>%
  ggplot(aes(text, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ sentiment, scales = "free") +
  coord_flip()

#pair count
pairwise <- tidy_words%>% 
  filter(season == 1) %>%
  pairwise_count(text,line_said, sort = TRUE)

pairwise %>%
  filter (n > 10) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +  
  geom_edge_link(aes(edge_alpha = n, edge_width = n)) +
  geom_node_point(color = "plum4", size = 5) +
  geom_node_text(aes(label = name), vjust = 1.8) +
  ggtitle(expression(paste(" test ", 
                           italic("why god")))) +
  theme_void()

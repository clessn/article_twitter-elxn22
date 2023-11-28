#########################
####Ce script vise à trouver le top 50 des mots pour chacun de nos corpus####
#########################

library(tidyverse)
library(tm)
library(tidytext)

tweets_texts <- readRDS("/Users/jeremiedrouin/Dropbox/Travail/Universite_Laval/CLESSN/Publications/article_twitter-elxn22/_SharedFolder_article_twitter-elxn22/Data/tweetstext.rds") %>% 
  select(data.text)

colnames(tweets_texts) <- c("doc_id", "text")
datatweets_source <- DataframeSource(tweets_texts)
datatweets_corpus <- VCorpus(datatweets_source)

str(tweets_texts)

media_texts <- readRDS("/Users/jeremiedrouin/Dropbox/Travail/Universite_Laval/CLESSN/Publications/article_twitter-elxn22/_SharedFolder_article_twitter-elxn22/Data/MediaClean_2023-11-21.rds") %>% 
  select(doc_id, text)
colnames(media_texts) <- c("doc_id", "text")


datatweets_source <- DataframeSource(tweets_texts, colnames = c("metadata.twitterHandle", "data.text"))
datatweets_corpus <- VCorpus(datatweets_source)

datamedia_source <- DataframeSource(media_texts)
datamedia_corpus <- VCorpus(datamedia_source)






clean_corpusFR <- function(corpus){
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, stopwords("fr"))
  #corpus <- tm_map(corpus, removeWords, words = stopWords_en)
  corpus <- tm_map(corpus, removePunctuation, preserve_intra_word_dashes = T)
  corpus <- tm_map(corpus, removeNumbers)
  # corpus <- tm_map(corpus, stemDocument) # stemming SEULEMENT pour le topic modeling
  # corpus <- tm_map(corpus, removeWords, words = keywords)
  #corpus <- tm_map(corpus, removeWords, words = after_job_en)
  corpus <- tm_map(corpus, stripWhitespace)
  return(corpus)
}

clean_mediaCorp <- clean_corpusFR(datamedia_corpus)

dtm_media <- DocumentTermMatrix(clean_mediaCorp)

data_n_media <- tidy(clean_mediaCorp) %>% 
  unnest_tokens(word, text)


top50_words <- data_n_media %>%
  group_by(word) %>%
  count() %>%
  arrange(desc(n)) %>%
  head(50)









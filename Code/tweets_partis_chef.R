library(tidyverse)
library(tidytext)



# Connecting to hub 2.0
clessnhub::login(
  Sys.getenv("HUB_USERNAME"),
  Sys.getenv("HUB_PASSWORD"),
  Sys.getenv("HUB_URL"))

### Get candidate Tweets ###

get_qc_candidates <- function(){
  filter <- clessnhub::create_filter(type = "candidate",
                                     data = list(currentParty = 
                                                   "Québec solidaire"))
  QS <- clessnhub::get_items(table = 'persons', filter = filter, 
                             download_data = TRUE)
  
  filter <- clessnhub::create_filter(type = "candidate",
                                     data = list(currentParty = 
                                                   "Parti conservateur du Québec"))
  PCQ <- clessnhub::get_items(table = 'persons', filter = filter, 
                              download_data = TRUE)
  
  filter <- clessnhub::create_filter(type = "candidate",
                                     data = list(currentParty = 
                                                   "Parti libéral du Québec"))
  PLQ <- clessnhub::get_items(table = 'persons', filter = filter, 
                              download_data = TRUE)
  
  filter <- clessnhub::create_filter(type = "candidate",
                                     data = list(currentParty = 
                                                   "Coalition avenir Québec"))
  CAQ <- clessnhub::get_items(table = 'persons', filter = filter, 
                              download_data = TRUE)
  
  filter <- clessnhub::create_filter(type = "candidate",
                                     data = list(currentParty = 
                                                   "Parti québécois"))
  PQ <- clessnhub::get_items(table = 'persons', filter = filter, 
                             download_data = TRUE)
  
  Candidates <- rbind(QS, PCQ, PLQ, CAQ, PQ)
  return(Candidates)
}

candidate_tweets <- get_qc_candidates()

#### Get party tweets ####

get_party_tweets <- function(begin_date, end_date){
  tictoc::tic()
  myfilter <- clessnhub::create_filter(
    metadata = list(personType__regex="party"),
    data = list(creationDate__gte = begin_date,
                creationDate__lte = end_date))
  tweets_politicians <- clessnhub::get_items('tweets', myfilter, 
                                             download_data = TRUE)
  tictoc::toc()
  return(tweets_politicians)
}

party_tweets <- get_party_tweets("2022-08-01", "2022-11-01")

#### Get politicians Tweet ####

get_politicians_tweets <- function(begin_date, end_date){
  tictoc::tic()
  myfilter <- clessnhub::create_filter(
    metadata = list(personType__regex="candidate"),
    data = list(creationDate__gte = begin_date,
                creationDate__lte = end_date))
  tweets_politicians <- clessnhub::get_items('tweets', myfilter, 
                                             download_data = TRUE)
  tictoc::toc()
  return(tweets_politicians)
}

politicians_tweets <- get_politicians_tweets("2022-08-01", "2022-11-01")


#### Create a clean dataframe containing the relevant variables ####

CleanDataChefs <- politicians_tweets %>% 
  select(data.creationDate, data.creationTime, metadata.twitterHandle, data.text,
         type, data.mentions) %>% 
  filter(metadata.twitterHandle %in% c("francoislegault", "GNadeauDubois", 
                                       "PaulPlamondon", "E_Duhaime", "DomAnglade"))

CleanDataParty <- party_tweets %>% 
  select(data.creationDate, data.creationTime, metadata.twitterHandle, data.text, 
         type, data.mentions) %>% 
  filter(metadata.twitterHandle %in% c("@QuebecSolidaire", "@coalitionavenir", 
                                       "@LiberalQuebec", "@partiquebecois",
                                       "@PconservateurQc"))

### Bind the dataframes to create data for topic modeling ###

tweets_df <- rbind(CleanDataChefs, CleanDataParty)

tweets_noise <- gsub("[^A-Za-z]", " ", tweets_df)

tweets_caq <- tweets_df %>% 
  filter(metadata.twitterHandle %in% c("francoislegault", "coalitionavenir"))

#### Begin the functions for topic-modeling ####

library(tm)
library(qdap)


french_stopwords <- stopwords("fr")
custom_stop <- c("plus", "ça", "comme", "faire", "gens", "tous", "matin",
                 "fait", "ans", "aujoud'hui", "’", "merci", "tout", "grand", 
                 "jour", "campagne", "cest", "aujourdhui", "québec", "être",
                 "candidate", "candidat", "candidats", "a", "depuis", "aussi",
                 "veut", "soir", "propose", "polqc", "toutes", "ceux", "celles",
                 "and", "équipe")

twittercorpus <- Corpus(VectorSource(tweets_df$data.text))
twittercorpus <- tm_map(twittercorpus, tolower)
twittercorpus <- tm_map(twittercorpus, removePunctuation)
twittercorpus <- tm_map(twittercorpus, removeNumbers)
twittercorpus <- tm_map(twittercorpus, removeWords, stopwords("fr"))
twittercorpus <- tm_map(twittercorpus, stripWhitespace)
twittercorpus <- tm_map(twittercorpus, removeWords, custom_stop)



dtm_twitter <- DocumentTermMatrix(twittercorpus)
dtm_twitter

dtm_matrix <- as.matrix(dtm_twitter)

lda_model <- LDA(dtm_matrix, k = 10)

terms(lda_model, 10)

#### test tidytext ####

library(tidytext)
library(topicmodels)
tweet_topics <- tidy(lda_model, matrix = "beta")
tweet_topics

tweet_top_terms <- tweet_topics %>% 
  group_by(topic) %>% 
  top_n(10, beta) %>% 
  ungroup() %>% 
  arrange(topic, -beta)

tweet_top_terms %>% 
  mutate(term = reorder(term, beta)) %>% 
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()


#### test CAQ ####

french_stopwords <- stopwords("fr")

caqcorpus <- Corpus(VectorSource(tweets_caq$data.text))
caqcorpus <- tm_map(caqcorpus, content_transformer(tolower))
caqcorpus <- tm_map(caqcorpus, removePunctuation)
caqcorpus <- tm_map(caqcorpus, removeNumbers)
caqcorpus <- tm_map(caqcorpus, removeWords, french_stopwords)

dtm_twitter <- DocumentTermMatrix(caqcorpus)
dtm_twitter

dtm_matrix <- as.matrix(dtm_twitter)

lda_model <- LDA(dtm_matrix, k = 5)

terms(lda_model, 10)



  


















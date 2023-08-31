library(readtext)
library(tidyverse)
library(tidytext)
library(textdata)
library(tm)
library(pdftools)
library(rebus)
library(stringr)
library(reshape2) # pour function acast (transformer df en matrix)
library(lubridate)
library(clessnverse)
# Sentiment
library(quanteda)
# Topic modeling
library(topicmodels)


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
saveRDS(tweets_df, "_SharedFolder_article_twitter-elxn22/Data/tweetstext.rds")


tweets_noise <- gsub("[^A-Za-z]", " ", tweets_df)

#tweets_caq <- tweets_df %>% 
  #filter(metadata.twitterHandle %in% c("francoislegault", "coalitionavenir"))

#### Cleaning the dataframe for topic modeling ####

library(tm)

####Mots à enlever####
stopWords_en <-
  c(# Nom
    "nom", "people", "new", "old", "back", "way", "thing", "things", "left", "right", "mr", "ms",
    # Origines et politique
    "ontario", "ottawa", "toronto", "halifax", "montreal", "york", "united", "states",
    "vancouver", "canadian", "american", "canada", "government", "quebec", "minister",
    "federal", "province", "city", "québec", "quebecers", "calgary",
    #"quebec",
    # Marqueur de relations, déterminants
    "also", "per", "just", "like", "even", "still", "much", "since", "around", "well", "really", "might",
    "across", "whether", "least", "already",
    # Verbes
    "said", "says", "say", "will", "can", "get", "got", "found", "may", "told", "make", "made", "going",
    "take", "took", "think", "including", "want", "see", "called", "know", "known", "according",
    "ask", "asked", "put", "away", "among", "set", "show", "find", "went", "call", "come", "came",
    "need", "go",
    # Nombre et quantités
    "number", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten", "cent", "lot",
    "first", "second", "last", "end", "many", "former", "later", "next", "never", "always", "with", "without",
    "every", "several", "big", "short", "long", "little", "small", "less", "something", "somethings",
    # Temps et lieux
    "time", "times", "now", "lundi", "mardi", "mercredi", "jeudi", "vendredi", "samedi", "dimanche",
    "monday", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday", "gmt", "bst",
    "décembre", "janvier", "février", "mars", "avril", "mai", "juin", "juillet", "août", "septembre",
    "octobre", "novembre", "december", "january", "february", "march", "april", "may", "june",
    "july", "august", "september", "october", "november", "feb",
    "today", "yesterday", "another", "day", "days", "week", "weeks", "month", "months", "year", "years",
    "ago", "near", "far", "place", "early", "yet",
    # Relatif au journalisme et aux médias
    "media", "presse", "plus", "journal", "cbc", "devoir", "radio-canada", "agence", "qmi",
    "mediaqmi", "star", "cbc", "news", "press", "reuters", "reuter", "cp", "ap", "nouvelles",
    "published", "rights", "guardian", "copyright", "reserved", "timeupdated", "updated",
    "globe", "mail", "block", "related", "grdn", "anglais", "sun", "thesun", "newspapers",
    "limited", "washington", "post", "co", "tor", "ont", "french", "herald", "national post", "reuters",
    # autres en français
    "plus", "ça", "comme", "faire", "gens", "tous", "matin",
    "fait", "ans", "aujoud'hui", "’", "merci", "tout", "grand", 
    "jour", "campagne", "cest", "aujourdhui", "québec", "être",
    "candidate", "candidat", "candidats", "a", "depuis", "aussi",
    "veut", "soir", "propose", "polqc", "toutes", "ceux", "celles",
    "and", "équipe"
  )
# Autres
#"x", "h", "s", "t", "th", "à") # ajouter d'autres mots

####Fonction nettoyage####
# Créer une fonction pour rapidement nettoyer notre corpus

####Fonction nettoyage####
# Créer une fonction pour rapidement nettoyer notre corpus



clean_corpusEN <- function(corpus){
  corpus <- tm::tm_map(corpus, content_transformer(tolower))
  corpus <- tm::tm_map(corpus, removeWords, stopwords("english"))
  corpus <- tm::tm_map(corpus, removeWords, words = stopWords_en)
  corpus <- tm::tm_map(corpus, removePunctuation, preserve_intra_word_dashes = T)
  #corpus <- tm::tm_map(corpus, removeNumbers)
  #corpus <- tm::tm_map(corpus, stemDocument) # stemming SEULEMENT pour le topic modeling
  #corpus <- tm_map(corpus, removeWords, words = keywords)
  #corpus <- tm_map(corpus, removeWords, words = after_job_en)
  corpus <- tm::tm_map(corpus, stripWhitespace)
  return(corpus)
}

clean_tweets <- cleanx(twittercorpus)

dtm_twitter <- DocumentTermMatrix(twittercorpus)

# Pour changer un DTM en format tidy, on utilise tidy() du broom package.
data_n <- tidy(clean_corp) %>%
  bind_cols(rds_all) %>%
  select(-author, -datetimestamp, -description, -heading, -language, -origin, -doc_id, -id, -text...10) %>%
  rename(text = text...8) %>%
  unnest_tokens(word, text)



french_stopwords <- stopwords("fr")
custom_stop <- c("plus", "ça", "comme", "faire", "gens", "tous", "matin",
                 "fait", "ans", "aujoud'hui", "’", "merci", "tout", "grand", 
                 "jour", "campagne", "cest", "aujourdhui", "québec", "être",
                 "candidate", "candidat", "candidats", "a", "depuis", "aussi",
                 "veut", "soir", "propose", "polqc", "toutes", "ceux", "celles",
                 "and", "équipe")
stopWords_en <-
  c(# Nom
    "nom", "people", "new", "old", "back", "way", "thing", "things", "left", "right", "mr", "ms",
    # Origines et politique
    "ontario", "ottawa", "toronto", "halifax", "montreal", "york", "united", "states",
    "vancouver", "canadian", "american", "canada", "government", "quebec", "minister",
    "federal", "province", "city", "québec", "quebecers", "calgary",
    #"quebec",
    # Marqueur de relations, déterminants
    "also", "per", "just", "like", "even", "still", "much", "since", "around", "well", "really", "might",
    "across", "whether", "least", "already",
    # Verbes
    "said", "says", "say", "will", "can", "get", "got", "found", "may", "told", "make", "made", "going",
    "take", "took", "think", "including", "want", "see", "called", "know", "known", "according",
    "ask", "asked", "put", "away", "among", "set", "show", "find", "went", "call", "come", "came",
    "need", "go",
    # Nombre et quantités
    "number", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten", "cent", "lot",
    "first", "second", "last", "end", "many", "former", "later", "next", "never", "always", "with", "without",
    "every", "several", "big", "short", "long", "little", "small", "less", "something", "somethings",
    # Temps et lieux
    "time", "times", "now", "lundi", "mardi", "mercredi", "jeudi", "vendredi", "samedi", "dimanche",
    "monday", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday", "gmt", "bst",
    "décembre", "janvier", "février", "mars", "avril", "mai", "juin", "juillet", "août", "septembre",
    "octobre", "novembre", "december", "january", "february", "march", "april", "may", "june",
    "july", "august", "september", "october", "november", "feb",
    "today", "yesterday", "another", "day", "days", "week", "weeks", "month", "months", "year", "years",
    "ago", "near", "far", "place", "early", "yet",
    # Relatif au journalisme et aux médias
    "media", "presse", "plus", "journal", "cbc", "devoir", "radio-canada", "agence", "qmi",
    "mediaqmi", "star", "cbc", "news", "press", "reuters", "reuter", "cp", "ap", "nouvelles",
    "published", "rights", "guardian", "copyright", "reserved", "timeupdated", "updated",
    "globe", "mail", "block", "related", "grdn", "anglais", "sun", "thesun", "newspapers",
    "limited", "washington", "post", "co", "tor", "ont", "french", "herald", "national post", "reuters",
    # autres en français
    "plus", "ça", "comme", "faire", "gens", "tous", "matin",
    "fait", "ans", "aujoud'hui", "’", "merci", "tout", "grand", 
    "jour", "campagne", "cest", "aujourdhui", "québec", "être",
    "candidate", "candidat", "candidats", "a", "depuis", "aussi",
    "veut", "soir", "propose", "polqc", "toutes", "ceux", "celles",
    "and", "équipe"
  )

twittercorpus <- Corpus(VectorSource(tweets_df$data.text))
twittercorpus <- tm_map(twittercorpus, tolower)
twittercorpus <- tm_map(twittercorpus, removePunctuation)
twittercorpus <- tm_map(twittercorpus, removeNumbers)
twittercorpus <- tm_map(twittercorpus, removeWords, stopwords("fr"))
twittercorpus <- tm_map(twittercorpus, removeWords, stopWords_en)
twittercorpus <- tm_map(twittercorpus, stripWhitespace)
twittercorpus <- tm_map(twittercorpus, removeWords, custom_stop)


dtm_twitter <- DocumentTermMatrix(twittercorpus)
dtm_twitter

dtm_matrix <- as.matrix(dtm_twitter)

lda_model <- LDA(dtm_matrix, k = 5)

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



  


















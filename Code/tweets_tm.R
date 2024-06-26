library(tidyverse)
library(tm)
library(topicmodels)
library(tidytext)
remotes::install_github("clessn/clessnverse")
library(clessnverse)


tweets_df <- readRDS("/Users/jeremiedrouin/Dropbox/Travail/Universite_Laval/CLESSN/article_twitter-elxn22/_SharedFolder_article_twitter-elxn22/Data/tweetstext.rds")

run_dictionary(
  data.frame(colnames(attitude)),
  text = colnames(attitude),
  dictionary = quanteda::data_dictionary_LSD2015
) %>% head()

run_dictionary(tweets_df, data.text, dictionary, verbose = TRUE)

#tweets_noise <- gsub("[^A-Za-z]", " ", tweets_df)


#### ###############
##########Cleaning the dataframe for topic modeling ####
####################


##Mots à enlever#

stopwords_custom <-
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
    "and", "équipe", "québécois"
  )
# Autres
#"x", "h", "s", "t", "th", "à") # ajouter d'autres mots

####Fonction nettoyage####
# Créer une fonction pour rapidement nettoyer notre corpus

clean_tweets <- function(corpus){
  corpus <- tm::tm_map(corpus, content_transformer(tolower))
  corpus <- tm::tm_map(corpus, removeWords, stopwords("english"))
  corpus <- tm::tm_map(corpus, removeWords, stopwords("fr"))
  corpus <- tm::tm_map(corpus, removeWords, words = stopwords_custom)
  corpus <- tm::tm_map(corpus, removeNumbers)
  corpus <- tm::tm_map(corpus, removePunctuation, preserve_intra_word_dashes = T)
  #corpus <- tm::tm_map(corpus, removeNumbers)
  #corpus <- tm::tm_map(corpus, stemDocument) # stemming SEULEMENT pour le topic modeling
  #corpus <- tm_map(corpus, removeWords, words = keywords)
  #corpus <- tm_map(corpus, removeWords, words = after_job_en)
  corpus <- tm::tm_map(corpus, stripWhitespace)
  return(corpus)
}

# Convert dataframe to corpus
corpus <- Corpus(VectorSource(tweets_df$data.text))

# Apply the cleaning function to the corpus
clean_corpus <- clean_tweets(corpus)

dtm <- DocumentTermMatrix(clean_corpus)
dtm

lda_model <- LDA(dtm, k = 5)
terms(lda_model, 10)

















# Pour changer un DTM en format tidy, on utilise tidy() du broom package.
data_n <- tidy(clean_corp) %>%
  bind_cols(rds_all) %>%
  select(-author, -datetimestamp, -description, -heading, -language, -origin, -doc_id, -id, -text...10) %>%
  rename(text = text...8) %>%
  unnest_tokens(word, text)


##### Code qui fonctionne #### 

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
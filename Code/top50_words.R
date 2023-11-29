#########################
####Ce script vise à trouver le top 50 des mots pour chacun de nos corpus####
#########################

library(tidyverse)
library(tm)
library(tidytext)



########
####Tweets
########

tweets_texts <- readRDS("/Users/jeremiedrouin/Dropbox/Travail/Universite_Laval/CLESSN/Publications/article_twitter-elxn22/_SharedFolder_article_twitter-elxn22/Data/tweetstext.rds") %>% 
  mutate(doc_id = row_number()) %>% 
  select(doc_id, data.text)

colnames(tweets_texts) <- c("doc_id", "text")
datatweets_source <- DataframeSource(tweets_texts)
datatweets_corpus <- VCorpus(datatweets_source)

####
####Créer la fonction pour ls stopwords français####

stopWords_fr <- # Nom et prénom
  c("paradis", "nom", "personne", "personnes", "simon", "mme", "emery", "françois",
    "gabriel", "éric", "dominique", "paul", "legault", "nadeau-dubois", "duhaime",
    "anglade", "st", "plamondon", "pierre", "jean", "poilievre", "côté",
    # Origines et politique
    "québec", "montréal", "ottawa", "états-unis", "new", "york", "américain", "américains", "saint",
    "canadien", "québécois", "québécoise", "chef", "parti", "campagne", "canada", "caq", "qs", 
    "caquiste", "circonscription", "candidat", "candidats", "cheffe", "pq", "plq",
    "élection", "élections", "chefs", "ministre", "partis", "politique", "politiques",
    "pcq", "avenir", "péquiste", "solidaire", "conservateur", "libéral", "libérale",
    "candidate", "libéraux", "polqc", "assnat", "qc", "paulplamondon", 
    "francoislegault", "partiquebecois", "quebecsolidaire",
    # Marqueur de relations, déterminants
    "chez", "tous", "tout", "toute", "toutes", "sous",  "dont", "entre", "encore", "ainsi",
    "donc", "notamment",  "également", "cas", "parce", "afin", "toutefois", "comme", "aussi",
    "contre", "dès", "lorsqu", "ceux", "celle", "elles", "car", "quant", "ça", 
    "si", "là", "où", "plutôt",
    # Verbes
    "faire", "fait", "être", "avoir", "peut", "pourrait", "dit", "faut", "veut", "explique",
    "vont", "doit", "dire", "voir", "note", "expliqué", "font", "mis", "lire", "affirme",
    "déclaré", "va", "a", "affirmé", "pense", "prendre",
    # Nombre et quantités
    "nombre", "un", "deux", "trois", "quatre", "cinq", "six", "sept", "huit", "neuf", "dix",
    "quelques", "plusieurs", "premier", "première", "beaucoup", "peu", "certain", "certains",
    "certaines", "dernier", "dernière", "très", "plus", "moins", "aucun", "environ",
    "grand", "petit", "bien", "jamais", "trop", "toujours",
    # Temps et lieux
    "temps", "lieux", "lundi", "mardi", "mercredi", "jeudi", "vendredi", "samedi", "dimanche",
    "décembre", "janvier", "février", "mars", "avril", "mai", "juin", "juillet", "août",
    "septembre", "octobre", "novembre",
    "jour", "jours", "mois", "ans", "année", "années", "quand", "déjà", "après", "jusqu", "lors", "alors",
    "aujourdhui", "hier", "demain", "pendant", "fois", "près", "maintenant", "depuis", 
    "ailleurs", "avant", "après", "début", "fin", "semaine", "vers", "durant",
    "soir", "matin", "aujourd'hui", 
    # Relatif au journalisme
    "presse", "plus", "journal", "cbc", "devoir", "radiocanada", "agence", "qmi",
    "mediaqmi", "selon", "nouvelle", "photo", "photos", "courtoisie", "archives",
    "actualités", "nouvelles", "wwwcanouvelles", "hyperliens", "wwwradioca", "afp",
    "ici",
    # Autres
    "x", "h", "the", "non", "oui", "autre", "autres", "rien", "propos", "parole",
    "sujet", "moment", "sortant", "équipe")


###### Fonction de noettoyage

clean_corpusFR <- function(corpus){
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, stopwords("fr"))
  corpus <- tm_map(corpus, removeWords, words = stopWords_fr)
  corpus <- tm_map(corpus, removePunctuation, preserve_intra_word_dashes = T)
  corpus <- tm_map(corpus, removeNumbers)
  # corpus <- tm_map(corpus, stemDocument) # stemming SEULEMENT pour le topic modeling
  # corpus <- tm_map(corpus, removeWords, words = keywords)
  #corpus <- tm_map(corpus, removeWords, words = after_job_en)
  corpus <- tm_map(corpus, stripWhitespace)
  return(corpus)
}


  clean_tweetCorp <- clean_corpusFR(datatweets_corpus)
  
  dtm_tweets <- DocumentTermMatrix(clean_tweetCorp)
  
  data_n_tweet <- tidy(clean_tweetCorp) %>% 
    unnest_tokens(word, text)
  
  
  top50_words_tweets <- data_n_tweet %>%
    group_by(word) %>%
    count() %>%
    arrange(desc(n)) %>%
    head(50)
  
  saveRDS(top50_words_tweets, "_SharedFolder_article_twitter-elxn22/Data/top50_words_tweets.rds")















#########
######Top 50 médias
#########

media_texts <- readRDS("/Users/jeremiedrouin/Dropbox/Travail/Universite_Laval/CLESSN/Publications/article_twitter-elxn22/_SharedFolder_article_twitter-elxn22/Data/MediaClean_2023-11-21.rds") %>% 
  select(doc_id, text)
colnames(media_texts) <- c("doc_id", "text")


datamedia_source <- DataframeSource(media_texts)
datamedia_corpus <- VCorpus(datamedia_source)

####
####Créer la fonction pour ls stopwords français####







clean_corpusFR <- function(corpus){
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, stopwords("fr"))
  corpus <- tm_map(corpus, removeWords, words = stopWords_fr)
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

saveRDS(top50_words, "_SharedFolder_article_twitter-elxn22/Data/top50_words_medias.rds")










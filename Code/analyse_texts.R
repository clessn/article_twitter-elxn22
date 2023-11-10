########################################################################################################### ##
################################################### Packages #################################################
########################################################################################################### ##

# Cleaning
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
# Sentiment
library(quanteda)
# Topic modeling
library(topicmodels)
# Ploting
library(ggthemes)
library(sp)
library(RColorBrewer)
library(wordcloud)
library(viridisLite)
library(SnowballC)
library(plotrix)
library(dendextend)
library(radarchart) # pour faire des radar chart
library(htmlwidgets) # pour enregistrer des radar chart
library(webshot) # pour enregistrer des radar chart
library(treemap)
library(gridExtra)
library(stargazer)

########################################################################################################### ##
####################################################### DATA #################################################
########################################################################################################### ##

data <- readRDS("_SharedFolder_article_syrie-ukraine/Data/dataset.rds") %>%
  unnest_sentences(text, text) %>%
  filter(grepl('refugee|refugees|migrant|migrants', text)) %>%
  ungroup() %>%
  mutate(id_sentence = row_number())
#  filter(opinion != 0) %>%


## Enregistrer le csv
#saveRDS(data, paste0("_SharedFolder_article_syrie-ukraine/Data/", "dataset_refugees.rds"))

dataSyrie <- data %>%
  filter(country == "Syrie")# %>%
 # filter(between(date, as.Date('2011-03-15'), as.Date('2024-01-01')))# %>%
 # filter(source %in% c("The Globe and Mail", "Toronto Star"))
 # filter(opinion == 0)  %>%
#  filter(opinion != 0)

## Enregistrer le csv
#write_csv(dataSyrie2015, paste0("_SharedFolder_article_syrie-ukraine/Data/", "dataset_refugees-Syria.csv"))

# dataSyrie2015_op <- data %>%
#   filter(country == "Syrie") %>%
#   filter(between(date, as.Date('2015-01-01'), as.Date('2015-12-31'))) %>%
#   filter(opinion != 0)

dataUkraine <- data %>%
  filter(country == "Ukraine")# %>%
 # filter(between(date, as.Date('2022-02-24'), as.Date('2024-01-01')))#  %>%
#  filter(source %in% c("The Globe and Mail", "Toronto Star"))
  #filter(opinion == 0)

#write_csv(dataUkraine, paste0("_SharedFolder_article_syrie-ukraine/Data/", "dataset_refugees-Ukraine.csv"))

dataIraq <- data %>%
  filter(country == "Iraq")

# dataUkraine_op <- data %>%
#   filter(country == "Ukraine") %>%
#   filter(between(date, as.Date('2022-01-01'), as.Date('2022-12-31'))) %>%
#   filter(opinion != 0)

nb_refugies_day <- read.csv("_SharedFolder_article_syrie-ukraine/Data/nb_refugies_day.csv", sep = ";") %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d"),
         refugies1000 = crossing_border/1000)

# Date de début du conflit en Syrie
start_date_syria <- ymd("2011-03-15")

# Date de début du conflit en Ukraine
start_date_ukraine <- ymd("2022-02-24")

# Date de début du conflit en Irak
start_date_iraq <- ymd("2003-03-20")

########################################################################################################### ##
################################################# Corpus anglo ###############################################
########################################################################################################### ##

# Créer un DataframeSource total
#data_source <- DataframeSource(data)
# Convertir en corpus volatile total
#data_corpus <- VCorpus(data_source)

# Syrie
#dataSyrie_source <- DataframeSource(dataSyrie)
#dataSyrie_corpus <- VCorpus(dataSyrie_source)

# Syrie
dataSyrie_source <- DataframeSource(dataSyrie)
dataSyrie_corpus <- VCorpus(dataSyrie_source)

# Ukraine
dataUkraine_source <- DataframeSource(dataUkraine)
dataUkraine_corpus <- VCorpus(dataUkraine_source)

# Iraq
dataIraq_source <- DataframeSource(dataIraq)
dataIraq_corpus <- VCorpus(dataIraq_source)

########################################################################################################### ##
################################################## Cleaning EN ###############################################
########################################################################################################### ##

# Utiliser cette ligne pour faire des test
#Weed_corpus <- tm_map(Weed_corpus, removeWords, words = c(stopwords("en")))

# les mots à enlever
stopWords_en <-
  c(# Nom
    "nom", "people", "new", "old", "back", "way", "thing", "things", "left", "right", "mr", "ms",
    # Origines et politique
    # "ontario", "ottawa", "toronto", "halifax", "quebec", "montreal", "york", "united", "states",
    # "vancouver", "canadian", "american",
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
    "limited", "washington", "post", "httpwwwwashingtonpostcom", "co", "tor", "ont",

    # Autres
    "x", "h", "s", "t", "th", "à") # ajouter d'autres mots

keywords <- # Pour enlever les mots clés liés au conflit
  c("refugees", "refugee", "ukraine", "syria", "syrian", "syrians", "ukrainians", "ukrainian",
    "war", "crisis", "migrants", "forces", "invasion", "military", "attack", "attacks", "conflict",
    "kill", "killed", "threat", "fight", "fights", "violence", "fighting", "death", "dead",
    "deads", "crime", "crimes", "aggression", "aggressions", "emergency")

after_job_en <- # Pour enlever les résidus d'après nettoyage
  c("s", "t", "th", "à", "a", "u", "x", "h", "tri", "st", "am", "pm", "m", "re", "c")

# Créer une fonction pour rapidement nettoyer notre corpus
clean_corpusEN <- function(corpus){
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, stopwords("english"))
  corpus <- tm_map(corpus, removeWords, words = stopWords_en)
  corpus <- tm_map(corpus, removePunctuation, preserve_intra_word_dashes = T)
  corpus <- tm_map(corpus, removeNumbers)
 # corpus <- tm_map(corpus, stemDocument) # stemming SEULEMENT pour le topic modeling
 # corpus <- tm_map(corpus, removeWords, words = keywords)
  corpus <- tm_map(corpus, removeWords, words = after_job_en)
  corpus <- tm_map(corpus, stripWhitespace)
  return(corpus)
}

# Appliquer la fonction
# Peut prendre plusieurs minutes, selon la taille du corpus
#clean_corp <- clean_corpusEN(data_corpus)
#cleanSyrie_corp <- clean_corpusEN(dataSyrie_corpus)
cleanSyrie_corp <- clean_corpusEN(dataSyrie_corpus)
cleanUkraine_corp <- clean_corpusEN(dataUkraine_corpus)
cleanIraq_corp <- clean_corpusEN(dataIraq_corpus)


# Vérifier un texte nettoyé
# content(clean_corpEN[[12]])

# Voir le vieux text pas nettoyé
# DataWeedMedia$text[12]

#meta(clean_corpEN[1])

########################################################################################################### ##
###################################################### DTM / TDM #############################################
########################################################################################################### ##

# Créer une document-term matrix à partir du corpus nettoyé

# TF-IDF est utilisé quand on a un large corpus avec peu de diversité de termes (ce qui n'est pas notre cas).
# Weed_dtm <- DocumentTermMatrix(clean_corp, control = list(weighting = weightTfIdf))

#data_dtm <- DocumentTermMatrix(clean_corp)

#dataSyrie_dtm <- DocumentTermMatrix(cleanSyrie_corp)

dataSyrie_dtm <- DocumentTermMatrix(cleanSyrie_corp)

dataUkraine_dtm <- DocumentTermMatrix(cleanUkraine_corp)

dataIraq_dtm <- DocumentTermMatrix(cleanIraq_corp)


# On peut aussi créer une term-document matrix (notamment pour dendrogram)
#Weed_tdmFR <- TermDocumentMatrix(clean_corpFR)
#Weed_tdmEN <- TermDocumentMatrix(clean_corpEN)

# Voir
# Weed_dtm

########################################################################################################### ##
####################################################### Matrix ###############################################
########################################################################################################### ##

# Convertir en matrice
#data_matrix <- as.matrix(data_dtm)

#Weed_mEN <- as.matrix(Weed_dtmEN)

# Imprimer les dimensions de la matrix
# dim(Weed_m)

# Imprimer une portion de la matrice
#Weed_mEN[,1:12]

########################################################################################################### ##
###################################################### Top_n ############################################
########################################################################################################### ##

# Créer la bd
data_n <- tidy(cleanSyrie2015_corp) %>%
  bind_cols(dataSyrie2015) %>%
  select(-author, -datetimestamp, -description, -heading, -language, -origin, -doc_id, -text...10, -id) %>%
  rename(text = text...8) %>%
  unnest_tokens(word, text)

# Ajouter un filtre par pays ou média pour gérer le nettoyage en détails
# data_n <- data_n %>%
#  # filter(country == "Ukraine")
#   filter(source == "Toronto Star")

# Calculer le top 50
top50_words <- data_n %>%
  group_by(word) %>%
  count() %>%
  arrange(desc(n)) %>%
  head(50)

########################################################################################################### ##
############################################### Nombre d'articles ############################################
########################################################################################################### ##

#color <- bpy.colors(n = 8, cutoff.tails = 0.1, alpha = 1.0)

count1 <- dataUkraine %>%
  group_by(date, country) %>%
#  filter(source != "The Gardian") %>%
  summarize(n = n()) %>%
  na.omit() %>%
  mutate(date = as.POSIXct(date))

count2 <- dataSyrie2015 %>%
  group_by(date, country) %>%
  #  filter(source != "The Gardian") %>%
  summarize(n = n()) %>%
  na.omit() %>%
  mutate(date = as.POSIXct(date))

plot1 <- ggplot(count1, aes(x = date, y = n)) +
  geom_line(aes(color = country), size = 1, color = "#5B9BD5") +
  scale_x_datetime("", date_labels = "%b", date_breaks = "1 month") +
  geom_vline(xintercept = as.POSIXct("2022-03-15"), color = "darkgrey", size = 0.8  , linetype = "dashed") +
 geom_text(x = as.POSIXct("2022-03-15"), y = max(count1$n) * 1.3, label = "Migration peak", angle = 90, vjust = -0.6) +
 # scale_y_continuous(name="", breaks = c(0, 10, 20, 30, 40, 50, 60), limits = c(0, 65)) +
  theme_clean() +
  ggtitle("Ukraine (2022)") +
  theme(plot.title = element_text(size = 18),
        plot.subtitle=element_text(size = 13),
        plot.caption = element_text(hjust = 1, size = 11),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 14),
        legend.position = c(.85, .90),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        panel.border = element_blank(),
        legend.margin = margin(6, 6, 6, 6))

plot2 <- ggplot(count2, aes(x = date, y = n)) +
  geom_line(aes(color = country), size = 1, color = "#CC9933") +
  scale_x_datetime("", date_labels = "%Y", date_breaks = "1 year") +
  geom_vline(xintercept = as.POSIXct("2015-10-01"), color = "darkgrey", size = 0.8  , linetype = "dashed") +
  geom_text(x = as.POSIXct("2015-10-01"), y = max(count2$n) * 1.3, label = "Migration peak", angle = 90, vjust = -0.60) +
  # geom_vline(xintercept = as.POSIXct("2015-09-02"), color = "darkgrey", size = 0.8  , linetype = "dashed") +
  # geom_text(x = as.POSIXct("2015-09-02"), y = max(count2$n) * 1.26, label = "Death of Alan Kurdi", angle = 90, vjust = -0.60) +
  # geom_vline(xintercept = as.POSIXct("2015-11-13"), color = "darkgrey", size = 0.8  , linetype = "dashed") +
  # geom_text(x = as.POSIXct("2015-11-13"), y = max(count2$n) * 1.32, label = "Paris attacks", angle = 90, vjust = -0.60) +
  # scale_y_continuous(name="Number of articles per month\n", breaks = c(0, 10, 20, 30, 40, 50, 60), limits = c(0, 65)) +
  theme_clean() +
  ggtitle("Syria (2015)") +
  theme(plot.title = element_text(size = 18),
        plot.subtitle=element_text(size = 13),
        plot.caption = element_text(hjust = 1, size = 11),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 14),
        legend.position = c(.85, .90),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        panel.border = element_blank(),
        legend.margin = margin(6, 6, 6, 6))

plot <- grid.arrange(plot2, plot1, nrow = 1)


ggsave("../_SharedFolder_article_syrie-ukraine/nbArticles/nbArticles-syrie-ukraine.png", plot, width = 30, height = 17, units = c("cm"))


########################################################################################################### ##
################################### Sentiment EN Syrie VS Ukraine ############################
########################################################################################################### ##

# Créer une fonction pour analyser le ton des mots avec le Lexicoder Sentiment Dictionary
runDictionaryFunction <- function(corpusA, dataA, word, dfmA, dataB, dictionaryA) {
  corpusA <- corpus(dataA$word)
  dfmA    <- dfm(corpusA, dictionary = dictionaryA)
  dataB   <- convert(dfmA, to = "data.frame")
  return(dataB)
}

# Pour changer un DTM en format tidy, on utilise tidy() du broom package.
data_tidy <- tidy(cleanUkraine_corp) %>%
  bind_cols(dataUkraine) %>%
  select(-author, -datetimestamp, -description, -heading, -language, -origin, -doc_id, -id, -text...10) %>%
  rename(text = text...8) %>%
  unnest_tokens(word, text)

# Pour obtenir des %
polarity <- data_tidy %>%
  mutate(date = as.POSIXct(date))  %>%
  group_by(date) %>%
  mutate(total_words = n())

# On utilise le lexicoder anglo du package de Quanteda, qui se nomme data_dictionary_LSD2015
data_ton <- runDictionaryFunction(dataA = polarity,
                                    word = word,
                                    dictionaryA = data_dictionary_LSD2015)

# Pour le top négatif et positif
topcount <- bind_cols(polarity, data_ton) %>%
  group_by(word) %>%
  summarise(negative = sum(negative),
            positive = sum(positive))

# Ukraine
graphdata_ukraine <- bind_cols(polarity, data_ton) %>%
  group_by(date, total_words) %>%
  summarise(negative = sum(negative),
            positive = sum(positive)) %>%
  mutate(propNeg = (negative/total_words),
         propPos = (positive/total_words)) %>%
  mutate(diffProp = propPos - propNeg) %>%
  mutate(country = "Ukraine")

# Syria
graphdata_syria <- bind_cols(polarity, data_ton) %>%
  group_by(date, total_words) %>%
  summarise(negative = sum(negative),
            positive = sum(positive)) %>%
  mutate(propNeg = (negative/total_words),
         propPos = (positive/total_words)) %>%
  mutate(diffProp = propPos - propNeg) %>%
  mutate(country = "Syria")

# On met tous ensemble et on calcule
graphdata <- graphdata_ukraine %>%
  bind_rows(graphdata_syria) %>%
  group_by(country) %>%
  summarise(
    n=n(),
    mean=mean(diffProp),
    sd=sd(diffProp)) %>%
  mutate(se=sd/sqrt(n))  %>%
  mutate(ic=se * qt((1-0.05)/2 + .5, n-1))


# Standard deviation
ggplot(graphdata) +
  geom_bar( aes(x=country, y=mean), stat="identity", fill="forestgreen", alpha=0.5) +
  geom_errorbar( aes(x=country, ymin=mean-sd, ymax=mean+sd), width=0.4, colour="orange", alpha=0.9, size=1.5) +
  ggtitle("using standard deviation")

# Standard Error
ggplot(graphdata) +
  geom_bar( aes(x=country, y=mean), stat="identity", fill="forestgreen", alpha=0.5) +
  geom_errorbar( aes(x=country, ymin=mean-se, ymax=mean+se), width=0.4, colour="orange", alpha=0.9, size=1.5) +
  ggtitle("using standard error")

# Confidence Interval
ggplot(graphdata) +
  geom_bar( aes(x=country, y=mean), stat="identity", fill="forestgreen", alpha=0.5) +
  geom_errorbar( aes(x=country, ymin=mean-ic, ymax=mean+ic), width=0.4, colour="orange", alpha=0.9, size=1.5) +
  ggtitle("using confidence interval")

# Permutation

# Chargement des bibliothèques nécessaires
library(boot)

# Calcul de la différence de ton observée entre les deux groupes
diff_observed <-  graphdata_ukraine %>%
  bind_rows(graphdata_syria) %>%
  ungroup() %>%
  summarise(mean=mean(diffProp))

# Fonction pour générer une statistique de test (différence de moyennes)
statistic <- function(data, indices) {
  mean(graphdata_ukraine$diffProp[indices]) - mean(graphdata_syria$diffProp[indices])
}

# Utilisation de la fonction boot pour effectuer le test de permutation
boot_results <- boot(data = data.frame(ton_syriens, ton_ukrainiens), statistic, R = 1000)

# Calcul de la p-valeur
p_value <- sum(abs(boot_results$t) >= abs(diff_observed)) / length(boot_results$t)

# Création de vos données simulées (remplacez par vos propres données)
set.seed(123) # pour la reproductibilité
ton_syriens <- rnorm(100, mean = 0.2, sd = 0.1) # Ton pour les réfugiés syriens
ton_ukrainiens <- rnorm(100, mean = 0.25, sd = 0.1) # Ton pour les réfugiés ukrainiens

# Test t de Student
t_test <- t.test(graphdata_ukraine$diffProp, graphdata_syria$diffProp, alternative = "two.sided", var.equal = TRUE)

# Affichage des résultats
cat("Statistique de test t:", t_test$statistic, "\n")
cat("P-valeur:", t_test$p.value, "\n")


# # On plot
# ggplot(graphdata, aes(x = date, y = diffProp)) +
#   geom_hline(yintercept = 0, color = "red") +
#  geom_line() +
#  # geom_point(alpha = 0.4) +
#   geom_smooth(se = F, span = 0.4, size = 2.5) +
#   scale_x_datetime("", date_labels = "%m-%Y", date_breaks = "1 month") +
#   # scale_color_viridis_d(labels = c("CBC", "Toronto Star"),
#   #                       limits = c("CBC", "TStar")) +
#   theme_clean() +
#   # labs(title = "Évolution du ton des articles sur le cannabis dans les médias anglophones (1985 à 2019)",
#   #      # subtitle = "Version 1: aucune pondération.",
#   #      subtitle = "",
#   #      # caption = "Dictionnaire: version francophone du Lexicoder (Duval et Pétry, 2016)
#   #      # Pondéré en fonction du nombre moyen de mots par mois, après nettoyage des textes et application du dictionnaire") +
#   #      caption = "n = 14 771 articles
#   #      Dictionnaire: Lexicoder (Soroka and Young, 2015)
#   #      L'indice de ton est calculé en fonction de la différence entre les proportions de mots positifs et négatifs par mois") +
#   ylab("Proportion de mots négatifs\n") +
#   theme(plot.title = element_text(hjust = -1.5, size = 18),
#         plot.subtitle=element_text(size = 13),
#         plot.caption = element_text(hjust = 1, size = 11),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.title.y = element_text(size = 14),
#         legend.position = "top") +
#   guides(color=guide_legend(title="Country"))
#
# ggsave("../_SharedFolder_article_syrie-ukraine/_v2/lexicoder/lexicoder-ukraine.png", width = 30, height = 17, units = c("cm"))

########################################################################################################### ##
########################################## Sentiment EN Opinion Vs article ###################################
########################################################################################################### ##

# Créer une fonction pour analyser le ton des mots avec le Lexicoder Sentiment Dictionary
runDictionaryFunction <- function(corpusA, dataA, word, dfmA, dataB, dictionaryA) {
  corpusA <- corpus(dataA$word)
  dfmA    <- dfm(corpusA, dictionary = dictionaryA)
  dataB   <- convert(dfmA, to = "data.frame")
  return(dataB)
}

# Pour changer un DTM en format tidy, on utilise tidy() du broom package.
data_tidy <- tidy(cleanSyrie2015_corp) %>%
  bind_cols(dataSyrie2015) %>%
  select(-author, -datetimestamp, -description, -heading, -language, -origin, -doc_id, -id, -text...10) %>%
  rename(text = text...8) %>%
  unnest_tokens(word, text)

# Pour obtenir des %
polarity <- data_tidy %>%
  mutate(date = as.POSIXct(date))  %>%
  group_by(date) %>%
  mutate(total_words = n())

# On utilise le lexicoder anglo du package de Quanteda, qui se nomme data_dictionary_LSD2015
data_ton <- runDictionaryFunction(dataA = polarity,
                                  word = word,
                                  dictionaryA = data_dictionary_LSD2015)

# Pour le top négatif et positif
topcount <- bind_cols(polarity, data_ton) %>%
  group_by(word) %>%
  summarise(negative = sum(negative),
            positive = sum(positive))

# Opinion
graphdata_opinion <- bind_cols(polarity, data_ton) %>%
  group_by(date, total_words) %>%
  filter(opinion == 1) %>%
  summarise(negative = sum(negative),
            positive = sum(positive)) %>%
  mutate(propNeg = (negative/total_words),
         propPos = (positive/total_words)) %>%
  mutate(diffProp = propPos - propNeg) %>%
  mutate(type = "Opinion")

# News
graphdata_news <- bind_cols(polarity, data_ton) %>%
  group_by(date, total_words) %>%
  filter(opinion == 0) %>%
  summarise(negative = sum(negative),
            positive = sum(positive)) %>%
  mutate(propNeg = (negative/total_words),
         propPos = (positive/total_words)) %>%
  mutate(diffProp = propPos - propNeg) %>%
  mutate(type = "News")

# On met tous ensemble et on calcule
graphdata <- graphdata_news %>%
  bind_rows(graphdata_opinion) %>%
  group_by(type) %>%
  summarise(
    n=n(),
    mean=mean(diffProp),
    sd=sd(diffProp)) %>%
  mutate(se=sd/sqrt(n))  %>%
  mutate(ic=se * qt((1-0.05)/2 + .5, n-1))


# Standard deviation
ggplot(graphdata) +
  geom_bar( aes(x=type, y=mean), stat="identity", fill="forestgreen", alpha=0.5) +
  geom_errorbar( aes(x=type, ymin=mean-sd, ymax=mean+sd), width=0.4, colour="orange", alpha=0.9, size=1.5) +
  ggtitle("using standard deviation")

# Standard Error
ggplot(graphdata) +
  geom_bar( aes(x=type, y=mean), stat="identity", fill="forestgreen", alpha=0.5) +
  geom_errorbar( aes(x=type, ymin=mean-se, ymax=mean+se), width=0.4, colour="orange", alpha=0.9, size=1.5) +
  ggtitle("using standard error")

# Confidence Interval
ggplot(graphdata) +
  geom_bar( aes(x=type, y=mean), stat="identity", fill="forestgreen", alpha=0.5) +
  geom_errorbar( aes(x=type, ymin=mean-ic, ymax=mean+ic), width=0.4, colour="orange", alpha=0.9, size=1.5) +
  ggtitle("using confidence interval")

# # On plot
# ggplot(graphdata, aes(x = date, y = diffProp)) +
#   geom_hline(yintercept = 0, color = "red") +
#  geom_line() +
#  # geom_point(alpha = 0.4) +
#   geom_smooth(se = F, span = 0.4, size = 2.5) +
#   scale_x_datetime("", date_labels = "%m-%Y", date_breaks = "1 month") +
#   # scale_color_viridis_d(labels = c("CBC", "Toronto Star"),
#   #                       limits = c("CBC", "TStar")) +
#   theme_clean() +
#   # labs(title = "Évolution du ton des articles sur le cannabis dans les médias anglophones (1985 à 2019)",
#   #      # subtitle = "Version 1: aucune pondération.",
#   #      subtitle = "",
#   #      # caption = "Dictionnaire: version francophone du Lexicoder (Duval et Pétry, 2016)
#   #      # Pondéré en fonction du nombre moyen de mots par mois, après nettoyage des textes et application du dictionnaire") +
#   #      caption = "n = 14 771 articles
#   #      Dictionnaire: Lexicoder (Soroka and Young, 2015)
#   #      L'indice de ton est calculé en fonction de la différence entre les proportions de mots positifs et négatifs par mois") +
#   ylab("Proportion de mots négatifs\n") +
#   theme(plot.title = element_text(hjust = -1.5, size = 18),
#         plot.subtitle=element_text(size = 13),
#         plot.caption = element_text(hjust = 1, size = 11),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.title.y = element_text(size = 14),
#         legend.position = "top") +
#   guides(color=guide_legend(title="Country"))
#
# ggsave("../_SharedFolder_article_syrie-ukraine/_v2/lexicoder/lexicoder-ukraine.png", width = 30, height = 17, units = c("cm"))

########################################################################################################### ##
########################################## Sentiment EN Opinion Syrie Vs Opinion Ukraine #####################
########################################################################################################### ##

# Créer une fonction pour analyser le ton des mots avec le Lexicoder Sentiment Dictionary
runDictionaryFunction <- function(corpusA, dataA, word, dfmA, dataB, dictionaryA) {
  corpusA <- corpus(dataA$word)
  dfmA    <- dfm(corpusA, dictionary = dictionaryA)
  dataB   <- convert(dfmA, to = "data.frame")
  return(dataB)
}

# Pour changer un DTM en format tidy, on utilise tidy() du broom package.
data_tidy <- tidy(cleanUkraine_corp) %>%
  bind_cols(dataUkraine) %>%
  select(-author, -datetimestamp, -description, -heading, -language, -origin, -doc_id, -id, -text...10) %>%
  rename(text = text...8) %>%
  unnest_tokens(word, text)

# Pour obtenir des %
polarity <- data_tidy %>%
  mutate(date = as.POSIXct(date))  %>%
  group_by(date) %>%
  mutate(total_words = n())

# On utilise le lexicoder anglo du package de Quanteda, qui se nomme data_dictionary_LSD2015
data_ton <- runDictionaryFunction(dataA = polarity,
                                  word = word,
                                  dictionaryA = data_dictionary_LSD2015)

# Pour le top négatif et positif
topcount <- bind_cols(polarity, data_ton) %>%
  group_by(word) %>%
  summarise(negative = sum(negative),
            positive = sum(positive))

# Ukraine
graphdata_Ukraine <- bind_cols(polarity, data_ton) %>%
  group_by(date, total_words) %>%
  filter(opinion == 0) %>%
  summarise(negative = sum(negative),
            positive = sum(positive)) %>%
  mutate(propNeg = (negative/total_words),
         propPos = (positive/total_words)) %>%
  mutate(diffProp = propPos - propNeg) %>%
  mutate(country = "Ukraine")

# Syria
graphdata_Syria <- bind_cols(polarity, data_ton) %>%
  group_by(date, total_words) %>%
  filter(opinion == 0) %>%
  summarise(negative = sum(negative),
            positive = sum(positive)) %>%
  mutate(propNeg = (negative/total_words),
         propPos = (positive/total_words)) %>%
  mutate(diffProp = propPos - propNeg) %>%
  mutate(country = "Syria")

# On met tous ensemble et on calcule
graphdata <- graphdata_Ukraine %>%
  bind_rows(graphdata_Syria) %>%
  group_by(country) %>%
  summarise(
    n=n(),
    mean=mean(diffProp),
    sd=sd(diffProp)) %>%
  mutate(se=sd/sqrt(n))  %>%
  mutate(ic=se * qt((1-0.05)/2 + .5, n-1))


# Standard deviation
ggplot(graphdata) +
  geom_bar( aes(x=country, y=mean), stat="identity", fill="forestgreen", alpha=0.5) +
  geom_errorbar( aes(x=country, ymin=mean-sd, ymax=mean+sd), width=0.4, colour="orange", alpha=0.9, size=1.5) +
  ggtitle("using standard deviation")

# Standard Error
ggplot(graphdata) +
  geom_bar( aes(x=country, y=mean), stat="identity", fill="forestgreen", alpha=0.5) +
  geom_errorbar( aes(x=country, ymin=mean-se, ymax=mean+se), width=0.4, colour="orange", alpha=0.9, size=1.5) +
  ggtitle("using standard error")

# Confidence Interval
ggplot(graphdata) +
  geom_bar( aes(x=country, y=mean), stat="identity", fill="forestgreen", alpha=0.5) +
  geom_errorbar( aes(x=country, ymin=mean-ic, ymax=mean+ic), width=0.4, colour="orange", alpha=0.9, size=1.5) +
  ggtitle("using confidence interval")

# # On plot
# ggplot(graphdata, aes(x = date, y = diffProp)) +
#   geom_hline(yintercept = 0, color = "red") +
#  geom_line() +
#  # geom_point(alpha = 0.4) +
#   geom_smooth(se = F, span = 0.4, size = 2.5) +
#   scale_x_datetime("", date_labels = "%m-%Y", date_breaks = "1 month") +
#   # scale_color_viridis_d(labels = c("CBC", "Toronto Star"),
#   #                       limits = c("CBC", "TStar")) +
#   theme_clean() +
#   # labs(title = "Évolution du ton des articles sur le cannabis dans les médias anglophones (1985 à 2019)",
#   #      # subtitle = "Version 1: aucune pondération.",
#   #      subtitle = "",
#   #      # caption = "Dictionnaire: version francophone du Lexicoder (Duval et Pétry, 2016)
#   #      # Pondéré en fonction du nombre moyen de mots par mois, après nettoyage des textes et application du dictionnaire") +
#   #      caption = "n = 14 771 articles
#   #      Dictionnaire: Lexicoder (Soroka and Young, 2015)
#   #      L'indice de ton est calculé en fonction de la différence entre les proportions de mots positifs et négatifs par mois") +
#   ylab("Proportion de mots négatifs\n") +
#   theme(plot.title = element_text(hjust = -1.5, size = 18),
#         plot.subtitle=element_text(size = 13),
#         plot.caption = element_text(hjust = 1, size = 11),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.title.y = element_text(size = 14),
#         legend.position = "top") +
#   guides(color=guide_legend(title="Country"))
#
# ggsave("../_SharedFolder_article_syrie-ukraine/_v2/lexicoder/lexicoder-ukraine.png", width = 30, height = 17, units = c("cm"))


########################################################################################################### ##
################################################## Time series ###############################################
########################################################################################################### ##

library(stats)

days <- seq(1, 365)
date <- as.Date(days, origin = "2014-12-31")
dates_syria <- data.frame(date)

timeSeries_syrie <- graphdata_Syrie %>%
  mutate(date = as.Date(date)) %>%
  select(date, diffProp) %>%
  full_join(dates_syria, by = "date") %>%
  arrange(date) %>%
  mutate(country = "Syrie")

days <- seq(1, 365)
date <- as.Date(days, origin = "2021-12-31")
dates_ukraine <- data.frame(date)

timeSeries_ukraine <- graphdata_ukraine %>%
  mutate(date = as.Date(date)) %>%
  select(date, diffProp) %>%
  full_join(dates_ukraine, by = "date") %>%
  arrange(date) %>%
  mutate(country = "Ukraine")

timeSeries <- timeSeries_syrie %>%
  bind_rows(timeSeries_ukraine)

aov_result <- aov(diffProp ~ date, data = timeSeries)
summary(aov_result)

timeSeries_graph <- dates_ukraine %>%
  bind_cols(timeSeries_syrie) %>%
  select(-date...2, date = date...1) %>%
  bind_rows(timeSeries_ukraine) %>%
  mutate(date = as.POSIXct(date, format = "%m-%d")) %>%
  na.omit() %>%
  group_by(country) %>%
  mutate(mean = mean(diffProp))


# On plot
ggplot(timeSeries_graph, aes(x = date, y = diffProp, color = country)) +
  geom_hline(yintercept = 0, color = "red") +
 # geom_line() +
   geom_point(alpha = 0.4) +
  geom_smooth(se = T, span = 0.6, size = 2.5) +
  scale_x_datetime("", date_labels = "%d-%m", date_breaks = "1 month") +
  # scale_color_viridis_d(labels = c("CBC", "Toronto Star"),
  #                       limits = c("CBC", "TStar")) +
  theme_clean() +
  # labs(title = "Évolution du ton des articles sur le cannabis dans les médias anglophones (1985 à 2019)",
  #      # subtitle = "Version 1: aucune pondération.",
  #      subtitle = "",
  #      # caption = "Dictionnaire: version francophone du Lexicoder (Duval et Pétry, 2016)
  #      # Pondéré en fonction du nombre moyen de mots par mois, après nettoyage des textes et application du dictionnaire") +
  #      caption = "n = 14 771 articles
  #      Dictionnaire: Lexicoder (Soroka and Young, 2015)
  #      L'indice de ton est calculé en fonction de la différence entre les proportions de mots positifs et négatifs par mois") +
  ylab("Tone Index\n") +
  theme(plot.title = element_text(hjust = -1.5, size = 18),
        plot.subtitle=element_text(size = 13),
        plot.caption = element_text(hjust = 1, size = 11),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 14),
        legend.position = "top") +
  guides(color=guide_legend(title="Country"))

ggsave("../_SharedFolder_article_syrie-ukraine/_v2/timeSeries/time-series.png", width = 30, height = 17, units = c("cm"))

########################################################################################################### ##
################################################ Sentiment NRC ###############################################
########################################################################################################### ##

# Pour changer un DTM en format tidy, on utilise tidy() du broom package.
syrie_tidy <- tidy(dataSyrie2015_dtm)

# Sortir le dictiononaire
nrc <- get_sentiments("nrc")

syrie_nrc <- syrie_tidy %>%
  inner_join(nrc, by = c("term" = "word")) %>%
  filter(!sentiment %in% c("positive", "negative")) %>%
  group_by(sentiment) %>%
  summarize(total_count = sum(count))

# Plot total_count vs. sentiment
ggplot(syrie_nrc, aes(x = sentiment, y = total_count)) +
  geom_col()

#ggsave("graphs/_v2/nrc/syria_nrc.png", width = 30, height = 17, units = c("cm"))

### Syrie

syrie_nrc_time <- syrie_tidy %>%
  rename(doc_id = document) %>%
  left_join(dataSyrie2015, by = "doc_id") %>%
  group_by(date) %>%
  mutate(total_words = n(),
         date = as.POSIXct(date)) %>%
  inner_join(nrc, by = c("term" = "word")) %>%
  filter(sentiment %in% c("fear", "sadness")) %>%
  group_by(date, sentiment, total_words) %>%
  summarize(total_count = sum(count)) %>%
  mutate(propSentiment = total_count/total_words)
  # pivot_wider(id_cols = date, names_from = sentiment, values_from = propSentiment) %>%
  # mutate(diffProp = positive - negative)

### Ukraine

# Pour changer un DTM en format tidy, on utilise tidy() du broom package.
ukraine_tidy <- tidy(dataUkraine_dtm)

# Sortir le dictiononaire
nrc <- get_sentiments("nrc")

ukraine_nrc <- ukraine_tidy %>%
  inner_join(nrc, by = c("term" = "word")) %>%
  filter(!sentiment %in% c("positive", "negative")) %>%
  group_by(sentiment) %>%
  summarize(total_count = sum(count))

# Plot total_count vs. sentiment
ggplot(ukraine_nrc, aes(x = sentiment, y = total_count)) +
  geom_col()

#ggsave("graphs/_v2/nrc/ukraine_nrc.png", width = 30, height = 17, units = c("cm"))

ukraine_nrc_time <- ukraine_tidy %>%
  rename(doc_id = document) %>%
  left_join(dataUkraine, by = "doc_id") %>%
  group_by(date) %>%
  mutate(total_words = n(),
         date = as.POSIXct(date)) %>%
  inner_join(nrc, by = c("term" = "word")) %>%
  filter(sentiment %in% c("fear", "sadness")) %>%
  group_by(date, sentiment, total_words) %>%
  summarize(total_count = sum(count)) %>%
  mutate(propSentiment = total_count/total_words)

# On plot
plotNRC1 <- ggplot(syrie_nrc_time, aes(x = date, y = propSentiment, color = sentiment)) +
 # geom_hline(yintercept = 0, color = "red") +
  geom_line() +
  geom_smooth(se = F, span = 0.6, size = 2.5) +
  scale_x_datetime("", date_labels = "%b", date_breaks = "1 month") +
  geom_vline(xintercept = as.POSIXct("2015-10-01"), color = "darkgrey", size = 0.8  , linetype = "dashed") +
  geom_text(x = as.POSIXct("2015-10-01"), y = 0.225, label = "Migration peak", angle = 90, vjust = -0.60, show.legend = F) +
  geom_vline(xintercept = as.POSIXct("2015-09-02"), color = "darkgrey", size = 0.8  , linetype = "dashed") +
  geom_text(x = as.POSIXct("2015-09-02"), y = 0.219, label = "Death of Alan Kurdi", angle = 90, vjust = -0.60, show.legend = F) +
  geom_vline(xintercept = as.POSIXct("2015-11-13"), color = "darkgrey", size = 0.8  , linetype = "dashed") +
  geom_text(x = as.POSIXct("2015-11-13"), y = 0.2275, label = "Paris attacks", angle = 90, vjust = -0.60, show.legend = F) +
  scale_y_continuous(name="Proportion of words related to sentiment\n",
                     breaks = c(0, 0.05, 0.1, 0.15, 0.20, 0.25),
                     limits = c(0, 0.26),
                     expand = c(0,0)) +
  scale_color_manual(values = c("fear" = "#FF0000", "sadness" = "#0000FF")) +
  theme_clean() +
  ggtitle("Syria (2015)") +
  theme(plot.title = element_text(size = 18),
        plot.subtitle=element_text(size = 13),
        plot.caption = element_text(hjust = 1, size = 11),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 14),
        legend.position = "none",
        panel.border = element_blank(),
        legend.margin = margin(6, 6, 6, 6))

# On plot
plotNRC2 <- ggplot(ukraine_nrc_time, aes(x = date, y = propSentiment, color = sentiment)) +
  geom_hline(yintercept = 0, color = "red") +
  geom_line() +
  geom_smooth(se = F, span = 0.6, size = 2.5) +
  geom_vline(xintercept = as.POSIXct("2022-03-15"), color = "darkgrey", size = 0.8  , linetype = "dashed") +
  geom_text(x = as.POSIXct("2022-03-15"), y = 0.225, label = "Migration peak", angle = 90, vjust = -0.6, show.legend = F) +
  scale_y_continuous(name="",
                     breaks = c(0, 0.05, 0.1, 0.15, 0.20, 0.25),
                     limits = c(0, 0.26),
                     expand = c(0,0)) +
  scale_color_manual(values = c("fear" = "#FF0000", "sadness" = "#0000FF")) +
  scale_x_datetime("", date_labels = "%b", date_breaks = "1 month") +
  theme_clean() +
  ggtitle("Ukraine (2022)") +
  theme(plot.title = element_text(size = 18),
        plot.subtitle=element_text(size = 13),
        plot.caption = element_text(hjust = 1, size = 11),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 14),
        legend.position = c(.85, .99),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6)) +
  guides(color=guide_legend(title="Sentiments"))

plot <- grid.arrange(plotNRC1, plotNRC2, nrow = 1)


ggsave("../_SharedFolder_article_syrie-ukraine/graphs/_v2/nrc/nrc-syrie-ukraine.png", plot, width = 30, height = 17, units = c("cm"))



########################################################################################################### ##
################################################# Topic modeling #############################################
########################################################################################################### ##

# D'abord, on a besoin d'un dtm  (voir section DTM/TDM)

# Run une LDA (Latent dirichlet allocation) avec 2 sujets et un «sampler» Gibbs
# seed: set a seed so that the output of the model is predictable (random seed for reproducability)
# α : Parameter that sets the topic distribution for the documents,
# the higher the more spread out the documents will be across the specified number of topics (K).
# η : Parameter that sets the topic distribution for the words,
# the higher the more spread out the words will be across the specified number of topics (K).

# On ne veut pas nécessairement être trop restrictif dans nos thèmes, puisque notre analyse est «par document»
# et que dans chacun de nos document, on a beaucoup d'articles, donc beaucoup de thèmes. On peut laisser un
# peu de liberté à notre alpha.

# Test 1, avec paramètres de bases selon tidy R
# lda_out <- LDA(
#   Weed_dtmTStar,
#   k = 3,
#   method = "Gibbs",
#   control = list(seed = 64)
#   )

# test 2 avec les paramètres du cours datacamp Topic Modeling (et j'ai mis seed 64, au lieu de 12345)
lda_out <- LDA(
  dataUkraine_dtm,
#  dataUkraine_dtm,
  k = 3,
  method = "Gibbs",
  control = list(alpha = 0.1,
                 delta = 0.1,
                 seed = 64))

# On jette un coup d'oeil
# glimpse(lda_out2)

# On tidy la matrice des probabilités des mots (bêta)
lda_topics <- lda_out %>%
  tidy(matrix="beta") %>%
  arrange(desc(beta))

# Sélectionnez les 15 premiers termes par topic et on les réorganise
word_probs <- lda_topics %>%
  group_by(topic) %>%
  top_n(15, beta) %>%
  ungroup() %>%
  mutate(term2 = fct_reorder(term, beta))

word_probs <- as.data.frame(word_probs)

# setter l'ordres des thèmes
#order <- c("War with Russia", "Impact on populations", "International response")
#order <- c("European migration", "Terrorism", "Refugee (new) life", "British politics", "Canadian politics")

# Graphiques de cadre
#word_probs$topic2 <- NA
#word_probs$topic2[word_probs$topic==1]<- "War with Russia"
#word_probs$topic2[word_probs$topic==2]<- "Impact on populations"
#word_probs$topic2[word_probs$topic==3]<- "International response"

# word_probs$topic2 <- NA
# word_probs$topic2[word_probs$topic==1]<- "European migration"
# word_probs$topic2[word_probs$topic==2]<- "Terrorism"
# word_probs$topic2[word_probs$topic==3]<- "Refugee (new) life"
# word_probs$topic2[word_probs$topic==4]<- "British politics"
# word_probs$topic2[word_probs$topic==5]<- "Canadian politics"

#word_probs$topic <- factor(word_probs$topic2, levels = order)

#levels(word_probs$topic) <- order

# on plot par topic
ggplot(word_probs, aes(x=term2, y=beta, fill=as.factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~topic, scales = "free") +
  coord_flip() +
  # labs(title = "Modélisation thématique dans les articles sur le cannabis des médias francophones (1988-2019)",
  #      caption = "n = 15 184 articles",
  #     # caption = "n = 14 771 articles",
  #      x="") +
  ylab("\nProbability (beta) of words being associated with each topic") +
  xlab("") +
  #  scale_fill_manual(values = c("#9e0015", "#9e9300")) +
  #  scale_fill_manual(values = c("#20a8a0", "#c32c6a", "#2b0057")) +
#  scale_fill_manual(values = c("#440154FF", "#33638CFF", "#1F968BFF", "#FDE725FF")) +
  #  scale_fill_manual(values = c("#6f2194", "#9e9300", "#026629", "#002d9e", "#9e0015")) +
  #  scale_fill_manual(values = c("#9e0015", "#9e9300", "#002d9e", "#026629", "#9e4400", "#6f2194")) +
  ggthemes::theme_clean() +
  theme(plot.title = element_text(hjust = -0.2, size = 16),
        plot.subtitle=element_text(size = 13),
        plot.caption = element_text(hjust = 1, size = 11),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 11),
        axis.title.x = element_text(size = 14),
        strip.text.x = element_text(size = 15))

ggsave("../_SharedFolder_article_syrie-ukraine/graphs/_v2/lda_syrie_2015/3k_LDAbeta-syrie2015.png", width = 30, height = 17, units = c("cm"))

# renvoie les probabilités des topics.
# posterior(lda_out2)$topics

# Voir les probabilités d'appartenance d'un mot aux différents topics
# tidy(mod, matrix="beta") %>%
#   filter(term == "government")

# On plot (plus nice)
word_probs2 <- tidy(lda_out, "gamma") %>%
  rename(doc_id = document) %>%
  left_join(dataUkraine_op, by = "doc_id") %>%
  mutate(date = as.POSIXct(date))

#word_probs2$topic2 <- NA
#word_probs2$topic2[word_probs2$topic==1]<- "War with Russia"
#word_probs2$topic2[word_probs2$topic==2]<- "Impact on populations"
#word_probs2$topic2[word_probs2$topic==3]<- "International response"

# word_probs2$topic2 <- NA
# word_probs2$topic2[word_probs2$topic==1]<- "European migration"
# word_probs2$topic2[word_probs2$topic==2]<- "Terrorism"
# word_probs2$topic2[word_probs2$topic==3]<- "Refugee (new) life"
# word_probs2$topic2[word_probs2$topic==4]<- "British politics"
# word_probs2$topic2[word_probs2$topic==5]<- "Canadian politics"

#word_probs2$topic <- factor(word_probs2$topic2, levels=order)

ggplot(word_probs2, aes(x = date, y = gamma)) +
  #  geom_point(aes(color=factor(topic)), alpha = 0.3) +
  geom_smooth(se = F, aes(color=factor(topic)), size = 2.5) +
  # labs(title = "Modélisation thématique dans les articles sur le cannabis dans les médias francophones (1988-2019)",
  #      caption = "n = 15 184 articles") +
  # caption = "n = 14 771 articles",
  ylab("Probability (gamma) of articles being associated with each topic\n") +
  # scale_color_manual(name = "Thèmes",
  #                      labels = c("Économie", "Criminalité", "Politique"),
  #                      values = c("#9e9300", "#9e0015", "#002d9e")) +
 # scale_color_manual(name = "Topics",
  #                    #   labels = c("Légalisation", "Criminalité"),
  #                    #  values = c("#9e0015", "#9e9300")) +
#  values = c("#20a8a0", "#c32c6a", "#2b0057"))+
  #                    values = c("#440154FF", "#33638CFF", "#1F968BFF", "#FDE725FF")) +
  #  values = c("#9e0015", "#9e9300", "#002d9e", "#026629")) +
  # values = c("#9e0015", "#9e9300", "#002d9e", "#026629", "#9e4400")) +
#  values = c("#6f2194", "#9e9300", "#026629", "#002d9e", "#9e0015")) +
#  geom_vline(xintercept = as.POSIXct("2022-03-15"), color = "darkgrey", size = 0.8  , linetype = "dashed") +
#  geom_text(x = as.POSIXct("2022-03-15"), y = 0.468, label = "Migration peak", angle = 90, vjust = -0.6, show.legend = F) +
  scale_x_datetime("", date_labels = "%m-%Y", date_breaks = "2 month") +
  # geom_vline(xintercept = as.POSIXct("2015-10-01"), color = "darkgrey", size = 0.8  , linetype = "dashed") +
  # geom_text(x = as.POSIXct("2015-10-01"), y = 0.358, label = "Migration peak", angle = 90, vjust = -0.60, show.legend = F) +
  # geom_vline(xintercept = as.POSIXct("2015-09-02"), color = "darkgrey", size = 0.8  , linetype = "dashed") +
  # geom_text(x = as.POSIXct("2015-09-02"), y = 0.345, label = "Death of Alan Kurdi", angle = 90, vjust = -0.60, show.legend = F) +
  # geom_vline(xintercept = as.POSIXct("2015-11-13"), color = "darkgrey", size = 0.8  , linetype = "dashed") +
  # geom_text(x = as.POSIXct("2015-11-13"), y = 0.362, label = "Paris attacks", angle = 90, vjust = -0.60, show.legend = F) +
  theme_clean() +
  theme(plot.title = element_text(hjust = 0.15, size = 16),
        plot.subtitle=element_text(size = 13),
        plot.caption = element_text(hjust = 1.19, size = 11),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 14),
        legend.position = "right") +
  guides(alpha = FALSE)

#  scale_color_manual(values=brewer.pal(n=4, "Set1"), name="Topic")
ggsave("../_SharedFolder_article_syrie-ukraine/graphs/_v2/lda_syrie_2015/3k_LDAgamma-syrie2015.png", width = 30, height = 17, units = c("cm"))

########################################################################################################### ##
################################################## Regressions ###############################################
########################################################################################################### ##

# Créer une fonction pour analyser le ton des mots avec le Lexicoder Sentiment Dictionary
runDictionaryFunction <- function(corpusA, dataA, word, dfmA, dataB, dictionaryA) {
  corpusA <- corpus(dataA$word)
  dfmA    <- dfm(corpusA, dictionary = dictionaryA)
  dataB   <- convert(dfmA, to = "data.frame")
  return(dataB)
}

family <- c("women", "woman", "female", "mother", "mothers", "mom", "moms", "father", "fathers", "dad", "dads",
            "child", "children", "kid", "kids", "girl", "girls", "boy", "boys",
            "family", "families", "parent", "parents", "sibling", "siblings", "brother", "brothers", "sister", "sisters",
            "spouse", "spouses", "husband", "husbands", "son", "sons", "daughter", "daughters",
            "grandparent", "grandparents", "grandmother", "grandmothers", "grandfather", "grandfathers",
            "grandchild", "grandchildren", "niece", "nieces", "nephew", "nephews",
            "aunt", "aunts", "uncle", "uncles")

men <- c("men", "man", "male")

# Pour changer un DTM en format tidy, on utilise tidy() du broom package.
data_tidy_ukraine <- tidy(cleanUkraine_corp) %>%
  bind_cols(dataUkraine) %>%
  select(-author, -datetimestamp, -description, -heading, -language, -origin, -doc_id, -id, -text...10) %>%
  rename(text = text...8) %>%
  unnest_tokens(word, text)

# Pour obtenir des %
polarity_ukraine <- data_tidy_ukraine %>%
  mutate(date = as.POSIXct(date))  %>%
  group_by(id_sentence) %>%
  mutate(total_words_sentence = n()) %>%
  ungroup() %>%
  group_by(date) %>%
  mutate(total_words_day1000 = n()/1000)


# On utilise le lexicoder anglo du package de Quanteda, qui se nomme data_dictionary_LSD2015
data_ton_ukraine <- runDictionaryFunction(dataA = polarity_ukraine,
                                  word = word,
                                  dictionaryA = data_dictionary_LSD2015)

# Ukraine
graphdata_ukraine <- bind_cols(polarity_ukraine, data_ton_ukraine) %>%
  mutate(family = ifelse(word %in% family, 1, 0),
         men = ifelse(word %in% men, 1, 0)) %>%
  group_by(id_sentence) %>%
  mutate(family_sum = sum(family),
         men_sum = sum(men)) %>%
  ungroup() %>%
  group_by(id_sentence, date, total_words_sentence, source, opinion, country, media_country, total_words_day1000, family_sum, men_sum) %>%
  summarise(negative = sum(negative),
            positive = sum(positive)) %>%
  mutate(propNeg = (negative/total_words_sentence),
         propPos = (positive/total_words_sentence)) %>%
  mutate(ton = propPos - propNeg) %>%
  mutate(country = "Ukraine") %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%
  inner_join(nb_refugies_day, by = c("country", "date")) %>%
  mutate(date = ymd(date),
         days_since_conflict_start100 = as.numeric((date - start_date_ukraine)/100))

# Pour changer un DTM en format tidy, on utilise tidy() du broom package.
data_tidy_iraq <- tidy(cleanIraq_corp) %>%
  bind_cols(dataIraq) %>%
  select(-author, -datetimestamp, -description, -heading, -language, -origin, -doc_id, -id, -text...10) %>%
  rename(text = text...8) %>%
  unnest_tokens(word, text)

# Pour obtenir des %
polarity_iraq <- data_tidy_iraq %>%
  mutate(date = as.POSIXct(date))  %>%
  group_by(id_sentence) %>%
  mutate(total_words_sentence = n()) %>%
  ungroup() %>%
  group_by(date) %>%
  mutate(total_words_day1000 = n()/1000)

# On utilise le lexicoder anglo du package de Quanteda, qui se nomme data_dictionary_LSD2015
data_ton_iraq <- runDictionaryFunction(dataA = polarity_iraq,
                                          word = word,
                                          dictionaryA = data_dictionary_LSD2015)

# Iraq
graphdata_iraq <- bind_cols(polarity_iraq, data_ton_iraq) %>%
  mutate(family = ifelse(word %in% family, 1, 0),
         men = ifelse(word %in% men, 1, 0)) %>%
  group_by(id_sentence) %>%
  mutate(family_sum = sum(family),
         men_sum = sum(men)) %>%
  ungroup() %>%
  group_by(id_sentence, date, total_words_sentence, source, opinion, country, media_country, total_words_day1000, family_sum, men_sum) %>%
  summarise(negative = sum(negative),
            positive = sum(positive)) %>%
  mutate(propNeg = (negative/total_words_sentence),
         propPos = (positive/total_words_sentence)) %>%
  mutate(ton = propPos - propNeg) %>%
  mutate(country = "Iraq") %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%
 # inner_join(nb_refugies_day, by = c("country", "date")) %>%
  mutate(date = ymd(date),
         days_since_conflict_start100 = as.numeric((date - start_date_iraq))/100)

# Pour changer un DTM en format tidy, on utilise tidy() du broom package.
data_tidy_syria <- tidy(cleanSyrie_corp) %>%
  bind_cols(dataSyrie) %>%
  select(-author, -datetimestamp, -description, -heading, -language, -origin, -doc_id, -id, -text...10) %>%
  rename(text = text...8) %>%
  unnest_tokens(word, text)

# Pour obtenir des %
polarity_syria <- data_tidy_syria %>%
  mutate(date = as.POSIXct(date))  %>%
  group_by(id_sentence) %>%
  mutate(total_words_sentence = n()) %>%
  ungroup() %>%
  group_by(date) %>%
  mutate(total_words_day1000 = n()/1000)

# On utilise le lexicoder anglo du package de Quanteda, qui se nomme data_dictionary_LSD2015
data_ton_syria <- runDictionaryFunction(dataA = polarity_syria,
                                        word = word,
                                        dictionaryA = data_dictionary_LSD2015)

# Syria
graphdata_syria <- bind_cols(polarity_syria, data_ton_syria) %>%
  mutate(family = ifelse(word %in% family, 1, 0),
         men = ifelse(word %in% men, 1, 0)) %>%
  group_by(id_sentence) %>%
  mutate(family_sum = sum(family),
         men_sum = sum(men)) %>%
  ungroup() %>%
  group_by(id_sentence, date, total_words_sentence, source, opinion, country, media_country, total_words_day1000, family_sum, men_sum) %>%
  summarise(negative = sum(negative),
            positive = sum(positive)) %>%
  mutate(propNeg = (negative/total_words_sentence),
         propPos = (positive/total_words_sentence)) %>%
  mutate(ton = propPos - propNeg) %>%
  mutate(country = "Syria") %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%
  inner_join(nb_refugies_day, by = c("country", "date")) %>%
  mutate(date = ymd(date),
         days_since_conflict_start100 = as.numeric((date - start_date_syria))/100)

reg <- bind_rows(graphdata_syria, graphdata_ukraine, graphdata_iraq) %>%
  mutate(year = substr(date, 1, 4))



# factorisé les variable pour déterminer les classes de référence
reg$source <- factor(reg$source)
reg <- within(reg, source <- relevel(source, ref = "The New York Times"))

reg$country <- factor(reg$country)
reg <- within(reg, country <- relevel(country, ref = "Syria"))

reg$media_country <- factor(reg$media_country)
reg <- within(reg, media_country <- relevel(media_country, ref = "Canada"))

# Modèle 1
model_1 <- lm(ton ~ country, data = reg)
summary(model_1)

# Modèle 2
model_2 <- lm(ton ~ country + as.factor(year), data = reg)
summary(model_2)

# # Modèle 2
# model_2 <- lm(ton ~ country + refugies1000, data = reg)
# summary(model_2)
#
# # Modèle 3
# model_3 <- lm(ton ~ country + refugies1000 + source, data = reg)
# summary(model_3)

# Modèle 4: Juste classe
model_3 <- lm(ton ~ country + as.factor(year) + source +
                total_words_day1000 + opinion + family_sum + men_sum + days_since_conflict_start100, data = reg)
summary(model_3)

# Stargazer pour clusters
stargazer(model_1, model_2, model_3,
          type = 'latex',

          header=FALSE, # to get rid of r package output text

          single.row = TRUE, # to put coefficients and standard errors on same line

          no.space = T, # to remove the spaces after each line of coefficients

          column.sep.width = "1pt", # to reduce column width

          font.size = "footnotesize" # to make font size smaller

)


# 9.2. Faire un tableau LaTeX avec Stargazer
# stargazer(model1, model2, model3, model4, model5, model6, model7,
#           title='Tests des hypothèses sur les déterminants du vote pour le NPD',
#           order=c("leadFeelNdp11", "rightistScale", "quebec", "female",
#                   "langFr", "langOth", "age34m", "age55p", "incomeHigh",
#                   "incomeLow", "educBHS", "educUniv"),
#           covariate.labels=c("Évaluation du chef NPD", "Droite idéologique", "Québec", "Femme",
#                              "Francophone", "Allophone", "Moins de 34 ans", "Plus de 55 ans", "Haut revenu",
#                              "Faible revenu", "Pas de diplôme secondaire", "Diplôme universitaire",
#                              "\\_constante"),
#           font.size="scriptsize", report="vc*s", nobs=TRUE, no.space=TRUE,
#           digits.extra=1, digits=2,
#           dep.var.labels="Vote pour le NPD",
#           omit.stat=c("f","ser","rsq","adj.rsq"),
#           style="apsr",
#           star.cutoffs=c(0.05,0.01,0.001),
#           notes.append=FALSE, notes.align="l",
#           notes.label="", # Remove the "Note:": By default in a table
#           notes=c("\\emph{Source}: Étude électorale canadienne, 2011.",
#                   "\\emph{Note}: Régression logistique binomiale.",
#                   "$^{*}$p$<$0.05; $^{**}$p$<$0.01; $^{***}$p$<$0.001"))

saveRDS(reg, "_SharedFolder_article_syrie-ukraine/Data/data_descriptive.RDS")


########################################################################################################### ##
#################################################### Models ##################################################
########################################################################################################### ##

# Pas besoin de tout rouler ci-dessus pour faire de belles régressions avec plein de contrôle!
# la bd ci-dessous est toute prêt, roulée à partir de toutes les lignes ci-dessus.
reg_controls <- read_rds("_SharedFolder_article_syrie-ukraine/Data/control_dataset/data_regression_controls.RDS") %>%
  mutate(month = substr(date, 6, 7),
         year = substr(date, 1, 4))


# library(rpart)
# library(rpart.plot)
#
# tree = rpart(ton ~ country  + source +
#                opinion + family_sum + men_sum
#               , data = reg, method = "anova", cp = 0.0009)
# rpart.plot(tree)

# Modèle 1
model_1 <- lm(ton ~ country, data = reg_controls)
summary(model_1)

# Modèle 2
# model_2 <- lm(ton ~ country + refugies1000, data = reg)
# summary(model_2)

# Modèle 3
model_3 <- lm(ton ~ country + christian100, data = reg_controls)
summary(model_3)

# Modèle 4: Juste classe
model_4 <- lm(ton ~ country + refugies1000 + source +
                total_words_day1000 + opinion + family_sum + men_sum + days_since_conflict_start100 +
                christian100 + muslim100 + non_religious100 + information_flow + globalization_index + migrant100 + pop_density + pop_urban_density, data = reg_controls)
summary(model_4)

# Modèle 4: Juste classe
model_5 <- lm(ton ~ country + media_country + as.factor(year) +
                total_words_day1000 + opinion + family_sum + men_sum + days_since_conflict_start100, data = reg_controls)
summary(model_5)

 # Stargazer pour clusters
stargazer(model_1, model_2, model_3, model_4,
          type = 'latex',

          header=FALSE, # to get rid of r package output text

          single.row = TRUE, # to put coefficients and standard errors on same line

          no.space = F, # to remove the spaces after each line of coefficients

          column.sep.width = "1pt", # to reduce column width

          font.size = "footnotesize" # to make font size smaller

)


########################################################################################################### ##
################################################### Prob prédites ############################################
########################################################################################################### ##

# Créer une base de données sans données manquantes
GraphData <- reg_controls[, c("ton", "country", "source", "men_sum", "family_sum", "opinion",
                     "total_words_day1000", "days_since_conflict_start100", "refugies1000")]

GraphData <- na.omit(GraphData)

# Modèle: ton en fonction du média
model_ton_source = lm(ton ~ source + country + men_sum + family_sum + opinion +
                        total_words_day1000 + days_since_conflict_start100 + refugies1000,
                      data=GraphData)

# Enregistrer les valeurs prédites
GraphData$predTone <- model_ton_source$fitted.values

library(ggplot2)

ggplot(GraphData, aes(x = source, y = predTone)) +
  geom_point(aes(y = predTone)) +
  geom_smooth(span = 0.95, method = "loess") +
  theme_classic() +
  scale_y_continuous(name="Ton Prédit") +
  scale_x_discrete(name="Média") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Utilisation de ggplot pour améliorer la visualisation
ggplot(GraphData, aes(x = source, y = predTone)) +
  geom_point(aes(y = predTone), alpha = 0.5, position = position_jitter(width = 0.2)) + # Ajout d'un jitter pour éviter le surimpression des points
  geom_smooth(method = "loess", se = T, color = "blue", fill = "lightblue") + # Ligne lisse avec marge d'erreur
  theme_classic() +
  labs(title = "Ton prédit en fonction du média",
       subtitle = "Avec intervalles de confiance à 95%") +
  scale_y_continuous(name="Ton Prédit") +
  scale_x_discrete(name="Média") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Calculer la moyenne et l'intervalle de confiance pour chaque source
library(dplyr)

GraphDataSummary <- GraphData %>%
  group_by(source) %>%
  summarise(mean_predTone = mean(predTone),
            se_predTone = sd(predTone) / sqrt(n()),
            lower_ci = mean_predTone - 1.96 * se_predTone,
            upper_ci = mean_predTone + 1.96 * se_predTone)

# Graphique à barres avec intervalles d'erreur
ggplot(GraphDataSummary, aes(x = source, y = mean_predTone)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "blue") +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, color = "blue") +
  theme_classic() +
  labs(title = "Ton prédit en fonction du média",
       subtitle = "Barres d'erreur représentant un intervalle de confiance à 95%") +
  scale_y_continuous(name="Ton Prédit Moyen") +
  scale_x_discrete(name="Média") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Modèle avec la variable men_sum en échelle continue
model_ton_men_continuous <- lm(ton ~ men_sum + country + source + family_sum + opinion +
                                 total_words_day1000 + days_since_conflict_start100 + refugies1000,
                               data=GraphData)

# Ajouter les valeurs prédites au jeu de données
GraphData$predTone_men_continuous <- model_ton_men_continuous$fitted.values

# Utilisation de ggplot pour créer le graphique
ggplot(GraphData, aes(x = men_sum, y = predTone_men_continuous, fill = as.factor(country), colour = as.factor(country))) +
  geom_point(aes(y = predTone_men_continuous), alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, aes(group = country)) +
  theme_classic() +
  theme(legend.position="top") +
  scale_fill_manual(values = c("Syria" = "blue", "Ukraine"="red")) +
  scale_colour_manual(values = c("Syria" = "blue", "Ukraine"="red")) +
  scale_x_continuous(name="Nombre de mentions des « hommes »") +
  scale_y_continuous(name="Ton Prédit")

########################################################################################################### ##
####################################################### FIN ##################################################
########################################################################################################### ##



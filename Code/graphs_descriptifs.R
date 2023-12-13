########################### ##
#### graphs descriptif médias ####
########################### ##

library(tidyverse)
library(lubridate)


# Loader les médias
data <- readRDS("Data/dict_media.rds") %>%
  rename_with(~ "date", 1)

## format de date 
data$date <- dmy(paste(data$date, "2022"))


#### Graph 1: Moyenne de chaque enjeux par média ####

# Calculer la moyenne de chaque enjeu par média
graph1 <- data %>%
  group_by(media) %>%
  summarise(across(macroeconomics:langue_fran, mean, na.rm = TRUE)) %>%
  pivot_longer(
    cols = macroeconomics:langue_fran, 
    names_to = "enjeu", 
    values_to = "moyenne"
  )

# Créer le graphique
ggplot(graph1, aes(x = enjeu, y = moyenne, fill = media)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  theme_minimal() +
  labs(title = "Moyenne de mots par enjeu et par média",
       x = "Enjeu",
       y = "Moyenne de mots") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_blank())


#### Graph 2 Total des mots par jour de chaque enjeu ####

graph2 <- data %>%
  pivot_longer(
    cols = macroeconomics:langue_fran, 
    names_to = "enjeu", 
    values_to = "nombre_mots"
  ) %>%
  group_by(date, enjeu) %>%
  summarise(total_mots = sum(nombre_mots, na.rm = TRUE))

# Création du graphique
ggplot(graph2, aes(x = date, y = total_mots, color = enjeu, group = enjeu)) +
  geom_line() +
  theme_minimal() +
  labs(title = "Évolution du nombre de mots par jour pour chaque enjeu",
       x = "Date",
       y = "Nombre total de mots") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_blank())


#### Graph 3 par enjeu ####

graph3 <- data %>%
  pivot_longer(
    cols = macroeconomics:langue_fran, 
    names_to = "enjeu", 
    values_to = "nombre_mots"
  ) %>%
  group_by(date, enjeu, media) %>%
  summarise(total_mots = sum(nombre_mots, na.rm = TRUE))

# Filtrer les données pour l'enjeu 'transportation'
data_transportation <- graph3 %>%
  filter(enjeu == "transportation")

# Créer le graphique
ggplot(data_transportation, aes(x = date, y = total_mots, color = media)) +
  geom_line() +
  theme_minimal() +
  labs(title = "Évolution du nombre de mots pour l'enjeu 'Transportation' par média",
       x = "",
       y = "Nombre de mots") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_blank(),
        legend.position = "bottom") +
  scale_color_brewer(palette = "Set1")  


#### Graph mean médias ####

## médias
mean_media <- data %>%
  summarise(across(macroeconomics:langue_fran, mean, na.rm = TRUE)) %>%
  pivot_longer(
    cols = macroeconomics:langue_fran, 
    names_to = "enjeu", 
    values_to = "moyenne"
  )

# Créer le graphique
ggplot(mean_media, aes(x = enjeu, y = moyenne)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  theme_minimal() +
  labs(title = "Moyenne de mots par enjeu pour tous les médias",
       x = "Enjeu",
       y = "Moyenne de mots") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_blank())

########################### ##
#### graphs descriptifs Tweet ####
########################## ###

## loader les tweets

data_tweets <- readRDS("Data/dict_tweets.rds") %>%
  rename_with(~ "date", 1)

## format de date 
data_tweets$date <- as.Date(data_tweets$date, format = "%Y-%m-%d")


#### graph 4 total par enjeux #### 
graph4 <- data_tweets %>%
  group_by(metadata.twitterHandle) %>%
  summarise(across(macroeconomics:langue_fran, mean, na.rm = TRUE)) %>%
  pivot_longer(
    cols = macroeconomics:langue_fran, 
    names_to = "enjeu", 
    values_to = "moyenne"
  )

# Créer le graphique
ggplot(graph4, aes(x = enjeu, y = moyenne, fill = metadata.twitterHandle)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  theme_minimal() +
  labs(title = "Moyenne de mots par enjeu et par média",
       x = "Enjeu",
       y = "Moyenne de mots") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_blank())

### graph 5 Nb de tweets par chefs/parti ####

graph5 <- data_tweets %>%
  group_by(metadata.twitterHandle) %>%
  summarise(n = n())

# Créer le graphique
ggplot(graph5, aes(x = metadata.twitterHandle, y = n)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  theme_minimal() +
  labs(title = "Nombre de Tweets par compte Twitter",
       x = "",
       y = "Moyenne de Tweets") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_blank())


##################### ###
#### Graphs croisés #### 
#################### ###

## médias
mean_media <- data %>%
  summarise(across(macroeconomics:langue_fran, mean, na.rm = TRUE)) %>%
  pivot_longer(
    cols = macroeconomics:langue_fran, 
    names_to = "enjeu", 
    values_to = "moyenne"
  ) %>%
  mutate(source = "Media") %>%
  select(source, enjeu, moyenne)

## tweets

mean_tweet <- data_tweets %>%
  group_by(metadata.twitterHandle) %>%
  rename(source = metadata.twitterHandle) %>%
  summarise(across(macroeconomics:langue_fran, mean, na.rm = TRUE)) %>%
  pivot_longer(
    cols = macroeconomics:langue_fran, 
    names_to = "enjeu", 
    values_to = "moyenne"
  )

combined_data <- bind_rows(mean_media, mean_tweet)

# Créer le graphique
ggplot(combined_data, aes(x = enjeu, y = moyenne, fill = source)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  theme_minimal() +
  labs(title = "Moyenne de mots par enjeu - Twitter et les médias",
       x = "Enjeu",
       y = "Moyenne de mots") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_blank())

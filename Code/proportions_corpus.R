library(tidyverse)
library(ggplot2)
library(clessnverse)
library(forcats)
library(patchwork)



# Loading datasets --------------------------------------------------------

data_media <- readRDS("//Users/jeremiedrouin/Dropbox/Travail/Universite_Laval/CLESSN/Publications/article_twitter-elxn22/_SharedFolder_article_twitter-elxn22/william/dict_media_final.rds")
data_tweets <- readRDS("/Users/jeremiedrouin/Dropbox/Travail/Universite_Laval/CLESSN/Publications/article_twitter-elxn22/_SharedFolder_article_twitter-elxn22/william/dict_tweets.rds")


##### Random checkup pour valider les dicos

datatweetsample <- data_tweets %>% 
  select("data.text","macroeconomics", "healthcare", "agriculture", "labour", 
         "immigration", "environment", "energy", "transportation", 
         "crime", "culture", "aboriginal", "religion", "education", "cost_life",
         "langue_fran" )

nombre_total_lignes <- nrow(datatweetsample)
nombre_lignes_selection <- 150
indices_aleatoires <- sample(1:nombre_total_lignes, nombre_lignes_selection)
dataset_selectionne <- datatweetsample[indices_aleatoires, ]

dataset_selectionne[1]







# pour les tweets ---------------------------------------------------------


data_tweets <- data_tweets %>% 
  select("macroeconomics", "healthcare", "agriculture", "labour", 
         "immigration", "environment", "energy", "transportation", 
         "crime", "culture", "aboriginal", "religion", "education", "cost_life",
         "langue_fran")


sum_row_tweets <- colSums(data_tweets)

# Add the sum row to the top of your dataset
data_tweets <- rbind(sum_row_tweets, data_tweets)


#### nouveau dataset avec les totaux 

tweets_prop <- data_tweets %>% 
  filter(row_number() == 1)

row_total_tweets <- sum(tweets_prop)

# Calculate the percentage for each variable based on the row total
percentage_tweets <- tweets_prop %>%
  mutate_all(function(x) (x / row_total_tweets) * 100)

names(percentage_tweets) <- c("Macro-\n economics", "Health-\n care", "Agriculture", "Labour", 
                              "Immigration", "Environment", "Energy", "Transportation", 
                              "Crime", "Culture", "Aboriginal \n affairs", "Religion",
                              "Education", "Cost of \n living", "French \n language")


percentage_tweets_long <- tidyr::gather(percentage_tweets, key = "variable", value = "percentage")

percentage_tweets_long <- percentage_tweets_long %>% 
  mutate_if(is.numeric, round)

# Create a bar plot using ggplot2
tweets_plot <- ggplot(percentage_tweets_long, aes(x = reorder(variable, percentage), y= percentage, fill = variable)) +
  geom_bar(stat = "identity", fill="blue" ) +
  labs(title = "",
       x = "",
       y = "Percentage") +
  geom_text(aes(label=percentage), position=position_dodge(width=0.9), vjust=-0.25, size= 7)+
  theme_clean_light()+
  theme(legend.position = "none",
        axis.text.x= element_text(angle=45, hjust = 1, size=20),
        axis.title.y = element_text(size=20))

ggsave("_SharedFolder_article_twitter-elxn22/graphs/tweets_prop.png", height= 10, width= 15)



  

# pour les médias ---------------------------------------------------------


data_media <- data_media %>% 
  select("macroeconomics", "healthcare", "agriculture", "labour", 
         "immigration", "environment", "energy", "transportation", 
         "crime", "culture", "aboriginal", "religion", "education", "cost_life",
         "langue_fran")

sum(data_media$macroeconomics)





sum_row <- colSums(data_media)

# Add the sum row to the top of your dataset
data_media <- rbind(sum_row, data_media)

#### nouveau dataset avec les totaux 

media_prop <- data_media %>% 
  filter(row_number() == 1)


# Calculate the total sum of all columns in the row
row_total <- sum(media_prop)

# Calculate the percentage for each variable based on the row total
percentage_media <- media_prop %>%
  mutate_all(function(x) (x / row_total) * 100)

names(percentage_media) <- c("Macro-\n economics", "Health-\n care", "Agriculture", "Labour", 
                             "Immigration", "Environment", "Energy", "Transportation", 
                             "Crime", "Culture", "Aboriginal \n affairs", "Religion",
                             "Education", "Cost of \n living", "French \n language")

percentage_media_long <- tidyr::gather(percentage_media, key = "variable", value = "percentage")

percentage_media_long <- percentage_media_long %>% 
  mutate_if(is.numeric, round)

# Create a bar plot using ggplot2
media_plot <- ggplot(percentage_media_long, aes(x = reorder(variable, percentage), y= percentage, fill = variable)) +
  geom_bar(stat = "identity", fill="red") +
  labs(title = "",
       x = "",
       y = "Percentage") +
  geom_text(aes(label=percentage), position=position_dodge(width=0.9), vjust=-0.25, size= 7)+
  theme_clean_light()+
  theme(legend.position = "none",
        axis.text.x= element_text(angle=45, hjust = 1, size=20),
        axis.title.y = element_text(size=20))

ggsave("_SharedFolder_article_twitter-elxn22/graphs/media_prop.png", height= 10, width= 15)

#### Combine the barplots####

# Combine your dataframes
combined_data <- rbind(percentage_media_long, percentage_tweets_long)
combined_data$type <- rep(c("Media", "Tweets"), each = nrow(percentage_media_long))

# Plot the combined data with grouped bars
ggplot(combined_data, aes(x = variable, y = percentage, fill = type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  labs(title = "",
       x = "",
       y = "Percentage") +
  geom_text(aes(label = percentage), position = position_dodge(width = 0.7), vjust = -0.25, size = 7) +
  scale_fill_manual(values = c("red", "blue")) +
  theme_clean_light() +
  theme(legend.position = "top",
        legend.text = element_text(size = 20),  # Adjust the size of legend text
        legend.title = element_text(size = 20),
        axis.text.x= element_text(angle=45, hjust = 1, size=20),
        axis.title.y = element_text(size=20)) +  # Adjust the size of legend title
  guides(fill = guide_legend(title = "Type")) 

ggsave("_SharedFolder_article_twitter-elxn22/graphs/barplot.png", height= 10, width= 15)
  

# Combine the plots using patchwork








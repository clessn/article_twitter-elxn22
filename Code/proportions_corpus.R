library(tidyverse)



# Loading datasets --------------------------------------------------------

data_media <- readRDS("/Users/jeremiedrouin/Dropbox/Travail/Universite_Laval/CLESSN/Publications/article_twitter-elxn22/_SharedFolder_article_twitter-elxn22/Data/dict_media.rds")
data_tweets <- readRDS("/Users/jeremiedrouin/Dropbox/Travail/Universite_Laval/CLESSN/Publications/article_twitter-elxn22/_SharedFolder_article_twitter-elxn22/Data/dict_tweets.rds")



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

percentage_tweets_long <- tidyr::gather(percentage_tweets, key = "variable", value = "percentage")

# Create a bar plot using ggplot2
ggplot(percentage_tweets_long, aes(x = variable, y = percentage, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Proportion de chaque enjeu, Twitter",
       x = "Variables",
       y = "Percentage") +
  theme_minimal()



  

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

percentage_media_long <- tidyr::gather(percentage_media, key = "variable", value = "percentage")

# Create a bar plot using ggplot2
ggplot(percentage_media_long, aes(x = variable, y = percentage, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Proportion de chaque enjeu, médias",
       x = "Variables",
       y = "Percentage") +
  theme_minimal()









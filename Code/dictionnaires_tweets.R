library(tidyverse)
library(clessnverse)

credentials <- hublot::get_credentials( Sys.getenv("HUB3_URL"),  Sys.getenv("HUB3_USERNAME"),  Sys.getenv("HUB3_PASSWORD"))

tweets_df <- readRDS("/Users/jeremiedrouin/Dropbox/Travail/Universite_Laval/CLESSN/Publications/article_twitter-elxn22/_SharedFolder_article_twitter-elxn22/Data/tweetstext.rds")

dict_df <- utils::read.csv2("/Users/jeremiedrouin/Dropbox/Travail/Universite_Laval/CLESSN/Publications/article_twitter-elxn22/_SharedFolder_article_twitter-elxn22/dictionnaires/dict_issues.csv", encoding = "UTF-8")

dict_en <- dict_df %>% 
  filter(language=="en")

dict_fr <- dict_df %>% 
  filter(language=="fr")
  


dict_listEn <- list()
for (c in unique(dict_en$category)) {
  dict_listEn[[c]] <- dict_en$item[dict_en$category == c]
}

dictEn <- quanteda::dictionary(as.list(dict_listEn))

dict_listFr <- list()
for (c in unique(dict_fr$category)) {
  dict_listFr[[c]] <- dict_fr$item[dict_fr$category == c]
}


qdictFr <- quanteda::dictionary(as.list(dict_listFr))

ExtractDict <- clessnverse::run_dictionary(data = tweets_df,
                                           text = tweets_df$data.text,
                                           dict = qdictFr)



###### Creating a new dataframe with the text and the dictionnary

full_df <- cbind(tweets_df, ExtractDict)

full_df$data.text[[59]]

saveRDS(full_df, "_SharedFolder_article_twitter-elxn22/Data/dict_tweets.rds")







dictFr <- clessnverse::get_dictionary(topic="issues", lang = "fr", credentials=credentials)








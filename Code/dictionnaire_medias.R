library(tidyverse)
library(clessnverse)

credentials <- hublot::get_credentials( Sys.getenv("HUB3_URL"),  Sys.getenv("HUB3_USERNAME"),  Sys.getenv("HUB3_PASSWORD"))

media_df <- readRDS("/Users/jeremiedrouin/Dropbox/Travail/Universite_Laval/CLESSN/Publications/article_twitter-elxn22/_SharedFolder_article_twitter-elxn22/Data/MediaClean_2023-11-21.rds")

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

MediaDict <- clessnverse::run_dictionary(data = media_df,
                                           text = media_df$text,
                                           dict = qdictFr)


######## combine the two dataframes ########

media_full <- cbind(media_df, MediaDict)

saveRDS(media_full, "_SharedFolder_article_twitter-elxn22/Data/dict_media.rds")









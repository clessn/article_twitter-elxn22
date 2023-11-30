library(tidyverse)
library(clessnverse)

credentials <- hublot::get_credentials( Sys.getenv("HUB3_URL"),  Sys.getenv("HUB3_USERNAME"),  Sys.getenv("HUB3_PASSWORD"))

media_df <- readRDS("_SharedFolder_article_twitter-elxn22/Data/MediaClean_2023-11-21.rds")

dict_pimp <- utils::read.csv2("_SharedFolder_article_twitter-elxn22/dictionnaires/dict_final.csv", encoding = "UTF-8")

dict_listFinal <- list()
for (c in unique(dict_pimp$category)) {
  dict_listFinal[[c]] <- dict_pimp$item[dict_pimp$category == c]
  
}

dictFinal <- quanteda::dictionary(as.list(dict_listFinal))



qdictFinal <- quanteda::dictionary(as.list(dict_listFinal))

MediaDict <- clessnverse::run_dictionary(data = media_df,
                                           text = media_df$text,
                                           dict = qdictFinal)


######## combine the two dataframes ########

media_full <- cbind(media_df, MediaDict)

saveRDS(media_full, "_SharedFolder_article_twitter-elxn22/Data/dict_media.rds")









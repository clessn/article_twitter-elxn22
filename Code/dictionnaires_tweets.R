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

#####
#rendre les données de dictionnaires en binaire
####

full_df$macroeconomics[full_df$macroeconomics >= 1] <- 1
full_df$macroeconomics[full_df$macroeconomics == 0] <- 0

full_df$civil_rights[full_df$civil_rights >= 1] <- 1
full_df$civil_rights[full_df$civil_rights == 0] <- 0

full_df$healthcare[full_df$healthcare >= 1] <- 1
full_df$healthcare[full_df$healthcare == 0] <- 0

full_df$agriculture[full_df$agriculture >= 1] <- 1
full_df$agriculture[full_df$agriculture == 0] <- 0

full_df$forestry[full_df$forestry >= 1] <- 1
full_df$forestry[full_df$forestry == 0] <- 0

full_df$labour[full_df$labour >= 1] <- 1
full_df$labour[full_df$labour == 0] <- 0

full_df$immigration[full_df$immigration >= 1] <- 1
full_df$immigration[full_df$immigration == 0] <- 0

full_df$education[full_df$education >= 1] <- 1
full_df$education[full_df$education == 0] <- 0

full_df$environment[full_df$environment >= 1] <- 1
full_df$environment[full_df$environment == 0] <- 0

full_df$energy[full_df$energy >= 1] <- 1
full_df$energy[full_df$energy == 0] <- 0

full_df$fisheries[full_df$fisheries >= 1] <- 1
full_df$fisheries[full_df$fisheries == 0] <- 0

full_df$transportation[full_df$transportation >= 1] <- 1
full_df$transportation[full_df$transportation == 0] <- 0

full_df$crime[full_df$crime >= 1] <- 1
full_df$crime[full_df$crime == 0] <- 0

full_df$social_welfare[full_df$social_welfare >= 1] <- 1
full_df$social_welfare[full_df$social_welfare == 0] <- 0

full_df$housing[full_df$housing >= 1] <- 1
full_df$housing[full_df$housing == 0] <- 0

full_df$finance[full_df$finance >= 1] <- 1
full_df$finance[full_df$finance == 0] <- 0

full_df$defence[full_df$defence >= 1] <- 1
full_df$defence[full_df$defence == 0] <- 0

full_df$sstc[full_df$sstc >= 1] <- 1
full_df$sstc[full_df$sstc == 0] <- 0

full_df$foreign_trade[full_df$foreign_trade >=1] <- 1
full_df$foreign_trade[full_df$foreign_trade ==0] <- 0

full_df$intl_affairs[full_df$intl_affairs >=1] <- 1
full_df$intl_affairs[full_df$intl_affairs ==0] <- 0

full_df$government_ops[full_df$government_ops >=1] <- 1
full_df$government_ops[full_df$government_ops ==0] <- 0

full_df$`land-water-management`[full_df$`land-water-management` >= 1] <- 1
full_df$`land-water-management`[full_df$`land-water-management` == 0] <- 0

full_df$culture[full_df$culture >= 1] <- 1
full_df$culture[full_df$culture == 0] <- 0

full_df$prov_local[full_df$prov_local >= 1] <- 1
full_df$prov_local[full_df$prov_local == 0] <- 0

full_df$intergovernmental[full_df$intergovernmental >= 1] <- 1
full_df$intergovernmental[full_df$intergovernmental == 0] <- 0

full_df$constitutional_natl_unity[full_df$constitutional_natl_unity >= 1] <- 1
full_df$constitutional_natl_unity[full_df$constitutional_natl_unity == 0] <- 0

full_df$aboriginal[full_df$aboriginal >= 1] <- 1
full_df$aboriginal[full_df$aboriginal == 0] <- 0

full_df$religion[full_df$religion >= 1] <- 1
full_df$religion[full_df$religion == 0] <- 0

saveRDS(full_df, "_SharedFolder_article_twitter-elxn22/Data/binary_dict_tweets.rds")






dictFr <- clessnverse::get_dictionary(topic="issues", lang = "fr", credentials=credentials)

########
#On essaie d'appliquer les dictionnaires aux textes médiatiques####
#######

tests_txt <- read_csv("_SharedFolder_article_twitter-elxn22/Data/MediaClean.csv")

dict_test <- clessnverse::run_dictionary(data = tests_txt,
                                           text = tests_txt$text,
                                           dict = qdictFr)








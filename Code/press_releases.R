library(tidyverse)
library(clessnverse)

# connect to hublot
credentials <<- hublot::get_credentials(
  Sys.getenv("HUB3_URL"), 
  Sys.getenv("HUB3_USERNAME"), 
  Sys.getenv("HUB3_PASSWORD"))


df <- clessnverse::get_warehouse_table(
  table_name = 'political_parties_press_releases',
  data_filter = list(
    data__province_or_state="QC"
  ),
  credentials = credentials,
  nbrows = 0
)

names(df)
table(df$political_party)
table(df$date)

df_press_release <- df %>% 
  filter(date>= "2022-08-28", date <= "2022-10-03")

########
####Appliquer les dictionnaires#####
########

dict_pimp <- utils::read.csv2("/Users/jeremiedrouin/Dropbox/Travail/Universite_Laval/CLESSN/Publications/article_twitter-elxn22/_SharedFolder_article_twitter-elxn22/dictionnaires/dict_final.csv", encoding = "UTF-8")

dict_listFinal <- list()
for (c in unique(dict_pimp$category)) {
  dict_listFinal[[c]] <- dict_pimp$item[dict_pimp$category == c]
  
}

dictFinal <- quanteda::dictionary(as.list(dict_listFinal))



qdictFinal <- quanteda::dictionary(as.list(dict_listFinal))

ReleaseDict <- clessnverse::run_dictionary(data = df_press_release,
                                           text = df_press_release$body,
                                           dict = qdictFinal)

releases_full <- cbind(df_press_release, ReleaseDict)


########Save le RDS#####

saveRDS(releases_full, "_SharedFolder_article_twitter-elxn22/Data/dict_releases.rds")
  
  
  
  
  
  
  
  
  


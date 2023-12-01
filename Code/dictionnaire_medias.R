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

MediaDict <- clessnverse::run_dictionary(data = media_df[1:10,],
                                           text = media_df$text[1:10],
                                           dict = qdictFinal)

########
####Test de loop####

# Define the size of each batch
batch_size <- 10

# Calculate the number of batches
num_batches <- ceiling(nrow(media_df) / batch_size)

# Initialize an empty list to store the results
result_list <- list()

# Loop through batches
for (i in 1:num_batches) {
  # Define the range for the current batch
  start_index <- (i - 1) * batch_size + 1
  end_index <- min(i * batch_size, nrow(media_df))
  
  # Extract the current batch
  current_batch <- media_df[start_index:end_index, ]
  
  # Apply run_dictionary to the current batch
  current_result <- clessnverse::run_dictionary(
    data = current_batch,
    text = current_batch$text,
    dict = qdictFinal
  )
  
  # Add the result to the list
  result_list[[i]] <- current_result
}

# Combine the results into a single dataframe
MediaDict <- do.call(rbind, result_list)


######## combine the two dataframes ########

media_full <- cbind(media_df, MediaDict)

saveRDS(media_full, "_SharedFolder_article_twitter-elxn22/Data/dict_media_final.rds")









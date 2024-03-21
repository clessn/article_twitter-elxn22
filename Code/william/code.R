library(dplyr)
library(tidyr)
library(lubridate)
library(fixest)
library(clubSandwich)
library(ggplot2)
library(stargazer) 
library(modelsummary)
library(magick)
library(broom)
library(webshot2)
library(writexl)
library(stringr)
library(xtable)

setwd("/Users/adriencloutier/Library/CloudStorage/Dropbox/Travail/Universite_Laval/publications/article_twitter-elxn22/_SharedFolder_article_twitter-elxn22/william")
#setwd("C:/Users/willi/Dropbox/_SharedFolder_article_twitter-elxn22/william")

#----------------------------------#
#-------- READ DATA ---------------#
#----------------------------------#

tweets <- readRDS("dict_tweets.rds")
media <- readRDS("dict_media_final.rds")
releases <- readRDS("dict_releases.rds")

#----------------------------------#
#-------- PRELIMINARIES -----------#
#----------------------------------#

# list of all topics

topic_vars <- c("macroeconomics", "healthcare", "agriculture", "labour", 
                "immigration", "environment", "energy", "transportation", 
                "crime", "culture", "aboriginal", "religion", "education", 
                "cost_life", "langue_fran")

# function to binarize

convert_numbers <- function(x) {
  
  ifelse(x > 1, 1, x)
  
}

# mapping between twitter handles and parties

party_mapping <- c(CAQ = "@coalitionavenir", CAQ = "francoislegault",
                   PLQ = "@LiberalQuebec", PLQ = "DomAnglade",
                   PQ = "@partiquebecois", PQ = "PaulPlamondon",
                   PCQ = "@PconservateurQc", PCQ = "E_Duhaime",
                   QS = "@QuebecSolidaire", QS = "GNadeauDubois")


#----------------------------------#
#------------ CLEAN ---------------#
#----------------------------------#

# clean tweets

tweets <- tweets %>% 
  mutate(date = as.Date(data.creationDate, format = "%Y-%m-%d"))

# clean media

media <- media %>% 
  select(-doc_id, -text)

media$year <- 2022
media <- media %>%
  mutate(date = as.Date(paste(year, month, day, sep = "-"), format = "%Y-%m-%d")) %>% 
  select(-year, -month, -day)

# clean releases

releases <- releases %>% 
  select(date, political_party, all_of(topic_vars))

# rename topic variables

tweets <- tweets %>% 
  rename_with(~ paste("tweet", ., sep = "_"), all_of(topic_vars))

media <- media %>% 
  rename_with(~ paste("media", ., sep = "_"), all_of(topic_vars))

releases <- releases %>% 
  rename_with(~ paste("release", ., sep = "_"), all_of(topic_vars))

# scaling of media variables
# I do this because there are article about almost everything every day
# I normalize to get the proportion of a topic per day

media <- media %>%
  group_by(date) %>%
  summarize(across(all_of(paste0("media_", topic_vars)), ~ sum(.x, na.rm = TRUE)))

media <- media %>%
  mutate(RowSum = rowSums(select(., -date))) %>%
  mutate(across(-c(date, RowSum), ~ .x / RowSum)) %>%
  select(-RowSum)

media <- media %>%
  mutate(across(starts_with("media_"), 
                list(prop10 = ~ as.integer(.x >= 0.10),
                     prop15 = ~ as.integer(.x >= 0.15),
                     prop25 = ~ as.integer(.x >= 0.25),
                     prop50 = ~ as.integer(.x >= 0.50)),
                .names = "prop_{.col}_{.fn}"))

media <- media %>% 
  mutate(across(starts_with("media_"), lag, .names = "lag_{.col}"))


# tweets per handle

tweets_by_handle <- tweets %>%
  mutate(handle = gsub("@", "", metadata.twitterHandle)) %>%
  group_by(date, handle) %>%
  summarize(tweet_count = n()) %>%
  ungroup() %>%
  pivot_wider(names_prefix = "tweet_count_", 
              names_from = handle, 
              values_from = tweet_count, 
              values_fill = list(tweet_count = 0))

# tweets per party

handle_to_party <- setNames(names(party_mapping), party_mapping)

tweets_by_party <- tweets %>%
  mutate(party = handle_to_party[metadata.twitterHandle]) %>%
  group_by(date, party) %>%
  summarize(tweet_count = n()) %>%
  ungroup() %>%
  pivot_wider(names_prefix = "tweet_count_", 
              names_from = party, 
              values_from = tweet_count, 
              values_fill = list(tweet_count = 0))

# tweets by any party, incumbent and others

count_any_party <- tweets %>%
  group_by(date) %>%
  summarize(across(starts_with("tweet_"), ~ sum(.x > 0, na.rm = TRUE)), .groups = "drop") %>%
  rename_with(~ paste("count_any", ., sep = "_"), starts_with("tweet_")) %>% 
  arrange(date) %>%  # Ensure the data is ordered by date
  mutate(across(starts_with("count_any"), lag, .names = "lag_{.col}"))


count_incumbent <- tweets %>%
  filter(metadata.twitterHandle %in% c("francoislegault", "@coalitionavenir")) %>%
  group_by(date) %>%
  summarize(across(starts_with("tweet_"), ~ sum(.x > 0, na.rm = TRUE)), .groups = "drop") %>%
  rename_with(~ paste("count_incumbent", ., sep = "_"), starts_with("tweet_"))

count_other_handles <- tweets %>%
  filter(!metadata.twitterHandle %in% c("francoislegault", "@coalitionavenir")) %>%
  group_by(date) %>%
  summarize(across(starts_with("tweet_"), ~ sum(.x > 0, na.rm = TRUE)), .groups = "drop") %>%
  rename_with(~ paste("count_other", ., sep = "_"), starts_with("tweet_"))

# indicators for mentions by handle

indicator_by_handle <- tweets %>%
  mutate(handle = gsub("@", "", metadata.twitterHandle)) %>%
  group_by(date, handle) %>%
  summarize(across(starts_with("tweet_"), ~ as.integer(sum(.x, na.rm = TRUE) > 0))) %>%
  ungroup() %>%
  pivot_wider(names_from = handle, 
              values_from = starts_with("tweet_"), 
              names_prefix = "ind_",
              names_sep = "_",
              values_fill = 0)

count_by_handle <- tweets %>%
  mutate(handle = gsub("@", "", metadata.twitterHandle)) %>%
  group_by(date, handle) %>%
  summarize(across(starts_with("tweet_"), ~ sum(.x > 0, na.rm = TRUE)), .groups = "drop") %>%
  pivot_wider(names_from = handle, 
              values_from = starts_with("tweet_"), 
              names_prefix = "count_",
              names_sep = "_",
              values_fill = 0) 

indicator_by_party <- tweets %>%
  mutate(party = handle_to_party[metadata.twitterHandle]) %>%
  group_by(date, party) %>%
  summarize(across(starts_with("tweet_"), ~ as.integer(sum(.x, na.rm = TRUE) > 0)), .groups = "drop") %>%
  pivot_wider(names_from = party, 
              values_from = starts_with("tweet_"), 
              names_prefix = "ind_",
              names_sep = "_",
              values_fill = 0)

count_by_party <- tweets %>%
  mutate(party = handle_to_party[metadata.twitterHandle]) %>%
  group_by(date, party) %>%
  summarize(across(starts_with("tweet_"), ~ sum(.x > 0, na.rm = TRUE)), .groups = "drop") %>%
  pivot_wider(names_from = party, 
              values_from = starts_with("tweet_"), 
              names_prefix = "count_",
              names_sep = "_",
              values_fill = 0)

# Likes and retweets

retweets_by_date <- tweets %>%
  group_by(date) %>%
  summarize(retweets_by_date = sum(as.numeric(data.retweetCount)))

likes_by_date <- tweets %>%
  group_by(date) %>%
  summarize(likes_by_date = sum(as.numeric(data.likeCount)))


# create indicators for release

indicator_by_party_releases <- releases %>%
  group_by(date, political_party) %>%
  summarize(across(starts_with("release_"), ~ as.integer(sum(.x, na.rm = TRUE) > 0)), .groups = "drop") %>%
  pivot_wider(names_from = political_party, 
              values_from = starts_with("release_"), 
              names_prefix = "ind_",
              names_sep = "_",
              values_fill = 0)

indicator_any_party_release <- releases %>%
  group_by(date) %>%
  summarize(across(starts_with("release_"), ~ as.integer(sum(.x, na.rm = TRUE) > 0)), .groups = "drop") %>%
  rename_with(~ paste("ind_any_party", ., sep = "_"), starts_with("release_"))

#----------------------------------#
#------------ MERGE ---------------#
#----------------------------------#

tweets_by_handle$date <- as.Date(tweets_by_handle$date)
tweets_by_party$date <- as.Date(tweets_by_party$date)
indicator_by_handle$date <- as.Date(indicator_by_handle$date)
count_by_handle$date <- as.Date(count_by_handle$date)
indicator_by_party$date <- as.Date(indicator_by_party$date)
count_by_party$date <- as.Date(count_by_party$date)
indicator_by_party_releases$date <- as.Date(indicator_by_party_releases$date)
indicator_any_party_release$date <- as.Date(indicator_any_party_release$date)
count_any_party$date <- as.Date(count_any_party$date)
count_incumbent$date <- as.Date(count_incumbent$date)
count_other_handles$date <- as.Date(count_other_handles$date)
retweets_by_date$date <- as.Date(retweets_by_date$date)
likes_by_date$date <- as.Date(likes_by_date$date)


dta_final <- media %>%
  left_join(tweets_by_handle, by = "date") %>%
  left_join(tweets_by_party, by = "date") %>%
  left_join(indicator_by_handle, by = "date") %>%
  left_join(count_by_handle, by = "date") %>%
  left_join(indicator_by_party, by = "date") %>%
  left_join(count_by_party, by = "date") %>%
  left_join(indicator_by_party_releases, by = "date") %>%
  left_join(indicator_any_party_release, by = "date") %>%
  left_join(count_any_party, by = "date") %>%
  left_join(count_incumbent, by = "date") %>%
  left_join(count_other_handles, by = "date") %>% 
  left_join(retweets_by_date, by = "date") %>%
  left_join(likes_by_date, by = "date") %>%
  
  mutate(across(everything(), ~replace_na(., 0)))

dta_final <- dta_final %>%
  arrange(date) %>%
  mutate(time_trend = 1:n()) 

#----------------------------------#
#-------- REGRESSIONS -------------#
#----------------------------------#

# first model -- without controlling for releases

for (topic in topic_vars) {

    formula <- as.formula(paste("media_", topic, " ~ count_any_tweet_", topic, "+ time_trend", sep = ""))
    lm_model <- feols(formula, data = dta_final, vcov = "hetero")
    print(summary(lm_model))

}



# second model -- controlling for releases

for (topic in topic_vars) {
  
  formula <- as.formula(paste("media_", topic, " ~ count_any_tweet_", topic, "+ ind_any_party_release_", topic, "+ time_trend", sep = ""))
  lm_model <- feols(formula, data = dta_final, vcov = "hetero")
  print(summary(lm_model))
  
}

# third model -- adding lag tweets

for (topic in topic_vars) {
  
  formula <- as.formula(paste("media_", topic, " ~ count_any_tweet_", topic, "+ lag_count_any_tweet_", topic, "+ ind_any_party_release_", topic, "+ time_trend", sep = ""))
  lm_model <- feols(formula, data = dta_final, vcov = "hetero")
  print(summary(lm_model))
  
}


# fourth model -- controlling for releases of CAQ

for (topic in topic_vars) {
  
  formula <- as.formula(paste("media_", topic, " ~ count_any_tweet_", topic, "+ release_", topic, "_ind_CAQ",  "+ time_trend", sep = ""))
  lm_model <- feols(formula, data = dta_final, vcov = "hetero")
  print(summary(lm_model))
  
}

# fifth model -- effects of incumbent


for (topic in topic_vars) {
  
  formula <- as.formula(paste("media_", topic, " ~ count_incumbent_tweet_", topic, "+ ind_any_party_release_", topic, "+ time_trend", sep = ""))
  lm_model <- feols(formula, data = dta_final , vcov = "hetero")
  print(summary(lm_model))
  
}

# sixth model -- effects of incumbent and others


for (topic in topic_vars) {
  
  formula <- as.formula(paste("media_", topic, " ~ count_incumbent_tweet_", topic, "+ count_other_tweet_", topic, "+ ind_any_party_release_", topic, "+ time_trend", sep = ""))
  lm_model <- feols(formula, data = dta_final, vcov = "hetero")
  print(summary(lm_model))
  
}


# seventh model -- controlling for lag media

for (topic in topic_vars) {
  
  formula <- as.formula(paste("media_", topic, " ~ count_any_tweet_", topic, "+ lag_media_", topic, "+ lag_count_any_tweet_", topic, "+ ind_any_party_release_", topic, "+ time_trend", sep = ""))
  lm_model <- feols(formula, data = dta_final, vcov = "hetero")
  print(summary(lm_model))
  
}

# independence condition #1

for (topic in topic_vars) {
  
  formula <- as.formula(paste(" lag_media_", topic, "~ ind_any_party_release_", topic, "+ time_trend", sep = ""))
  lm_model <- feols(formula, data = dta_final, vcov = "hetero")
  print(summary(lm_model))
  
}

# independence condition #2

for (topic in topic_vars) {
  
  formula <- as.formula(paste(" lag_count_any_tweet_", topic, "~ ind_any_party_release_", topic, "+ time_trend", sep = ""))
  lm_model <- feols(formula, data = dta_final, vcov = "hetero")
  print(summary(lm_model))
  
}


#----------------------------------#
#--------- MODEL SUMMARY ----------#
#----------------------------------#

topic_vars <- c("macroeconomics", "healthcare", "agriculture", "labour", 
                "immigration", "environment", "energy", "transportation", 
                "crime", "culture", "aboriginal", "religion", "education", 
                "cost_life", "langue_fran")

# Préparation des listes pour stocker les modèles de chaque type
models_without_releases <- list()
models_with_releases <- list()
models_with_lag <- list()
models_CAQ_releases <- list()
models_incumbent_effects <- list()
models_incumbent_and_others_effects <- list()
models_incumbent_and_others_effects <- list()
models_likes <- list()
models_retweets <- list()
models_likes_retweets <- list()
models_lag_media <- list()



# Boucles pour chaque type de modèle
for (topic in topic_vars) {
  
    # Modèle sans contrôle des publications
    formula <- as.formula(paste("media_", topic, " ~ count_any_tweet_", topic, "+ time_trend", sep = ""))
    models_without_releases[[topic]] <- feols(formula, data = dta_final, vcov = "hetero")

    # Modèle avec contrôle des publications
    formula <- as.formula(paste("media_", topic, " ~ count_any_tweet_", topic, "+ ind_any_party_release_", topic, "+ time_trend", sep = ""))
    models_with_releases[[topic]] <- feols(formula, data = dta_final, vcov = "hetero")

    # Modèle avec lag tweets
    formula <- as.formula(paste("media_", topic, " ~ count_any_tweet_", topic, "+ lag_count_any_tweet_", topic, "+ ind_any_party_release_", topic, "+ time_trend", sep = ""))
    models_with_lag[[topic]] <- feols(formula, data = dta_final, vcov = "hetero")

    # Modèle avec contrôle des publications de CAQ
    formula <- as.formula(paste("media_", topic, " ~ count_any_tweet_", topic, "+ release_", topic, "_ind_CAQ",  "+ time_trend", sep = ""))
    models_CAQ_releases[[topic]] <- feols(formula, data = dta_final, vcov = "hetero")

    # Modèle avec effets de l'incumbent
    formula <- as.formula(paste("media_", topic, " ~ count_incumbent_tweet_", topic, "+ ind_any_party_release_", topic, "+ time_trend", sep = ""))
    models_incumbent_effects[[topic]] <- feols(formula, data = dta_final, vcov = "hetero")

    # Modèle avec effects of incumbent and others
    formula <- as.formula(paste("media_", topic, " ~ count_incumbent_tweet_", topic, "+ count_other_tweet_", topic, "+ ind_any_party_release_", topic, "+ time_trend", sep = ""))
    models_incumbent_and_others_effects[[topic]] <- feols(formula, data = dta_final, vcov = "hetero")
    
    # Modèle pour likes
    formula <- as.formula(paste("media_", topic, " ~ count_any_tweet_", topic, "+ likes_by_date", "+ time_trend", sep = ""))
    models_likes[[topic]] <- feols(formula, data = dta_final, vcov = "hetero")
    
    # Modèle pour retweets
    formula <- as.formula(paste("media_", topic, " ~ count_any_tweet_", topic, "+ retweets_by_date", "+ time_trend", sep = ""))
    models_retweets[[topic]] <- feols(formula, data = dta_final, vcov = "hetero")
    
    # Modèle pour like et retweets
    formula <- as.formula(paste("media_", topic, " ~ count_any_tweet_", topic, "+ likes_by_date", "+ retweets_by_date", "+ time_trend", sep = ""))
    models_likes_retweets[[topic]] <- feols(formula, data = dta_final, vcov = "hetero")
    
    # controlling for lag media
    formula <- as.formula(paste("media_", topic, " ~ count_any_tweet_", topic, "+ lag_media_", topic, "+ lag_count_any_tweet_", topic, "+ ind_any_party_release_", topic, "+ time_trend", sep = ""))
    models_lag_media[[topic]] <- feols(formula, data = dta_final, vcov = "hetero")
}

#modelsummary(models_without_releases, output = "tableau1.png", fmt = 2)
#modelsummary(models_with_releases, output = "tableauSTP.png", fmt = 2, stars = TRUE)
#modelsummary(models_with_lag, output = "tableau3.png", fmt = 2, stars = TRUE)
#modelsummary(models_CAQ_releases, output = "tableau4.png", fmt = 2, stars = TRUE)
#modelsummary(models_incumbent_effects, output = "tableau5.png", fmt = 2, stars = TRUE)
#modelsummary(models_incumbent_and_others_effects, output = "tableau6.png", fmt = 2, stars = TRUE)
#modelsummary(models_likes_retweets, output = "tableau7.png", fmt = 2, stars = TRUE)


coef_map <- c(                  "count_any_tweet_macroeconomics" = "Macroeconomics in tweets",
              "ind_any_party_release_macroeconomics" = "Macroeconomics in party releases",
              "media_macroeconomics" = "Macroeconomics in the media",
              "count_incumbent_tweet_macroeconomics" = "Macroeconomics in incumbent tweets",
              "count_other_tweet_macroeconomics" = "Macroeconomics in other tweets",
                               "count_any_tweet_healthcare" = "Healthcare in tweets",
              "ind_any_party_release_healthcare" = "Healthcare in party releases",
              "media_healthcare" = "Healthcare in the media",
              "count_incumbent_tweet_healthcare" = "Healthcare in incumbent tweets",
              "count_other_tweet_healthcare" = "Healthcare in other tweets",
                               "count_any_tweet_agriculture" = "Agriculture in tweets",
              "ind_any_party_release_agriculture" = "Agriculture in party releases",
              "media_agriculture" = "Agriculture in the media",
              "count_incumbent_tweet_agriculture" = "Agriculture in incumbent tweets",
              "count_other_tweet_agriculture" = "Agriculture in other tweets",
                               "count_any_tweet_labour" = "Labour in tweets",
              "ind_any_party_release_labour" = "Labour in party releases",
              "media_labour" = " in the mediaLabour",
              "count_incumbent_tweet_labour" = "Labour in incumbent tweets",
              "count_other_tweet_labour" = "Labour in other tweets",
                               "count_any_tweet_immigration" = "Immigration in tweets",
              "ind_any_party_release_immigration" = "Immigration in party releases",
              "media_immigration" = "Immigration in the media",
              "count_incumbent_tweet_immigration" = "Immigration in incumbent tweets",
              "count_other_tweet_immigration" = "Immigration in other tweets",
                               "count_any_tweet_environment" = "Environment in tweets",
              "ind_any_party_release_environment" = "Environment in party releases",
              "media_environment" = "Environment in the media",
              "count_incumbent_tweet_environment" = "Environment in incumbent tweets",
              "count_other_tweet_environment" = "Environment in other tweets",
                               "count_any_tweet_energy" = "Energy in tweets",
              "ind_any_party_release_energy" = "Energy in party releases",
              "media_energy" = "Energy in the media",
              "count_incumbent_tweet_energy" = "Energy in incumbent tweets",
              "count_other_tweet_energy" = "Energy in other tweets",
                               "count_any_tweet_transportation" = "Transportation in tweets",
              "ind_any_party_release_transportation" = "Transportation in party releases",
              "media_transportation" = "Transportation in the media",
              "count_incumbent_tweet_transportation" = "Transportation in incumbent tweets",
              "count_other_tweet_transportation" = "Transportation in other tweets",
                               "count_any_tweet_crime" = "Crime in tweets",
              "ind_any_party_release_crime" = "Crime in party releases",
              "media_crime" = "Crime in the media",
              "count_incumbent_tweet_crime" = "Crime in incumbent tweets",
              "count_other_tweet_crime" = "Crime in other tweets",
                               "count_any_tweet_culture" = "Culture in tweets",
              "ind_any_party_release_culture" = "Culture in party releases",
              "media_culture" = "Culture in the media",
              "count_incumbent_tweet_culture" = "Culture in incumbent tweets",
              "count_other_tweet_culture" = "Culture in other tweets",
                               "count_any_tweet_aboriginal" = "Aboriginal Affairs in tweets",
              "ind_any_party_release_aboriginal" = "Aboriginal Affairs in party releases",
              "media_aboriginal" = "Aboriginal Affairs in the media",
              "count_incumbent_tweet_aboriginal" = "Aboriginal Affairs in incumbent tweets",
              "count_other_tweet_aboriginal" = "Aboriginal Affairs in other tweets",
                               "count_any_tweet_religion" = "Religion in tweets",
              "ind_any_party_release_religion" = "Religion in party releases",
              "media_religion" = "Religion in the media",
              "count_incumbent_tweet_religion" = "Religion in incumbent tweets",
              "count_other_tweet_religion" = "Religion in other tweets",
                               "count_any_tweet_education" = "Education in tweets",
              "ind_any_party_release_education" = "Education in party releases",
              "media_education" = "Education in the media",
              "count_incumbent_tweet_education" = "Education in incumbent tweets",
              "count_other_tweet_education" = "Education in other tweets",
                               "count_any_tweet_cost_life" = "Cost of Living in tweets",
              "ind_any_party_release_cost_life" = "Cost of Living in party releases",
              "media_cost_life" = "Cost of Living in the media",
              "count_incumbent_tweet_cost_life" = "Cost of Living in incumbent tweets",
              "count_other_tweet_cost_life" = "Cost of Living in other tweets",
                               "count_any_tweet_langue_fran" = "French Language in tweets",
              "ind_any_party_release_langue_fran" = "French Language in party releases",
              "$langue_fran" = "French Language in the media",
              "count_incumbent_tweet_langue_fran" = "French Language in incumbent tweets",
              "count_other_tweet_langue_fran" = "French Language in other tweets",
              "time_trend" = "Time trend")


modelsummary(models_with_releases, 
             estimate  = "{estimate}{stars} ({std.error})",
             statistic = NULL,
             coef_omit = "Intercept",
             stars = T,
             coef_map = coef_map,
             gof_map = c("nobs", "r.squared"),
             notes = c("Method: Fixed-effects OLS estimation.",
                       "Dependent variables: Number of texts in the media for each issues.", 
                       "Independent variables: Number of tweets for each issues.", 
                       "Control variables: Number of press releases for each issues.",
                       "*p&lt;0.05; **p&lt;0.01; ***p&lt;0.001"),
             output = "models_with_releases.html")



modelsummary(models_incumbent_and_others_effects, 
             estimate  = "{estimate}{stars} ({std.error})",
             statistic = NULL,
             coef_omit = "Intercept",
             stars = T,
             coef_map = coef_map,
             gof_map = c("nobs", "r.squared"),
             notes = c("Method: Fixed-effects OLS estimation.",
                       "Dependent variables: Number of texts in the media for each issues .", 
                       "Independent variables: Number of tweets for each issues. Incumbents ", 
                       "Control variables: Number of press releases for each issues.",
                       "*p&lt;0.05; **p&lt;0.01; ***p&lt;0.001"),
             output = "models_incumbent_and_others_effects.html")

modelsummary(models_lag_media, 
             estimate  = "{estimate}{stars} ({std.error})",
             statistic = NULL,
             coef_omit = "Intercept",
             stars = T,
          #   coef_map = coef_map,
             gof_map = c("nobs", "r.squared"),
             notes = c("Method: Fixed-effects OLS estimation.",
                       "Dependent variables: Number of texts in the media for each issues .", 
                       "Independent variables: Number of tweets for each issues. ", 
                       "Control variables: Number of press releases for each issues.",
                       "*p&lt;0.05; **p&lt;0.01; ***p&lt;0.001"),
             output = "models_lag_media.html")


# Tableaux William

toto <- etable(models_with_releases, se.below = TRUE)

colnames(toto)[1] <- "coef"

for(topic in topic_vars) {
  toto$coef <- str_replace_all(toto$coef, paste0("_?", topic, "_?"), "")
  toto$coef <- str_replace_all(toto$coef, "count_any_tweet", "CountTweet")
  toto$coef <- str_replace_all(toto$coef, "ind_any_party_release", "Release")
  toto$coef <- str_replace_all(toto$coef, "time_trend", "TimeTrend")
  
}

toto <- toto %>%
  mutate(previous_coef = lag(coef)) %>%
  rowwise() %>%
  mutate(coef = if_else(str_detect(coef, "^\\s*$"), paste0("(se)_var_", previous_coef), coef)) %>%
  select(-previous_coef) %>%
  ungroup()

toto <- toto %>% 
  filter(coef != "Dependent Var.:")


aggregation_criteria <- c("Constant", "CountTweet", "Release", "TimeTrend") # Coefs to aggregate
to_aggregate <- toto %>%
  filter(str_detect(coef, fixed("(se)")) | coef %in% aggregation_criteria)

to_keep <- toto %>% 
  filter(!str_detect(coef, fixed("(se)")) & !coef %in% aggregation_criteria)

# Aggregating by pasting together values within each group

aggregated <- to_aggregate %>%
  group_by(coef) %>%
  summarise(across(everything(), ~paste(., collapse=' '), .names = "{.col}"),
            .groups = 'drop') # Drop grouping structure after summarisation

combined <- bind_rows(aggregated, to_keep)

ordered_criteria <- unlist(lapply(aggregation_criteria, function(x) c(x, paste0("(se)_var_", x))))
combined <- combined %>%
  filter(coef %in% ordered_criteria) %>%
  mutate(order_key = match(coef, ordered_criteria)) %>%
  arrange(order_key) %>%
  select(-order_key)


combined <- bind_rows(combined, to_keep) %>% 
  mutate(coef = str_replace(coef, "(\\(se\\)).*", "\\1")) %>% 
  mutate(across(where(is.character), ~str_replace_all(., " ", "")))


tab <- xtable(combined)
sink("table1.txt")  
print(tab, include.rownames = FALSE)
sink()


prepare_and_save_table <- function(model, file_path) {
  # Initial table generation from etable, assuming etable returns a data frame-like object
  toto <- etable(model, se.below = TRUE)
  colnames(toto)[1] <- "coef"
  
  # Clean and replace specific parts of the 'coef' column
  for(topic in topic_vars) {
    patterns_to_replace <- list("_?" = "", "count_any_tweet" = "CountTweet", 
                                "ind_any_party_release" = "Release", "time_trend" = "TimeTrend", 
                                "count_incumbent_tweet" = "CountTweetInc",
                                "count_other_tweet" = "CountTweetOther", 
                                "lag_media" = "Media (lag)", "lag_CountTweet" = "CountTweet (lag)")
    for (pattern in names(patterns_to_replace)) {
      replacement <- patterns_to_replace[[pattern]]
      regex_pattern <- ifelse(pattern == "_?", paste0("_?", topic, "_?"), pattern)
      toto$coef <- str_replace_all(toto$coef, regex_pattern, replacement)
    }
  }
  
  # Modify coefficients with '(se)' when necessary and remove 'Dependent Var.:'
  toto <- toto %>%
    mutate(previous_coef = lag(coef)) %>%
    rowwise() %>%
    mutate(coef = if_else(str_detect(coef, "^\\s*$"), paste0("(se)_var_", previous_coef), coef)) %>%
    select(-previous_coef) %>%
    ungroup() %>%
    filter(coef != "Dependent Var.:")
  
  # Define aggregation criteria
  aggregation_criteria <- c("Constant", "CountTweet", "CountTweet (lag)", "Media (lag)", "CountTweetInc", "CountTweetOther", "Release", "TimeTrend")
  
  # Separate rows to aggregate from those to keep as is
  to_aggregate <- filter(toto, str_detect(coef, fixed("(se)")) | coef %in% aggregation_criteria)
  to_keep <- filter(toto, !str_detect(coef, fixed("(se)")) & !coef %in% aggregation_criteria)
  
  # Aggregate and combine
  aggregated <- to_aggregate %>%
    group_by(coef) %>%
    summarise(across(everything(), ~paste(., collapse=' '), .names = "{.col}"), .groups = 'drop')
  
  combined <- bind_rows(aggregated, to_keep)
  
  # Order and clean up
  ordered_criteria <- unlist(lapply(aggregation_criteria, function(x) c(x, paste0("(se)_var_", x))))
  combined <- combined %>%
    filter(coef %in% ordered_criteria) %>%
    mutate(order_key = match(coef, ordered_criteria)) %>%
    arrange(order_key) %>%
    select(-order_key)
  
  # Final cleanup
  combined <- bind_rows(combined, to_keep) %>%
    mutate(coef = str_replace(coef, "(\\(se\\)).*", "\\1")) %>%
    mutate(across(where(is.character), ~str_replace_all(., " ", "")))
  
  # Save output
  tab <- xtable(combined)
  sink(file_path)
  print(tab, include.rownames = FALSE)
  sink()
}

prepare_and_save_table(models_with_releases, "models_with_releases.txt")
prepare_and_save_table(models_incumbent_and_others_effects, "models_incumbent_and_others_effects.txt")
prepare_and_save_table(models_lag_media, "models_lag_media.txt")


## Graph 1 models_with_releases

# Extraction et préparation des coefficients
coefficients_df <- bind_rows(
  lapply(models_with_releases, tidy),
  .id = "model"
) %>%
  mutate(
    conf.low = estimate - 1.96 * std.error,
    conf.high = estimate + 1.96 * std.error,
    sig = case_when(p.value < 0.05 ~ "Significant (p-value <0.05)",
                    TRUE ~ "Non-significant")# Création d'une colonne pour la significativité
  ) %>%
  filter(!grepl("^ind_", term), !grepl("^lag_", term), term != "(Intercept)", term != "time_trend")

# Renommage des termes (ajustez selon vos besoins)
coefficients_df$term <- recode(coefficients_df$term, 
                               "count_any_tweet_macroeconomics" = "Macroeconomics",
                               "count_any_tweet_healthcare" = "Healthcare",
                               "count_any_tweet_agriculture" = "Agriculture",
                               "count_any_tweet_labour" = "Labour",
                               "count_any_tweet_immigration" = "Immigration",
                               "count_any_tweet_environment" = "Environment",
                               "count_any_tweet_energy" = "Energy",
                               "count_any_tweet_transportation" = "Transportation",
                               "count_any_tweet_crime" = "Crime",
                               "count_any_tweet_culture" = "Culture",
                               "count_any_tweet_aboriginal" = "Aboriginal Affairs",
                               "count_any_tweet_religion" = "Religion",
                               "count_any_tweet_education" = "Education",
                               "count_any_tweet_cost_life" = "Cost of Living",
                               "count_any_tweet_langue_fran" = "French Language"
                               # Add more renamings if necessary
)

# Ajustement de l'ordre des variables selon topic_vars
topic_vars <- rev(c("Macroeconomics", "Healthcare", "Agriculture", "Labour", 
                "Immigration", "Environment", "Energy", "Transportation", 
                "Crime", "Culture", "Aboriginal Affairs", "Religion", "Education", 
                "Cost of Living", "French Language"))

# Assurez-vous que coefficients_df$term est un facteur et réordonnez ses niveaux selon topic_vars
coefficients_df$term <- factor(coefficients_df$term, levels = topic_vars)

# Création du graphique avec l'ordre ajusté
ggplot(coefficients_df, aes(x = estimate, y = term)) +
  geom_point(aes(color = sig), size = 4) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high, color = sig), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  scale_color_manual(values = c("Significant (p-value <0.05)" = "darkblue", "Non-significant" = "grey")) +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_text(size = rel(1.3)), # Augmente la taille de la police du titre de la légende
        legend.text = element_text(size = rel(1.3)), # Augmente la taille de la police du texte de la légende
        axis.title = element_text(size = rel(1.3)), # Augmente la taille de la police des titres des axes
        axis.text = element_text(size = rel(1.3))) + # Augmente la taille de la police des textes des axes
  labs(x = "\nCoefficient Estimate", y = "", title = "",
       color = "")

ggsave("_SharedFolder_article_twitter-elxn22/william/models_with_releases.png", width = 30, height = 17, units = c("cm"), bg = "white")



## Graph 2 models_incumbent_and_others_effects

# Extraction et préparation des coefficients
coefficients_df <- bind_rows(
  lapply(models_incumbent_and_others_effects, tidy),
  .id = "model"
) %>%
  mutate(
    conf.low = estimate - 1.96 * std.error,
    conf.high = estimate + 1.96 * std.error,
    sig = case_when(p.value < 0.05 ~ "Significant (p-value <0.05)",
                    TRUE ~ "Non-significant")# Création d'une colonne pour la significativité
  ) %>%
  mutate(party = ifelse(grepl("incumbent", term), "Incumbent", "Other")) %>%
  filter(grepl("count_", term))

# Création d'une nouvelle variable pour les enjeus et la combinaison parti/significativité
coefficients_df <- coefficients_df %>%
  mutate(
    issue = gsub("count_(incumbent|other)_tweet_", "", term),
    issue = recode(issue, 
                   "macroeconomics" = "Macroeconomics",
                   "healthcare" = "Healthcare",
                   "agriculture" = "Agriculture",
                   "labour" = "Labour",
                   "immigration" = "Immigration",
                   "environment" = "Environment",
                   "energy" = "Energy",
                   "transportation" = "Transportation",
                   "crime" = "Crime",
                   "culture" = "Culture",
                   "aboriginal" = "Aboriginal Affairs",
                   "religion" = "Religion",
                   "education" = "Education",
                   "cost_life" = "Cost of Living",
                   "langue_fran" = "French Language"
    ),
    party_color = case_when(
      grepl("incumbent", term) & sig == "Significant (p-value <0.05)" ~ "Incumbent Significant (p-value <0.05)",
      grepl("incumbent", term) & sig != "Significant (p-value <0.05)" ~ "Incumbent Non-Significant",
      !grepl("incumbent", term) & sig == "Significant (p-value <0.05)" ~ "Other Significant (p-value <0.05)",
      TRUE ~ "Other Non-Significant"
    )
  )

# Changer l'ordre
coefficients_df$issue <- factor(coefficients_df$issue, levels = topic_vars)

# Étape 3: Création du graphique avec l'ordre ajusté et inversé pour issue
ggplot(coefficients_df, aes(x = estimate, y = issue, color = party_color, shape = party_color)) +
  geom_point(position = position_dodge(width = 0.5), size = 4) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2, position = position_dodge(width = 0.5)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  scale_color_manual(values = c(
    "Incumbent Significant (p-value <0.05)" = "darkblue",
    "Incumbent Non-Significant" = "lightblue",
    "Other Significant (p-value <0.05)" = "darkred",
    "Other Non-Significant" = "lightpink"
  )) +
  scale_shape_manual(values = c(
    "Incumbent Significant (p-value <0.05)" = 16,  # Circle
    "Incumbent Non-Significant" = 16,  # Circle
    "Other Significant (p-value <0.05)" = 17,  # Triangle
    "Other Non-Significant" = 17  # Triangle
  )) +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_text(size = rel(1.3)),
        legend.text = element_text(size = rel(1.3)),
        axis.title = element_text(size = rel(1.3)),
        axis.text = element_text(size = rel(1.3))) +
  guides(color = guide_legend(nrow = 2, title.hjust = 0.5), shape = guide_legend(nrow = 2, title.hjust = 0.5)) +
  labs(x = "\nCoefficient Estimate", y = "", title = "",
       color = "", shape = "")


ggsave("_SharedFolder_article_twitter-elxn22/william/models_incumbent_and_others_effects.png", width = 30, height = 17, units = c("cm"), bg = "white")


#----------------------------------#
#---------- FIGURES ---------------#
#----------------------------------#

dta_fig <- dta_final %>% 
  select(date, all_of(paste0("media_", topic_vars)), all_of(paste0("count_any_tweet_", topic_vars))) %>% 
  pivot_longer(
    cols = starts_with("media_") | starts_with("count_any_tweet_"),
    names_to = c(".value", "topic"),
    names_pattern = "(media_|count_any_tweet_)(.*)"
  )

ggplot() +
  geom_bar(data = dta_fig, aes(x = date, y = count_any_tweet_, fill = "tweet"), stat = "identity", position = "dodge") +
  geom_line(data = dta_fig, aes(x = date, y = media_*100, color = "media"), linewidth=0.85) +
  facet_wrap(~ topic) +
  labs(x = "\n Date", y = "Num. of Tweets and Media Coverage (%)", fill = NULL, color = NULL) +
  theme_minimal() +
  theme(legend.position = "bottom")+
  scale_fill_manual(values = c("tweet" = "#ff7f0e")) + 
  scale_color_manual(values = c("media" = "#1f77b4"))   

###

dta_fig2 <- dta_final %>% 
  select(date, all_of(paste0("media_", topic_vars)), all_of(paste0("ind_any_party_release_", topic_vars))) %>% 
  pivot_longer(
    cols = starts_with("media_") | starts_with("ind_any_party_release_"),
    names_to = c(".value", "topic"),
    names_pattern = "(media_|ind_any_party_release_)(.*)"
  )

ggplot() +
  geom_line(data = dta_fig, aes(x = date, y = media_*100, color = "media"), linewidth=0.85) +
  geom_vline(data = dta_fig2 %>% filter(ind_any_party_release_ == 1), aes(xintercept = as.numeric(date)), 
             linetype = "dashed", color = "grey") +
  facet_wrap(~ topic) +
  labs(x = "\n Date", y = "Releases and Media Coverage (%)", fill = NULL, color = NULL) +
  theme_minimal() +
  theme(legend.position = "bottom")+
  scale_color_manual(values = c("media" = "#1f77b4"))   


### 

dta_fig3 <- dta_final %>% 
  select(date, all_of(paste0("media_", topic_vars))) %>% 
  pivot_longer(
    cols = starts_with("media_"),
    names_to = c(".value", "topic"),
    names_pattern = "(media_)(.*)"
  )

df_ca <- releases %>%
  filter(political_party == "CAQ") %>%
  pivot_longer(
    cols = starts_with("release_"),
    names_to = "topic",
    names_prefix = "release_",
    values_to = "value"
  )

df_ca$date = as.Date(df_ca$date)

dta_fig3 <- dta_fig3 %>% 
  left_join(df_ca)

ggplot() +
  geom_line(data = dta_fig3, aes(x = date, y = media_*100, color = "media"), linewidth=0.85) +
  geom_vline(data = dta_fig3 %>% filter(value > 0), aes(xintercept = as.numeric(date)), 
             linetype = "dashed", color = "black") +
  facet_wrap(~ topic) +
  labs(x = "\n Date", y = "Incumbent Releases and Media Coverage (%)", fill = NULL, color = NULL) +
  theme_minimal() +
  theme(legend.position = "bottom")+
  scale_color_manual(values = c("media" = "#1f77b4"))   


## Stargazer 



### STOP HERE


# main_results <- data.frame(topic = character(), 
#                          coefficient_name = character(),
#                          coefficient = numeric(), 
#                          std_error = numeric(), 
#                          p_value = numeric(),
#                          stringsAsFactors = FALSE)
# 
# # Loop over each topic
# for (topic in topic_vars) {
#   # Create the regression formula
#   formula <- as.formula(paste("media_", topic, " ~ tweet_", topic, "+ time_trend", sep = ""))
#   
#   # Run the regression
#   lm_model <- feols(formula, data = merged_data, vcov = "hetero")
#   
#   # Extract and store the results
#   
#   model_summary <- as.data.frame(lm_model$coeftable)
#   coefs <- round(model_summary$Estimate, 4)
#   se <- round(model_summary$`Std. Error`, 4)
#   pvals <- round(model_summary$`Pr(>|t|)`, 4)
#   coef_names <- rownames(model_summary)
#   
#   # Append to the results dataframe
#   temp_df <- data.frame(
#     topic = rep(topic, length(coefs)), 
#     coefficient_name = coef_names, 
#     coefficient = coefs, 
#     std_error = se, 
#     p_value = pvals
#   )
#   
#   main_results <- rbind(main_results, temp_df)
# }
# 
# 
# ## MAIN RESULTS INCUMBENT
# 
# main_results_incumbent <- data.frame(topic = character(), 
#                            coefficient_name = character(),
#                            coefficient = numeric(), 
#                            std_error = numeric(), 
#                            p_value = numeric(),
#                            stringsAsFactors = FALSE)
# 
# # Loop over each topic
# for (topic in topic_vars) {
#   # Create the regression formula
#   formula <- as.formula(paste("media_", topic, " ~ tweet_", topic, " + tweet_", topic, ":incumbent + time_trend", sep = ""))
#   
#   # Run the regression
#   lm_model <- feols(formula, data = merged_data, vcov = "hetero")
#   
#   # Extract and store the results
#   
#   model_summary <- as.data.frame(lm_model$coeftable)
#   coefs <- model_summary$Estimate
#   se <- model_summary$`Std. Error`
#   pvals <- round(model_summary$`Pr(>|t|)`, 4)
#   coef_names <- rownames(model_summary)
#   
#   # Append to the results dataframe
#   temp_df <- data.frame(
#     topic = rep(topic, length(coefs)), 
#     coefficient_name = coef_names, 
#     coefficient = coefs, 
#     std_error = se, 
#     p_value = pvals
#   )
#   
#   main_results_incumbent <- rbind(main_results_incumbent, temp_df)
# }
# 
# 
# ## MAIN RESULTS LAG1
# 
# main_results_lag1 <- data.frame(topic = character(), 
#                                      coefficient_name = character(),
#                                      coefficient = numeric(), 
#                                      std_error = numeric(), 
#                                      p_value = numeric(),
#                                      stringsAsFactors = FALSE)
# 
# # Loop over each topic
# for (topic in topic_vars) {
#   # Create the regression formula
#   formula <- as.formula(paste("media_", topic, " ~ tweet_", topic, " + tweet_", topic, "_lag1 + time_trend", sep = ""))
#   
#   # Run the regression
#   lm_model <- feols(formula, data = merged_data, vcov = "hetero")
#   
#   # Extract and store the results
#   
#   model_summary <- as.data.frame(lm_model$coeftable)
#   coefs <- model_summary$Estimate
#   se <- model_summary$`Std. Error`
#   pvals <- round(model_summary$`Pr(>|t|)`, 4)
#   coef_names <- rownames(model_summary)
#   
#   # Append to the results dataframe
#   temp_df <- data.frame(
#     topic = rep(topic, length(coefs)), 
#     coefficient_name = coef_names, 
#     coefficient = coefs, 
#     std_error = se, 
#     p_value = pvals
#   )
#   
#   main_results_lag1 <- rbind(main_results_lag1, temp_df)
# }
# 
# ## MAIN RESULTS LAG2
# 
# main_results_lag2 <- data.frame(topic = character(), 
#                                 coefficient_name = character(),
#                                 coefficient = numeric(), 
#                                 std_error = numeric(), 
#                                 p_value = numeric(),
#                                 stringsAsFactors = FALSE)
# 
# # Loop over each topic
# for (topic in topic_vars) {
#   # Create the regression formula
#   formula <- as.formula(paste("media_", topic, " ~ tweet_", topic, " + tweet_", topic, "_lag1 + tweet_", topic, "_lag2 + time_trend", sep = ""))
#   
#   # Run the regression
#   lm_model <- feols(formula, data = merged_data, vcov = "hetero")
#   
#   # Extract and store the results
#   
#   model_summary <- as.data.frame(lm_model$coeftable)
#   coefs <- model_summary$Estimate
#   se <- model_summary$`Std. Error`
#   pvals <- round(model_summary$`Pr(>|t|)`, 4)
#   coef_names <- rownames(model_summary)
#   
#   # Append to the results dataframe
#   temp_df <- data.frame(
#     topic = rep(topic, length(coefs)), 
#     coefficient_name = coef_names, 
#     coefficient = coefs, 
#     std_error = se, 
#     p_value = pvals
#   )
#   
#   main_results_lag2 <- rbind(main_results_lag2, temp_df)
# }
# 
# 
# ## MAIN RESULTS LAG2
# 
# main_results_lead <- data.frame(topic = character(), 
#                                 coefficient_name = character(),
#                                 coefficient = numeric(), 
#                                 std_error = numeric(), 
#                                 p_value = numeric(),
#                                 stringsAsFactors = FALSE)
# 
# # Loop over each topic
# for (topic in topic_vars) {
#   # Create the regression formula
#   formula <- as.formula(paste("media_", topic, " ~ tweet_", topic, "_lead2 + tweet_", topic, "_lead1 + tweet_", topic, " + tweet_", topic, "_lag1 + tweet_", topic, "_lag2 + time_trend", sep = ""))
#   
#   # Run the regression
#   lm_model <- feols(formula, data = merged_data, vcov = "hetero")
#   
#   # Extract and store the results
#   
#   model_summary <- as.data.frame(lm_model$coeftable)
#   coefs <- model_summary$Estimate
#   se <- model_summary$`Std. Error`
#   pvals <- round(model_summary$`Pr(>|t|)`, 4)
#   coef_names <- rownames(model_summary)
#   
#   # Append to the results dataframe
#   temp_df <- data.frame(
#     topic = rep(topic, length(coefs)), 
#     coefficient_name = coef_names, 
#     coefficient = coefs, 
#     std_error = se, 
#     p_value = pvals
#   )
#   
#   main_results_lead <- rbind(main_results_lead, temp_df)
# }
# 
# 





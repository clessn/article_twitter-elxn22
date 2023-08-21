library(tidyverse)



# Connecting to hub 2.0
clessnhub::login(
  Sys.getenv("HUB_USERNAME"),
  Sys.getenv("HUB_PASSWORD"),
  Sys.getenv("HUB_URL"))

### Get candidate Tweets ###

get_qc_candidates <- function(){
  filter <- clessnhub::create_filter(type = "candidate",
                                     data = list(currentParty = 
                                                   "Québec solidaire"))
  QS <- clessnhub::get_items(table = 'persons', filter = filter, 
                             download_data = TRUE)
  
  filter <- clessnhub::create_filter(type = "candidate",
                                     data = list(currentParty = 
                                                   "Parti conservateur du Québec"))
  PCQ <- clessnhub::get_items(table = 'persons', filter = filter, 
                              download_data = TRUE)
  
  filter <- clessnhub::create_filter(type = "candidate",
                                     data = list(currentParty = 
                                                   "Parti libéral du Québec"))
  PLQ <- clessnhub::get_items(table = 'persons', filter = filter, 
                              download_data = TRUE)
  
  filter <- clessnhub::create_filter(type = "candidate",
                                     data = list(currentParty = 
                                                   "Coalition avenir Québec"))
  CAQ <- clessnhub::get_items(table = 'persons', filter = filter, 
                              download_data = TRUE)
  
  filter <- clessnhub::create_filter(type = "candidate",
                                     data = list(currentParty = 
                                                   "Parti québécois"))
  PQ <- clessnhub::get_items(table = 'persons', filter = filter, 
                             download_data = TRUE)
  
  Candidates <- rbind(QS, PCQ, PLQ, CAQ, PQ)
  return(Candidates)
}

candidate_tweets <- get_qc_candidates()

#### Get party tweets ####

get_party_tweets <- function(begin_date, end_date){
  tictoc::tic()
  myfilter <- clessnhub::create_filter(
    metadata = list(personType__regex="party"),
    data = list(creationDate__gte = begin_date,
                creationDate__lte = end_date))
  tweets_politicians <- clessnhub::get_items('tweets', myfilter, 
                                             download_data = TRUE)
  tictoc::toc()
  return(tweets_politicians)
}

party_tweets <- get_party_tweets("2022-08-01", "2022-11-01")

#### Get politicians Tweet ####

get_politicians_tweets <- function(begin_date, end_date){
  tictoc::tic()
  myfilter <- clessnhub::create_filter(
    metadata = list(personType__regex="candidate"),
    data = list(creationDate__gte = begin_date,
                creationDate__lte = end_date))
  tweets_politicians <- clessnhub::get_items('tweets', myfilter, 
                                             download_data = TRUE)
  tictoc::toc()
  return(tweets_politicians)
}

politicians_tweets <- get_politicians_tweets("2022-08-01", "2022-11-01")


#### Create a clean dataframe containing the relevant variables ####

CleanDataChefs <- politicians_tweets %>% 
  select(data.creationDate, data.creationTime, metadata.twitterHandle, data.text,
         type, data.mentions) %>% 
  filter(metadata.twitterHandle %in% c("francoislegault", "GNadeauDubois", 
                                       "PaulPlamondon", "E_Duhaime", "DomAnglade"))

CleanDataParty <- party_tweets %>% 
  select(data.creationDate, data.creationTime, metadata.twitterHandle, data.text, 
         type, data.mentions) %>% 
  filter(metadata.twitterHandle %in% c("@QuebecSolidaire", "@coalitionavenir", 
                                       "@LiberalQuebec", "@partiquebecois",
                                       "@PconservateurQc"))


#### Code for topic modelling ####

  
  
  
  


















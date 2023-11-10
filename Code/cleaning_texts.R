##### %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% MOT DE BIENVENUE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Bienvenue dans le PREMIER Code R de l'article 2 du mémoire d'Adrien Cloutier
# sur l'évolution de l'opinion publique par rapport au cannabis mesuré AU TRAVERS des médias
# J'espère que vous le trouverez simple et logique.

# À NOTER: IL S'AGIT SEULEMENT DU CODE DE NETTOYAGE DES PDF TIRÉS D'EUREKA.

# Pour toute question ou demande de reproduction, merci de me contacter au 418-590-2605
# ou adrien.cloutier.1@ulaval.ca.
# Au plaisir!


##### %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Notes et rappels à moi-même %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




########################################################################################################### ##
####################################################### PATH #################################################
########################################################################################################### ##

library(readtext)
library(rebus)
library(stringr)
library(googleLanguageR) # https://cran.r-project.org/web/packages/googleLanguageR/vignettes/setup.html
library(tidyverse)

# library(pdftools)

# Établir le chemin d'arborescence
path <- setwd("/Users/adrien/Library/CloudStorage/Dropbox/Travail/Universite_Laval/publications/article_twitter-elxn22/_SharedFolder_article_twitter-elxn22")

########################################################################################################### ##
################################################### Créer les txt ############################################
########################################################################################################### ##


# ATTENTION: SI LES TXT SONT DÉJÀ CRÉÉS, NETTOYÉS ET RÉENREGISTRÉ DANS LEUR DOSSIER, VOUS POUVEZ PASSER
# À L'ÉTAPE DE LA TRADUCTION SANS REROULER LE CODE DE CETTE SECTION.

# Trouver tous les documents dans mes dossiers
document_list <- list.files(path='texts', pattern='pdf$', recursive=TRUE)

document_texts <- list()
# Pour enregistrer le path (va servir à réenregistrer les textes propres dans nos dossiers)
document_path <- list()

# Traiter tous les documents
#for (i in 1) # si on ne veut pas faire les textes au complet
for (i in 1:length(document_list))
{
  print(paste0(i, '/', length(document_list)))
  pdf_path <- paste0("texts/", document_list[i])
  txt_path <- paste0(pdf_path, '.txt')
  html_path <- paste0(pdf_path, '.html')
  system(paste('./pdftotext', paste0(' -raw ', ' -enc UTF-8 ', pdf_path, ' ', txt_path)), wait = TRUE)
  # system(paste('./pdftohtml', paste0(' ', pdf_path, ' ', html_path)), wait = TRUE)
  document_texts[[i]] <- paste0(unlist(readLines(txt_path, warn = FALSE)), collapse = " ")
  document_path[[i]] <- txt_path # pour aller chercher les paths de tous les documents
}


########################################################################################################### ##
############################################## Nettoyer les txt ##############################################
########################################################################################################### ##

# original: Le texte original qu'on veut remplacer
# before: ce qui vient AVANT ce qu'on veut remplacer
# after: ce qui vient APRÈS ce qu'on veut remplacer
# replacewith: ce qu'on veut mettre À LA PLACE de ce qu'on remplace (le défaults met rien)
replaceRange <- function(original, before, after, replaceWith = '')
{
  regex <- paste0(before, '(.*?)' , after)
  return(gsub(regex, replaceWith, original))
}

document_texts[[1]]

# On utilise la fonction pour enlever des txt les passages qui ne sont pas des textes médiatiques
# On met le début du passage entre les premiers ' ', et la fin entre les 2e ' '
result <- replaceRange(document_texts, 'Documents sauvegardés', 'CEDROM-SNi Inc. ')
result <- replaceRange(result, '\fSommaire', 'Nom de ')
result <- replaceRange(result, 'la source', 'words ')
result <- replaceRange(result, 'la source', 'mots ')
result <- replaceRange(result, '©', '\f')
result <- replaceRange(result, 'Cet article est paru dans', 'Nom de')
result <- replaceRange(result, 'Cet article est paru dans', '\f')
#result <- replaceRange(result, 'Image Credit:', ') ')
#result <- replaceRange(result, 'With files from', 'About CBC News')
result <- replaceRange(result, 'LES DROITS D', 'FAÇON QUE CE SOIT. ')
result <- replaceRange(result, 'Le présent document est protégé', 'fins de visualisation personnelle et temporaire.')
result <- replaceRange(result, 'news·', '·TTA·')
result <- replaceRange(result, 'Copyright', 'All Rights Reserved.')




# on vérifie un résultat
# result[[1]]


# Créer des fonctions qui utilise les regex pour continuer le nettoyage
# pour tester des regex (https://regex101.com/)
replaceTabsWithSpace <- function(x) gsub("\t", " ", x) # mettre un espace au lieu des tabulations
replaceBulletsToDots <- function(x) gsub("\\-{2,}", ". ", x) # enlever les bullets points
removeMultipleSpaces <- function(x) gsub("\\s{2,}", " ", x) # enlève les espaces de trop
removeDashes <- function(x) gsub("\\/{2,}", " ", x) # Enlève les dash
removeRealDashes <- function(x) gsub("- ", "", x) # enlève les dash ?!
removeQuotes <- function(x) gsub("\"", "", x) # essai de supprimer les ""
removePhrase <- function(x) str_replace_all(x, fixed("About CBC News"), replacement = "")
removeNotes <- function(x) str_replace_all(x, fixed("Note(s) : "), replacement = "")
removeCollab <- function(x) str_replace_all(x, fixed("Collaboration spéciale"), replacement = "")
removeJdeM <- function(x) str_replace_all(x, fixed("Journal de Montréal"), replacement = "")
removeJdeQ <- function(x) str_replace_all(x, fixed("Journal de Québec"), replacement = "")
removeQMI <- function(x) str_replace_all(x, fixed("Agence QMI"), replacement = "")
removeLaPresse <- function(x) str_replace_all(x, fixed("La Presse"), replacement = "")
removeLeDevoir <- function(x) str_replace_all(x, fixed("Le Devoir"), replacement = "")
removeRadioCan <- function(x) str_replace_all(x, fixed("Radio-Canada"), replacement = "")
removeTStar <- function(x) str_replace_all(x, fixed("Toronto Star"), replacement = "")
removeTStarMaj <- function(x) str_replace_all(x, fixed("TORONTO STAR"), replacement = "")
removeCBC <- function(x) str_replace_all(x, fixed("CBC News"), replacement = "")
removeFiles <- function(x) str_replace_all(x, fixed("With files from"), replacement = "")
removeCP <- function(x) str_replace_all(x, fixed("Canadian Press"), replacement = "")
removeCPMaj <- function(x) str_replace_all(x, fixed("CANADIAN PRESS"), replacement = "")
removeAP <- function(x) str_replace_all(x, fixed("Associated Press"), replacement = "")
removeWeb <- function(x) str_replace_all(x, fixed("(web site)"), replacement = "")
removeSites <- function(x) str_replace_all(x, fixed("site web"), replacement = "")
removePhoto <- function(x) str_replace_all(x, fixed("file photo"), replacement = "")
removePhotoMaj <- function(x) str_replace_all(x, fixed("FILE PHOTO"), replacement = "")
removeRights <- function(x) str_replace_all(x, fixed("All rights reserved."), replacement = "")
removeParuDans <- function(x) str_replace_all(x, fixed("Cet article est paru dans"), replacement = "")
removeNomDe <- function(x) str_replace_all(x, fixed("Nom de"), replacement = "")
removeStandards <- function(x) str_replace_all(x, fixed("CBC's Journalistic Standards and Practices"), replacement = "")
removeIllustration <- function(x) str_replace_all(x, fixed("Illustration(s) :"), replacement = "")
removeBureauP <- function(x) str_replace_all(x, fixed("Bureau parlementaire"), replacement = "")
removeBureauE <- function(x) str_replace_all(x, fixed("Bureau d'enquête"), replacement = "")
removeResponsable <- function(x) str_replace_all(x, fixed("n'est aucunement responsable du contenu des sites externes"), replacement = "")
removeTexteDe <- function(x) str_replace_all(x, fixed("Un texte de"), replacement = "")
removeLireAussi <- function(x) str_replace_all(x, fixed("À lire aussi"), replacement = "")


# On applique nos fonctions
result <- lapply(result, replaceTabsWithSpace)
result <- lapply(result, replaceBulletsToDots)
result <- lapply(result, removeMultipleSpaces)
result <- lapply(result, removeDashes)
result <- lapply(result, removeRealDashes)
result <- lapply(result, removeQuotes)
result <- lapply(result, removePhrase)
result <- lapply(result, removeNotes)
result <- lapply(result, removeCollab)
result <- lapply(result, removeJdeM)
result <- lapply(result, removeJdeQ)
result <- lapply(result, removeQMI)
result <- lapply(result, removeLaPresse)
result <- lapply(result, removeLeDevoir)
result <- lapply(result, removeRadioCan)
result <- lapply(result, removeTStar)
result <- lapply(result, removeTStarMaj)
result <- lapply(result, removeCBC)
result <- lapply(result, removeFiles)
result <- lapply(result, removeCP)
result <- lapply(result, removeCPMaj)
result <- lapply(result, removeAP)
result <- lapply(result, removeWeb)
result <- lapply(result, removeSites)
result <- lapply(result, removePhoto)
result <- lapply(result, removePhotoMaj)
result <- lapply(result, removeRights)
result <- lapply(result, removeParuDans)
result <- lapply(result, removeNomDe)
result <- lapply(result, removeStandards)
result <- lapply(result, removeIllustration)
result <- lapply(result, removeBureauP)
result <- lapply(result, removeBureauE)
result <- lapply(result, removeResponsable)
result <- lapply(result, removeTexteDe)
result <- lapply(result, removeLireAussi)


# On revérifie
# result[[1]]

########################################################################################################### ##
######################################## Réenregistrer dans nos dossier ######################################
########################################################################################################### ##

# Créer un data frame avec notre list
df <- data.frame(matrix(unlist(result),
                        nrow = length(result), byrow=T),
                 stringsAsFactors=FALSE)

# Ajouter au data frame le path vers chacun des dossiers
df[,2] <- data.frame(matrix(unlist(document_path),
                            nrow = length(document_path), byrow=T),
                     stringsAsFactors=FALSE)

# Avec une loop, renvoyer chaque txt nettoyé dans son dossier!
for(i in 1:length(document_list)) {
  write.table(df[i, 1],  paste0(path, "/_SharedFolder_article_twitter-elxn22/", df[i, 2]))
}


########################################################################################################### ##
################################################# Charger les txt ############################################
########################################################################################################### ##

# Charger tous les txt français nettoyés
LaPresse <- readtext(paste0(path, "/_SharedFolder_article_twitter-elxn22/texts/lapresse/")) %>%
   filter(grepl(".txt", doc_id))

LeDevoir <- readtext(paste0(path, "/_SharedFolder_article_twitter-elxn22/texts/ledevoir/")) %>%
   filter(grepl(".txt", doc_id))

RadioCan <- readtext(paste0(path, "/_SharedFolder_article_twitter-elxn22/texts/radiocanada/")) %>%
   filter(grepl(".txt", doc_id))

JdeM <- readtext(paste0(path, "/_SharedFolder_article_twitter-elxn22/texts/jdem/")) %>%
   filter(grepl(".txt", doc_id))


########################################################################################################### ##
############################################### Création d'un csv ############################################
########################################################################################################### ##

# Créer un df vraiment nice, parfait pour l'analyse
# MediaClean <- LaPresse_trad %>%
# bind_rows(LaPresse2_trad, LeDevoir_trad, RadioCan_trad, JdeM_trad, CBC, TStar) %>%
MediaClean <- LaPresse %>%
  bind_rows(LeDevoir, RadioCan, JdeM) %>%
  mutate(media = ifelse(grepl("^jdem_", doc_id), "JdeM",
                 ifelse(grepl("^lapresse_", doc_id), "LaPresse",
                 ifelse(grepl("^ledevoir_", doc_id), "LeDevoir",
                 ifelse(grepl("^radiocanada_", doc_id), "RadioCan", "Error!!")))),
         month = ifelse(grepl("_01_", doc_id), "01",
                 ifelse(grepl("_02_", doc_id), "02",
                 ifelse(grepl("_03_", doc_id), "03",
                 ifelse(grepl("_04_", doc_id), "04",
                 ifelse(grepl("_05_", doc_id), "05",
                 ifelse(grepl("_06_", doc_id), "06",
                 ifelse(grepl("_07_", doc_id), "07",
                 ifelse(grepl("_08_", doc_id), "08",
                 ifelse(grepl("_09_", doc_id), "09",
                 ifelse(grepl("_10_", doc_id), "10",
                 ifelse(grepl("_11_", doc_id), "11",
                 ifelse(grepl("_12_", doc_id), "12", "Error!!")))))))))))),
          day = ifelse(grepl("01.pdf.txt$", doc_id), "01",
                 ifelse(grepl("02.pdf.txt$", doc_id), "02",
                 ifelse(grepl("03.pdf.txt$", doc_id), "03",
                 ifelse(grepl("04.pdf.txt$", doc_id), "04",
                 ifelse(grepl("05.pdf.txt$", doc_id), "05",
                 ifelse(grepl("06.pdf.txt$", doc_id), "06",
                 ifelse(grepl("07.pdf.txt$", doc_id), "07",
                 ifelse(grepl("08.pdf.txt$", doc_id), "08",
                 ifelse(grepl("09.pdf.txt$", doc_id), "09",
                 ifelse(grepl("10.pdf.txt$", doc_id), "10",
                 ifelse(grepl("11.pdf.txt$", doc_id), "11",
                 ifelse(grepl("12.pdf.txt$", doc_id), "12",
                 ifelse(grepl("13.pdf.txt$", doc_id), "13",
                 ifelse(grepl("13_2.pdf.txt$", doc_id), "13",
                 ifelse(grepl("14.pdf.txt$", doc_id), "14",
                 ifelse(grepl("15.pdf.txt$", doc_id), "15",
                 ifelse(grepl("16.pdf.txt$", doc_id), "16",
                 ifelse(grepl("16_2.pdf.txt$", doc_id), "16",
                 ifelse(grepl("17.pdf.txt$", doc_id), "17",
                 ifelse(grepl("18.pdf.txt$", doc_id), "18",
                 ifelse(grepl("19.pdf.txt$", doc_id), "19",
                 ifelse(grepl("20.pdf.txt$", doc_id), "20",
                 ifelse(grepl("20_2.pdf.txt$", doc_id), "20",
                 ifelse(grepl("21.pdf.txt$", doc_id), "21",
                 ifelse(grepl("22.pdf.txt$", doc_id), "22",
                 ifelse(grepl("23.pdf.txt$", doc_id), "23",
                 ifelse(grepl("24.pdf.txt$", doc_id), "24",
                 ifelse(grepl("25.pdf.txt$", doc_id), "25",
                 ifelse(grepl("26.pdf.txt$", doc_id), "26",
                 ifelse(grepl("27.pdf.txt$", doc_id), "27",
                 ifelse(grepl("28.pdf.txt$", doc_id), "28",
                 ifelse(grepl("29.pdf.txt$", doc_id), "29",
                 ifelse(grepl("30.pdf.txt$", doc_id), "30",
                 ifelse(grepl("31.pdf.txt$", doc_id), "31", "Error!!")))))))))))))))))))))))))))))))))),
          doc_id = paste0(day, "-", month))

########################################################################################################### ##
################################################## Enregistrer ###############################################
########################################################################################################### ##

# Enregistrer le csv
write.csv(MediaClean, file = "Data/MediaClean.csv")

########################################################################################################### ##
####################################################### FIN ##################################################
########################################################################################################### ##

error <- MediaClean %>%
  filter(day == "Error!!")

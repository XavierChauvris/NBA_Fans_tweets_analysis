#' Title: NLP Case 1
#' NAME: Xavier Chauvris
#' Date: MAR 15 2022
#' 

# To limit errors please run this code
Sys.setlocale('LC_ALL','C')

#Getting working directory and setting it 
getwd()
setwd("C:/Users/xavie/OneDrive/Documents/Text-Mining-NLP/Case/Case I/Data")


#Loading libraries
library(tm)
library(ggplot2)
library(ggthemes)
library(stringi)
library(qdap)
library(stringr)
library(spelling)
library(hunspell)
library(mgsub)
library(pbapply)
library(dplyr)
library(data.table)
library(wordcloud)
library(RColorBrewer)
library(epiDisplay)
library(forcats)


################################################################################
# Loading the data sets
################################################################################

#Combining all files pre-covid
files <- list.files(pattern='A_|B_|C_|E_|F_')
temp <- lapply(files, fread, sep=',')
data <- rbindlist(temp)
pre_covid_full <- data


#Combining all data from post-suspension
files_post <- list.files(pattern='I_|J_|K_|L_|M_')
temp_post <- lapply(files_post, fread, sep=',')
data_post <-rbindlist(temp_post)
post_covid_full <- data_post


#Selecting only 20% of the data setfor 
pre_covid <- pre_covid_full[sample(nrow(pre_covid_full), size=0.20*nrow(pre_covid_full))]
post_covid <- post_covid_full[sample(nrow(post_covid_full), size=0.20*nrow(post_covid_full))]


################################################################################
# Exploring the data set
################################################################################

#Printing column names to the Console
colnames(pre_covid)
colnames(post_covid)

#Printing team columns values
table(pre_covid$team)
table(post_covid$team)

#Printing frequency tables for team columns values
tab1(pre_covid$team, sort.group='decreasing', cum.percent="FALSE")
tab1(post_covid$team, sort.group='decreasing', cum.percent="FALSE")


###############################################################################
# Pre Covid Cleaning
###############################################################################

#Printing first 10 rows to the console
head(pre_covid$text, 10)

#Removing punctuation
text_pre <- gsub(pattern='\\W', replacement=' ', pre_covid$text)
head(text_pre, 10) #Checking Results

#Removing Numbers
text_pre <- gsub(pattern='\\d', replacement=' ', text_pre)
head(text_pre, 10) #Checking results

#Lowering every characters
text_pre <- tolower(text_pre)
head(text_pre, 10) #Checking results

#Removing non-alpha numeric characters
text_pre <- str_replace_all(text_pre, "[^[:alnum:]]", " ")
head(text_pre, 10) #Checking results

#Remove stopwords from smart lexicon
text_pre <- removeWords(text_pre, c(stopwords('smart'), 'rt', 'https', 'team', 
                                    'player', 'players', 'nba', 'coach', 'manager', 'fan',
                                    'fans', 'game', 'games', 'basketball', 'live', 'season',
                                    'year', 'years', 'sports'))
head(text_pre, 10) #Checking results

#Remove stopwords residuals 
text_pre <- gsub(pattern='\\b[a-z]{1,2}\\b', replacement=' ', text_pre)
head(text_pre, 10) #Checking results

#Remove custom stopwords, special characters, and white spaces generated
text_pre <- stripWhitespace(text_pre)
head(text_pre, 10) #Checking results



###############################################################################
# Post Covid Cleaning
###############################################################################

#Printing first 10 rows to the console
head(post_covid$text, 10)

#Removing punctuation
text_post <- gsub(pattern='\\W', replacement=' ', post_covid$text)
head(text_post, 10) #Checking Results

#Removing Numbers
text_post <- gsub(pattern='\\d', replacement=' ', text_post)
head(text_post, 10) #Checking results

#Lowering every characters
text_post <- tolower(text_post)
head(text_post, 10) #Checking results

#Removing non-alpha numeric characters
text_post <- str_replace_all(text_post, "[^[:alnum:]]", " ")
head(text_post, 10) #Checking results

#Remove stopwords from smart lexicon
text_post <- removeWords(text_post, c(stopwords('smart'), 'rt', 'https', 'team', 
                                      'player', 'players', 'nba', 'coach', 'manager', 'fan',
                                      'fans', 'game', 'games', 'basketball', 'live', 'season',
                                      'year', 'years', 'sports'))
head(text_post, 10) #Checking results

#Remove stopwords residuals 
text_post <- gsub(pattern='\\b[a-z]{1,2}\\b', replacement=' ', text_post)
head(text_post, 10) #Checking results

#Remove custom stopwords, special characters, and white spaces generated
text_post <- stripWhitespace(text_post)
head(text_pre, 10) #Checking results



################################################################################
#Building Corpus and DTM
################################################################################

#Make it into a corpus
text_pre_corpus <- VCorpus(VectorSource(text_pre))

#Transforming corpus into DTM and converting into simple matrix
pre_dtm <- DocumentTermMatrix(text_pre_corpus)
reduced_pre_dtm <- removeSparseTerms(pre_dtm, sparse=0.99) #Reducing the number of column in a matrix
pre_mat <- as.matrix(reduced_pre_dtm) #Converting reduced matrix to a simple matrix


#Make it into a corpus
text_post_corpus <- VCorpus(VectorSource(text_post))

#Transforming corpus into DTM and converting into simple matrix
post_dtm <- DocumentTermMatrix(text_post_corpus)
reduced_post_dtm <- removeSparseTerms(post_dtm, sparse=0.99) #Reducing number of column
post_mat <- as.matrix(reduced_post_dtm) #Converting reduced matrix to a simple matrix



################################################################################
#Nike
################################################################################

# Use sum with stri_count on the newt txt object
# with "trump", "biden" and in the last one check for "virus" OR "vaccine"
lebron_pre  <- sum(stri_count(text_pre, fixed ='lebron'))
kawhi_pre  <- sum(stri_count(text_pre, fixed ='kawhi'))
giannis_pre <- sum(stri_count(text_pre, regex ='giannis'))

# Organize term objects into a data frame
termFreq_pre_nike <- data.frame(terms = c('lebron','kawhi','giannis'),
                       freq  = c(lebron_pre,kawhi_pre, giannis_pre))


lebron_post  <- sum(stri_count(text_post, fixed ='lebron'))
kawhi_post  <- sum(stri_count(text_post, fixed ='kawhi'))
giannis_post <- sum(stri_count(text_post, regex ='giannis'))

# Organize term objects into a data frame
termFreq_post_nike <- data.frame(terms = c('lebron','kawhi','giannis'),
                       freq  = c(lebron_post,kawhi_post, giannis_post))


################################################################################
#Adidas
################################################################################

# Use sum with stri_count on the newt txt object
# with "trump", "biden" and in the last one check for "virus" OR "vaccine"
lillard_pre  <- sum(stri_count(text_pre, fixed ='lillard'))
harden_pre  <- sum(stri_count(text_pre, fixed ='harden'))
curry_pre <- sum(stri_count(text_pre, regex ='curry'))

# Organize term objects into a data frame
termFreq_pre_adidas <- data.frame(terms = c('lillard','harden','curry'),
                           freq  = c(lillard_pre,harden_pre, curry_pre))


lillard_post  <- sum(stri_count(text_post, fixed ='lillard'))
harden_post  <- sum(stri_count(text_post, fixed ='harden'))
curry_post <- sum(stri_count(text_post, regex ='curry'))

# Organize term objects into a data frame
termFreq_post_adidas <- data.frame(terms = c('lillard','harden','curry'),
                            freq  = c(lillard_post,harden_post, curry_post))



################################################################################
# Most frequent term pre & post covid
################################################################################

# Get the most frequent terms
topTermspre <- colSums(pre_mat)
topTermspre <- data.frame(terms = colnames(reduced_pre_dtm), freq = topTermspre)


topTermspost <- colSums(post_mat)
topTermspost <- data.frame(terms = colnames(reduced_post_dtm), freq = topTermspost)


################################################################################
# Visualization 
################################################################################

topTermspre %>%
  mutate(name = fct_reorder(terms, freq)) %>%
  ggplot( aes(x=freq, y=terms)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  xlab("") +
  theme_bw()


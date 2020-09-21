####################################################### Loading Required Libraries #########################################################

library(tidyr)
library(tm)
library(wordcloud)
library(RWeka)
library(textclean)
library(doSNOW)
library(dplyr)

####################################################### LOading Given Data Sets and Seperating Visitors Chat ###############################

setwd("D:/Data Science Project/Conversation Mining/Chat Transcripts for Project")

chat_transcript <- read.csv("D:/Data Science Project/Conversation Mining/chat_transcript.csv", col.names = "Chat")

chat_transcript$Chat <- iconv(chat_transcript$Chat, from = 'UTF-8', to = 'ASCII//TRANSLIT')

visitor<-data.frame(Visitors_Chat = grep(chat_transcript$Chat,pattern = "Visitor (.*)",value = T))

#library(tidyr)
Clean_Chat<- visitor %>% separate(Visitors_Chat, c("A", "B"),": ",fill = "left")


####################################################### Creating Corpus ####################################################################

corpus <- Corpus(VectorSource(Clean_Chat$B))

stopwdrds = readLines("D:/Data Science/Assignments/R Codes/Text Mining/stop.txt")
pos.words=scan("D:/Data Science/Assignments/R Codes/Text Mining/positive-words.txt", what="character", comment.char=";")	# read-in positive-words.txt
neg.words=scan("D:/Data Science/Assignments/R Codes/Text Mining/negative-words.txt", what="character", comment.char=";") 	# read-in negative-words.txt


#library(tm)
corpus_clean = tm_map(corpus, tolower)
corpus_clean = tm_map(corpus_clean, removeWords,c(stopwords("english"),stopwdrds,"hi","what","can","i","ananya","mounica"))
corpus_clean = tm_map(corpus_clean, removePunctuation)
corpus_clean <- tm_map(corpus_clean,removeNumbers)
corpus_clean = tm_map(corpus_clean, stripWhitespace)
corpus_clean <- tm_map(corpus_clean, stemDocument)
inspect(corpus_clean[c(1:10)])

####################################################### Creating Term Document Matrix (TDM) ################################################

#term document metrix
tdm <- TermDocumentMatrix(corpus_clean)
inspect(tdm)

a0 <- NULL

cl <- makeCluster(4, type="SOCK") 
registerDoSNOW (cl)

# getting the indexes of documents having count of words = 0
for (i1 in 1:ncol(tdm))
{ if (sum(tdm[, i1]) == 0) {a0 = c(a0, i1)} }

# Removing empty docs 
tdm <- tdm[,-a0]

# Document term matrix 
dtm0 <- t(tdm)
inspect(dtm0)


####################################################### Creating LDA Model with Gibbs Sampling ################################################

#load topic models library
library(topicmodels)

#Set parameters for Gibbs sampling
burnin <- 4000  #iter, burnin, thin: These parameters control how many Gibbs sampling draws are made.
iter <- 2000    #The first burnin iterations are discarded and then every thin iteration is returned for iter iterations.
thin <- 500
seed <-list(2003,5,63,100001,765)
nstart <- 5
best <- TRUE #best: All draws are returned if best=FALSE, otherwise only the draw with the highest posterior likelihood over all runs is returned

#Number of topics
k <- 6

#Run LDA using Gibbs sampling
ldaOut <-LDA(dtm0,k,method= "Gibbs", control=list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin))
ldaOut.topics <- as.matrix(topics(ldaOut))
ldaOut.terms <- as.matrix(terms(ldaOut,10))
ldaOut.terms
#topicProbabilities <- as.data.frame(ldaOut@gamma)

library(tidytext)

ap_topics <- tidy(ldaOut, matrix = "beta")

library(ggplot2)

ap_top_terms <- ap_topics %>%group_by(topic) %>% top_n(10, beta) %>%ungroup()%>% arrange(topic, -beta)

ap_top_terms %>% mutate(term = reorder(term, beta)) %>% ggplot(aes(term, beta,fill = factor(topic))) + geom_col(show.legend = FALSE)+
  facet_wrap(~topic, scale = "free") +coord_flip()


# Topic 1 - Training, Mode of Training
# Topic 2 - 
# Topic 3 - Location Information
# Topic 4 - Email and Contact Inforamtion 
# Topic 5 - Fees, Duration, Timings Types of Course and Structure Placements and Jobs
# Topic 6 - Data Science
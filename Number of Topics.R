library(dplyr)
library(tm) #to process text
library(tidyverse)
library(SnowballC)
library(parallel)
library(doSNOW)
library(topicmodels)
library(tidytext)
library(ldatuning)


#library(SnowballC) # for stemming
#library(stringr)

#library(gutenbergr)
#library(stringr)

chat_transcript <- read.csv("D:/Data Science Project/Conversation Mining/chat_transcript.csv", col.names = "Chat")

chat_transcript$Chat <- iconv(chat_transcript$Chat, from = 'UTF-8', to = 'ASCII//TRANSLIT')

visitor<-data.frame(Visitors_Chat = grep(chat_transcript$Chat,pattern = "Visitor (.*)",value = T))

Clean_Chat<- visitor %>% separate(Visitors_Chat, c("A", "B"),": ",fill = "left")

corpus <- Corpus(VectorSource(Clean_Chat$B))

stopwdrds <- readLines("https://raw.githubusercontent.com/niranjan2408/Training-Models-in-R/master/Text%20Mining/stop.txt")
pos.words <- readLines("https://raw.githubusercontent.com/niranjan2408/Training-Models-in-R/master/Text%20Mining/positive-words.txt")
neg.words <- readLines("https://raw.githubusercontent.com/niranjan2408/Training-Models-in-R/master/Text%20Mining/negative-words.txt")

corpus_clean = tm_map(corpus, tolower)
corpus_clean = tm_map(corpus_clean, removeWords,c(stopwords("english"),stopwdrds,"hi","hii","what","can","i","ananya","mounica","hey","mam","hyd"))
corpus_clean = tm_map(corpus_clean, removePunctuation)
corpus_clean <- tm_map(corpus_clean,removeNumbers)
corpus_clean = tm_map(corpus_clean, stripWhitespace)
inspect(corpus_clean[c(1:10)])


#term document metrix
tdm <- TermDocumentMatrix(corpus_clean)
inspect(tdm)

a0 <- NULL
cl <- makeCluster(8, type="SOCK")
registerDoSNOW (cl)
# getting the indexes of documents having count of words = 0
for (i1 in 1:ncol(tdm)) { if (sum(tdm[, i1]) == 0) {a0 = c(a0, i1)} }

stopCluster(cl)
# Removing empty docs 
tdm <- tdm[,-a0]
#inspect(tdm)

# Document term matrix 
dtm <- t(tdm)
inspect(dtm)

#Set parameters for Gibbs sampling
burnin <- 4000  #iter, burnin, thin: These parameters control how many Gibbs sampling draws are made.
iter <- 2000    #The first burnin iterations are discarded and then every thin iteration is returned for iter iterations.
thin <- 500
seed <-list(2003,5,63,100001,765)
nstart <- 5
best <- TRUE #best: All draws are returned if best=FALSE, otherwise only the draw with the highest posterior likelihood over all runs is returned

#Number of topics
k <- 6

chapters_lda <- LDA(dtm,k,method= "Gibbs", control=list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin))
chapters_lda

# tease out the data of the topics
chapter_topics <- tidy(chapters_lda, matrix = "beta")
chapter_topics

# tease out the data of 20 top terms
top_terms <- chapter_topics %>%  group_by(topic) %>%  top_n(20, beta) %>%  ungroup() %>%  arrange(topic, -beta)
top_terms

# Plot the 20 top terms per topic
top_terms %>%  mutate(term = reorder(term, beta)) %>%  ggplot(aes(term, beta, fill = factor(topic))) +  geom_col(show.legend = FALSE) +  facet_wrap(~ topic, scales = "free") +  coord_flip()

result <- FindTopicsNumber(
  dtm,
  topics = seq(from = 2, to = 20, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin),
  mc.cores = 2L,
  verbose = TRUE)

FindTopicsNumber_plot(result)



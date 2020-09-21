####################################################### Loading Required Libraries #########################################################

library(tidyr)
library(tm)
library(textclean)
library(doSNOW)
library(dplyr)
library(stringi)
library(LDAvis)
library(topicmodels)

####################################################### LOading Given Data Sets and Seperating Visitors Chat ###############################

#chat_transcript <- read.csv("D:/Data Science Project/Conversation Mining/Final Folder/Deployment/chat_transcript.csv", col.names = "Chat")
chat_transcript$Chat <- iconv(chat_transcript$Chat, from = 'UTF-8', to = 'ASCII//TRANSLIT')
visitor<-data.frame(Visitors_Chat = grep(chat_transcript$Chat,pattern = "Visitor (.*)",value = T))
Clean_Chat<- visitor %>% separate(Visitors_Chat, c("A", "B"),": ",fill = "left")

####################################################### Creating Corpus ####################################################################

stopwdrds <- readLines("https://raw.githubusercontent.com/niranjan2408/Training-Models-in-R/master/Text%20Mining/stop.txt")

corpus <- Corpus(VectorSource(Clean_Chat$B))
corpus_clean = tm_map(corpus, tolower)
corpus_clean = tm_map(corpus_clean, removeWords,c(stopwords("english"),stopwdrds,"hi","what","can","i","ananya","mounica","mam","excelr","hyd","bye"))
corpus_clean = tm_map(corpus_clean, removePunctuation)
corpus_clean <- tm_map(corpus_clean,removeNumbers)
corpus_clean = tm_map(corpus_clean, stripWhitespace)
#corpus_clean <- tm_map(corpus_clean, stemDocument)
inspect(corpus_clean[c(1:10)])

####################################################### Creating Term Document Matrix (TDM) ################################################

#term document metrix
tdm <- TermDocumentMatrix(corpus_clean)
inspect(tdm)

a0 <- NULL
cl <- makeCluster(8, type="SOCK") 
registerDoSNOW (cl)
for (i1 in 1:ncol(tdm))
{ if (sum(tdm[, i1]) == 0) {a0 = c(a0, i1)} }
stopCluster(cl)
tdm <- tdm[,-a0]
dtm <- t(tdm)
inspect(dtm)

####################################################### Creating LDA Model with Gibbs Sampling ################################################

#Number of topics
#k <- 12
k <- input$NumberOfTopics

#Run LDA using Gibbs sampling
fitted <-LDA(dtm,k,method= "Gibbs", control = list(seed = list(2003,5,63,100001,765),best=TRUE, burnin = 4000, thin = 500, iter = 2000,nstart=5))
LDA.topics <- topics(fitted, 1)
#LDA_topic.terms <- as.data.frame(terms(fitted, 10), stringsAsFactors = FALSE)
doctopics.df <- as.data.frame(LDA.topics)
doctopics.df <- transmute(doctopics.df, ChatID = rownames(doctopics.df), Topic = LDA.topics)
doctopics.df$ChatID <- as.integer(doctopics.df$ChatID)

## Adds topic number to original dataframe of lessons
Clean_Chat["ChatID"] <- c(1:nrow(Clean_Chat))
Clean_Chat <- inner_join(Clean_Chat, doctopics.df, by = "ChatID")

new_corpus <- Corpus(VectorSource(Clean_Chat$B))

phi <- posterior(fitted)$terms %>% as.matrix
theta <- posterior(fitted)$topics %>% as.matrix
vocab <- colnames(phi)
doc_length <- vector()
for (i in 1:length(new_corpus)) {
  temp <- paste(new_corpus[[i]]$content, collapse = ' ')
  doc_length <- c(doc_length, stri_count(temp, regex = '\\S+'))
}
temp_frequency <- as.matrix(dtm)
freq_matrix <- data.frame(ST = colnames(temp_frequency),Freq = colSums(temp_frequency))
json_lda <- createJSON(phi = phi, theta = theta,vocab = vocab,doc.length = doc_length,term.frequency = freq_matrix$Freq)
serVis(json_lda)
detach(package:topicmodels)

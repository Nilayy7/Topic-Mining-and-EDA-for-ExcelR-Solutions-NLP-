####################################################### Loading Required Libraries #########################################################

library(tidyr)
library(tm)
library(wordcloud)
library(RWeka)
library(textclean)
library(doSNOW)
library(dplyr)
library(topicmodels)

####################################################### LOading Given Data Sets and Seperating Visitors Chat ###############################

#chat_transcript <- read.csv("D:/Data Science Project/Conversation Mining/Final Folder/Deployment/chat_transcript.csv", col.names = "Chat")
chat_transcript$Chat <- iconv(chat_transcript$Chat, from = 'UTF-8', to = 'ASCII//TRANSLIT')

visitor<-data.frame(Visitors_Chat = grep(chat_transcript$Chat,pattern = "Visitor (.*)",value = T))

#library(tidyr)
Clean_Chat<- visitor %>% separate(Visitors_Chat, c("A", "B"),": ",fill = "left")

corpus <- Corpus(VectorSource(Clean_Chat$B))

stopwdrds <- readLines("https://raw.githubusercontent.com/niranjan2408/Training-Models-in-R/master/Text%20Mining/stop.txt")
pos.words <- readLines("https://raw.githubusercontent.com/niranjan2408/Training-Models-in-R/master/Text%20Mining/positive-words.txt")
neg.words <- readLines("https://raw.githubusercontent.com/niranjan2408/Training-Models-in-R/master/Text%20Mining/negative-words.txt")

corpus_clean = tm_map(corpus, tolower)
corpus_clean = tm_map(corpus_clean, removeWords,c(stopwords("english"),stopwdrds,"hi","what","can","i","ananya","mounica","mam","excelr","hyd","bye"))
corpus_clean = tm_map(corpus_clean, removePunctuation)
corpus_clean <- tm_map(corpus_clean,removeNumbers)
corpus_clean = tm_map(corpus_clean, stripWhitespace)
#inspect(corpus_clean[c(1:10)])

#term document metrix
tdm <- TermDocumentMatrix(corpus_clean)
#inspect(tdm)
# Term document matrix with inverse frequency 
tfidf <- TermDocumentMatrix(corpus_clean,control = list(weighting = function(p) weightTfIdf(p,normalize = F),stopwords=T,stemming=T))
#inspect(tfidf)

a0 <- NULL
cl <- makeCluster(4, type="SOCK") # for 4 cores machine
registerDoSNOW (cl)
# getting the indexes of documents having count of words = 0
for (i1 in 1:ncol(tdm))
{ if (sum(tdm[, i1]) == 0) {a0 = c(a0, i1)} }

# Removing empty docs 
tdm <- tdm[,-a0]

# Document term matrix 
dtm0 <- t(tdm)
#inspect(dtm0)
burnin <- 4000  
iter <- 2000    
thin <- 500
seed <-list(2003,5,63,100001,765)
nstart <- 5
best <- TRUE
k <- input$NumberOfTopics
ldaOut <-LDA(dtm0,k,method= "Gibbs", control=list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin))
ldaOut.topics <- as.matrix(topics(ldaOut))
ldaOut.terms <- as.matrix(terms(ldaOut,10))
ldaOut.terms
ap_topics <- tidy(ldaOut, matrix = "beta")
ap_top_terms <- ap_topics %>%group_by(topic) %>% top_n(10, beta) %>%ungroup()%>% arrange(topic, -beta)
#ap_top_terms %>% mutate(term = reorder(term, beta)) %>% ggplot(aes(term, beta,fill = factor(topic))) + geom_col(show.legend = FALSE) + facet_wrap(~topic, scale = "free") +coord_flip()+theme(axis.text = element_text(size = 15, color = "black"))


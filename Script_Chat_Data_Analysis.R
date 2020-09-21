####################################################### Loading Required Libraries #########################################################

library(tidyr)
library(tm)
library(wordcloud)
library(RWeka)
library(textclean)
library(doSNOW)
library(dplyr)

####################################################### LOading Given Data Sets and Seperating Visitors Chat ###############################


chat_transcript$Chat <- iconv(chat_transcript$Chat, from = 'UTF-8', to = 'ASCII//TRANSLIT')

visitor<-data.frame(Visitors_Chat = grep(chat_transcript$Chat,pattern = "Visitor (.*)",value = T))

#library(tidyr)
Clean_Chat<- visitor %>% separate(Visitors_Chat, c("A", "B"),": ",fill = "left")

corpus <- Corpus(VectorSource(Clean_Chat$B))

#stopwdrds = readLines("D:/Data Science Project/Conversation Mining/Final Folder/Deployment/stop.txt")
#pos.words=scan("D:/Data Science Project/Conversation Mining/Final Folder/Deployment/positive-words.txt", what="character", comment.char=";")	# read-in positive-words.txt
#neg.words=scan("D:/Data Science Project/Conversation Mining/Final Folder/Deployment/negative-words.txt", what="character", comment.char=";") 	# read-in negative-words.txt

stopwdrds <- readLines("https://raw.githubusercontent.com/niranjan2408/Training-Models-in-R/master/Text%20Mining/stop.txt")
pos.words <- readLines("https://raw.githubusercontent.com/niranjan2408/Training-Models-in-R/master/Text%20Mining/positive-words.txt")
neg.words <- readLines("https://raw.githubusercontent.com/niranjan2408/Training-Models-in-R/master/Text%20Mining/negative-words.txt")



#library(tm)
corpus_clean = tm_map(corpus, tolower)
corpus_clean = tm_map(corpus_clean, removeWords,c(stopwords("english"),stopwdrds,"hi","what","can","i","ananya","mounica","hey","mam","hyd"))
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
a1 <- NULL
cl <- makeCluster(4, type="SOCK") # for 4 cores machine
registerDoSNOW (cl)
# getting the indexes of documents having count of words = 0
for (i1 in 1:ncol(tdm))
{ if (sum(tdm[, i1]) == 0) {a0 = c(a0, i1)} }
for (i1 in 1:ncol(tfidf))
{ if (sum(tfidf[, i1]) == 0) {a1 = c(a1, i1)} }

# Removing empty docs 
tdm <- tdm[,-a0]


tfidf <- tfidf[,-a1]
#inspect(tfidf)

# Document term matrix 
dtm0 <- t(tdm)
#inspect(dtm0)
dtm1 <- t(tfidf)
#inspect(dtm1)
####################################################### Unigram ######################################################################

#library(wordcloud)

################################################## Creating Functions

# Making wordcloud function 
makewordc = function(x){	
  freq = sort(rowSums(as.matrix(x)),decreasing = TRUE)
  freq.df = data.frame(word=names(freq), freq=freq)
  wordcloud(freq.df$word[1:50], freq.df$freq[1:50],scale = c(4,.5),random.order = F, colors=1:10)
} 

words_bar_plot <- function(x){
  freq = sort(rowSums(as.matrix(x)),decreasing = TRUE)
  freq.df = data.frame(word=names(freq), freq=freq)
  head(freq.df, 20)
  library(ggplot2)
  ggplot(head(freq.df,input$NumberOfWords), aes(reorder(word,freq), freq)) +
    geom_bar(stat = "identity", col = rainbow(input$NumberOfWords)) + coord_flip() +
    xlab("Words") + ylab("Frequency") +theme(axis.text = element_text(size = 13, color = "black"))+
    ggtitle("Most frequent words")
}

# Making positive wordcloud function 
makeposwordc = function(x){
  freq = sort(rowSums(as.matrix(x)),decreasing = TRUE)
  # matching positive words
  pos.matches = match(names(freq), pos.words)
  pos.matches = !is.na(pos.matches)
  freq_pos <- freq[pos.matches]
  names <- names(freq_pos)
  wordcloud(names,freq_pos,scale=c(4,.5),colors = brewer.pal(8,"Dark2"))
}

# Making negatice wordcloud function
makenegwordc = function(x){	
  freq = sort(rowSums(as.matrix(x)),decreasing = TRUE)
  # matching negatice words
  neg.matches = match(names(freq), neg.words)
  neg.matches = !is.na(neg.matches)
  freq_neg <- freq[neg.matches]
  names <- names(freq_neg)
  wordcloud(names,freq_neg,scale=c(4,.5),colors = brewer.pal(8,"Dark2"))
}

pos_words_bar_plot <- function(x){
  pos.matches = match(colnames(x), pos.words)
  pos.matches = !is.na(pos.matches)
  pos_words_freq = as.data.frame(apply(x, 2, sum)[pos.matches])
  colnames(pos_words_freq)<-"freq"
  pos_words_freq["word"] <- rownames(pos_words_freq)
  # Sorting the words in deceasing order of their frequency
  pos_words_freq <- pos_words_freq[order(pos_words_freq$freq,decreasing=T),]
  ggplot(head(pos_words_freq,input$NumberOfWords), aes(reorder(word,freq), freq)) +
    geom_bar(stat = "identity", col = rainbow(input$NumberOfWords)) + coord_flip() +
    xlab("Positive Words") + ylab("Frequency") +theme(axis.text = element_text(size = 13, color = "green"))+
    ggtitle("Most frequent positive words")
}

neg_words_bar_plot <- function(x){
  neg.matches = match(colnames(x), neg.words)
  neg.matches = !is.na(neg.matches)
  neg_words_freq = as.data.frame(apply(x, 2, sum)[neg.matches])
  colnames(neg_words_freq)<-"freq"
  neg_words_freq["word"] <- rownames(neg_words_freq)
  # Sorting the words in deceasing order of their frequency
  neg_words_freq <- neg_words_freq[order(neg_words_freq$freq,decreasing=T),]
  ggplot(head(neg_words_freq,input$NumberOfWords), aes(reorder(word,freq), freq)) +
    geom_bar(stat = "identity", col = rainbow(input$NumberOfWords)) + coord_flip() +
    xlab("Negative Words") + ylab("Frequency") +theme(axis.text = element_text(size = 13, color = "red"))+
    ggtitle("Most frequent negative words")
}

tfidf
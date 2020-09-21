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
inspect(corpus_clean[c(1:10)])

#term document metrix
tdm <- TermDocumentMatrix(corpus_clean)
inspect(tdm)
# Term document matrix with inverse frequency 
tfidf <- TermDocumentMatrix(corpus_clean,control = list(weighting = function(p) weightTfIdf(p,normalize = F),stopwords=T,stemming=T))
inspect(tfidf)

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
inspect(tfidf)

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
  windows()
  wordcloud(freq.df$word[1:100], freq.df$freq[1:100],scale = c(4,.5),random.order = F, colors=1:10)
} 

words_bar_plot <- function(x){
  freq = sort(rowSums(as.matrix(x)),decreasing = TRUE)
  freq.df = data.frame(word=names(freq), freq=freq)
  head(freq.df, 20)
  library(ggplot2)
  windows()
  ggplot(head(freq.df,20), aes(reorder(word,freq), freq)) +
    geom_bar(stat = "identity", col = rainbow(20)) + coord_flip() +
    xlab("Words") + ylab("Frequency") +
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
  windows()
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
  windows()
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
  windows()
  ggplot(head(pos_words_freq,30), aes(reorder(word,freq), freq)) +
    geom_bar(stat = "identity") + coord_flip() +
    xlab("Positive words") + ylab("Frequency") +
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
  windows()
  ggplot(head(neg_words_freq,30), aes(reorder(word,freq), freq)) +
    geom_bar(stat = "identity") + coord_flip() +
    xlab("words") + ylab("Frequency") +
    ggtitle("Most frequent negative words")
}

# --- func to make cluster dendograms --- #
clusdend = function(a){	# writing func clusdend() 	
  mydata.df = as.data.frame(inspect(a));	
  mydata1.df = mydata.df[, order(-colSums(mydata.df))];
  min1 = min(ncol(mydata.df), 40) 	# minimum dimn of dist matrix
  test = matrix(0,min1,min1)
  test1 = test
  for(i1 in 1:(min1-1)){ 
    for(i2 in i1:min1){
      test = sum(mydata1.df[ ,i1]-mydata1.df[ ,i2])^2
      test1[i1,i2] = test; test1[i2, i1] = test1[i1, i2] 	}
  }
  # making dissimilarity matrix out of the freq one
  test2 = test1
  rownames(test2) = colnames(mydata1.df)[1:min1]
  # now plot collocation dendogram
  d <- dist(test2, method = "euclidean") # distance matrix
  fit <- hclust(d, method="complete")
  windows()
  plot(fit) # display dendogram
} # clusdend() func ends



################################################## Plotting Word clouds and Bar Plots
memory.limit(size=40000)
# Word cloud - TF - Uni gram
makewordc(tdm) %>% title(sub = "UNIGRAM - Wordcloud using TDM")

# Frequency Bar plot - TF - Unigram
words_bar_plot(tdm) 

# Word cloud - TFIDF - Uni gram
makewordc(tfidf) %>% title(sub = "UNIGRAM - Wordcloud using TFIDF")

# Frequency Bar plot - TFIDF - Unigram
words_bar_plot(tfidf)

# Positive word cloud - TF - Unigram
makeposwordc(tdm) %>% title(sub = "UNIGRAM - Positive Wordcloud using TDM")
# Frequency Barplot - Positive words - Unigram
pos_words_bar_plot(dtm0)

# Positive word cloud - Unigram - TFIDF
makeposwordc(tfidf) %>% title(sub = "UNIGRAM - Positive Wordcloud using TFIDF")
# Frequency Barplot - Positive words - TFIDF - Unigram
pos_words_bar_plot(dtm1)

# Negative word cloud - TF - unigam
makenegwordc(tdm) %>% title(sub = "UNIGRAM - NEGATIVE Wordcloud using TDM")
# Frequency Barplot -negative words - Unigram - TF
neg_words_bar_plot(dtm0) 

# Negative word cloud - TFIDF - unigam
makenegwordc(tfidf) %>% title(sub = "UNIGRAM - NEGATIVE Wordcloud using TFIDF")
# Frequency Barplot -negative words - Unigram - TFIDF
neg_words_bar_plot(dtm1) 

# --- cluster dendograms --- #
clusdend(dtm1)

####################################################### Bigram ######################################################################
#library(RWeka)

minfreq_bigram<-2
token_delim <- " \\t\\r\\n.!?,;\"()"
bitoken <- NGramTokenizer(corpus_clean, Weka_control(min=2,max=2, delimiters = token_delim))
two_word <- data.frame(table(bitoken))
sort_two <- two_word[order(two_word$Freq,decreasing=TRUE),]
windows()
wordcloud(sort_two$bitoken,sort_two$Freq,random.order=FALSE,scale = c(4,1),colors = brewer.pal(8,"Dark2"),max.words=50)

windows()
sort_two %>% slice(1:20) %>% 
  ggplot() + geom_bar(aes(x= reorder(bitoken,Freq), y = Freq), stat = "identity", fill = "#de5833") +
  theme_minimal()  +  coord_flip() +labs(x = NULL)

####################################################### Loading Required Libraries #########################################################

library(tidyr)
library(tm)
library(wordcloud)
library(RWeka)
library(textclean)
library(doSNOW)
library(dplyr)
library(tidytext)
library(digest)
library(textmineR)
library(ggplot2)

####################################################### LOading Given Data Sets and Seperating Visitors Chat ###############################

#chat_transcript <- read.csv("D:/Data Science Project/Conversation Mining/chat_transcript.csv", col.names = "Chat")

chat_transcript$Chat <- iconv(chat_transcript$Chat, from = 'UTF-8', to = 'ASCII//TRANSLIT')
visitor<-data.frame(Visitors_Chat = grep(chat_transcript$Chat,pattern = "Visitor (.*)",value = T))
Clean_Chat<- visitor %>% separate(Visitors_Chat, c("A", "B"),": ",fill = "left")

corpus <- Corpus(VectorSource(Clean_Chat$B))

stop_wdrds <- readLines("https://raw.githubusercontent.com/niranjan2408/Training-Models-in-R/master/Text%20Mining/stop.txt")

corpus_clean = tm_map(corpus, tolower)
corpus_clean = tm_map(corpus_clean, removeWords,c(stopwords("english"),stop_wdrds,"hi","hii","what","can","i","ananya","mounica","hey","mam","hyd"))
corpus_clean = tm_map(corpus_clean, removePunctuation)
corpus_clean <- tm_map(corpus_clean,removeNumbers)
corpus_clean = tm_map(corpus_clean, stripWhitespace)
corpus_clean = tm_map(corpus_clean, stemDocument)

#inspect(corpus_clean[c(1:10)])

data <- data.frame(text = sapply(corpus_clean, as.character), stringsAsFactors = FALSE)


data$text[data$text==""]<-NA
data <- drop_na(data)
data["id"] <- c(1:nrow(data))

text_cleaning_tokens <- data %>%   unnest_tokens(word, text)
text_cleaning_tokens$word <- gsub('[[:digit:]]+', '', text_cleaning_tokens$word)
text_cleaning_tokens$word <- gsub('[[:punct:]]+', '', text_cleaning_tokens$word)
colnames(stop_words) <- "stop_words"
text_cleaning_tokens <- text_cleaning_tokens %>% filter(!(nchar(word) == 1))
text_cleaning_tokens <- anti_join(text_cleaning_tokens,stop_words,by=c("word"="stop_words"))
tokens <- text_cleaning_tokens %>% filter(!(word=="")) 
tokens <- tokens %>% mutate(ind = row_number())
tokens <- tokens %>% group_by(id) %>% mutate(ind = row_number()) %>%  spread(key = ind, value = word)
tokens [is.na(tokens)] <- ""
tokens <- unite(tokens, text,-id,sep =" " )
tokens$text <- trimws(tokens$text)
dtm <- CreateDtm(tokens$text,doc_names = tokens$id,ngram_window = c(1, 2))
tf <- TermDocFreq(dtm = dtm)
original_tf <- tf %>% select(term, term_freq,doc_freq)
rownames(original_tf) <- 1:nrow(original_tf)
vocabulary <- tf$term[ tf$term_freq > 1 & tf$doc_freq < nrow(dtm) / 2 ] # Eliminate words appearing less than 2 times or in more than half of the documents
dtm = dtm

k_list <- seq(1, 20, by = 1)
model_dir <- paste0("models_", digest(vocabulary, algo = "sha1"))
if (!dir.exists(model_dir)) dir.create(model_dir)
model_list <- TmParallelApply(X = k_list, FUN = function(k){
  filename = file.path(model_dir, paste0(k, "_topics.rda"))
  
  if (!file.exists(filename)) {
    m <- FitLdaModel(dtm = dtm, k = k, iterations = 100)
    m$k <- k
    m$coherence <- CalcProbCoherence(phi = m$phi, dtm = dtm, M = 5)
    save(m, file = filename)
  } else {
    load(filename)
  }
  
  m
}, export=c("dtm", "model_dir")) 
# export only needed for Windows machines model tuning choosing the best model

coherence_mat <- data.frame(k = sapply(model_list, function(x) nrow(x$phi)),coherence = sapply(model_list, function(x) mean(x$coherence)),stringsAsFactors = FALSE)

#ggplot(coherence_mat, aes(x = k, y = coherence)) +  geom_point() +  geom_line(group = 1)+  ggtitle("Best Topic by Coherence Score") + theme_minimal() +  scale_x_continuous(breaks = seq(1,20,1)) + ylab("Coherence")

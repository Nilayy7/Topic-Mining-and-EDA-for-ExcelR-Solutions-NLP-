####################################################### Defining Function For LDA Topic Modeling #################################

top_terms_by_topic_LDA <- function(input_text, # should be a columm from a dataframe
                                   plot = T, # return a plot? TRUE by defult
                                   number_of_topics = 4) # number of topics (4 by default)
{    
  # create a corpus (type of object expected by tm) and document term matrix
  Corpus <- Corpus(VectorSource(input_text)) # make a corpus object
  DTM <- DocumentTermMatrix(Corpus) # get the count of words/document
  
  # remove any empty rows in our document term matrix (if there are any 
  # we'll get an error when we try to run our LDA)
  unique_indexes <- unique(DTM$i) # get the index of each unique value
  DTM <- DTM[unique_indexes,] # get a subset of only those indexes
  
  # preform LDA & get the words/topic in a tidy text format
  lda <- LDA(DTM, k = number_of_topics, control = list(seed = 1234))
  topics <- tidy(lda, matrix = "beta")
  
  # get the top ten terms for each topic
  top_terms <- topics  %>% # take the topics data frame and..
    group_by(topic) %>% # treat each topic as a different group
    top_n(10, beta) %>% # get the top 10 most informative words
    ungroup() %>% # ungroup
    arrange(topic, -beta) # arrange words in descending informativeness
  
  # if the user asks for a plot (TRUE by default)
  if(plot == T){
    # plot the top ten terms for each topic in order
    top_terms %>% # take the top terms
      mutate(term = reorder(term, beta)) %>% # sort terms by beta value 
      ggplot(aes(term, beta, fill = factor(topic))) + # plot beta by theme
      geom_col(show.legend = FALSE) + # as a bar plot
      facet_wrap(~ topic, scales = "free") + # which each topic in a seperate plot
      labs(x = NULL, y = "Beta") + # no x label, change y label 
      coord_flip() # turn bars sideways
  }else{ 
    # if the user does not request a plot
    # return a list of sorted terms instead
    return(top_terms)
  }
}


####################################################### Loading Required Libraries #########################################################

library(tm)
library(textclean)
library(foreach)
library(doSNOW)
library(tidytext)
library(topicmodels)
library(SnowballC) 

####################################################### LOading Given Data Sets and Seperating Visitors Chat ###############################


#library(foreach)
#library(doSNOW)
cl <- makeCluster(4, type="SOCK") # for 4 cores machine
registerDoSNOW (cl)

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

DTM <- DocumentTermMatrix(corpus_clean)
DTM_tidy <- tidy(DTM)

custom_stop_words <- tibble(word = c("ananya","mounica","patel","visitor"))

DTM_tidy_cleaned <- DTM_tidy %>% anti_join(stop_words, by = c("term" = "word")) %>% anti_join(custom_stop_words, by = c("term" = "word"))

cleaned_documents <- DTM_tidy_cleaned %>%  group_by(document) %>% mutate(terms = toString(rep(term, count))) %>% select(document, terms) %>%  unique()

top_terms_by_topic_LDA(cleaned_documents$terms, number_of_topics = 4)

# Stemming the words
DTM_tidy_cleaned <- DTM_tidy_cleaned %>%   mutate(stem = wordStem(term)) 

# reconstructing the documents
cleaned_documents <- DTM_tidy_cleaned %>%  group_by(document) %>%   mutate(terms = toString(rep(stem, count))) %>% select(document, terms) %>% unique()

top_terms_by_topic_LDA(cleaned_documents$terms, number_of_topics = 4)

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

####################################################### Loading Chat Transcript Derived From Given Data ##########################

chat_transcript <- read.csv("D:/Data Science Project/Conversation Mining/chat_transcript.csv", col.names = "V1")

chat_transcript$V1 <- iconv(chat_transcript$V1, from = 'UTF-8', to = 'ASCII//TRANSLIT')

chat_transcript <- data.frame(V1 = chat_transcript[!grepl("================================================================================", chat_transcript$V1),]) # Chat seperator
chat_transcript <-  data.frame(V1 = chat_transcript[!grepl("Thank you for connecting with ExcelR!", chat_transcript$V1),]) # opening statement of Mounica
chat_transcript <-  data.frame(V1 = chat_transcript[!grepl("Hello, how may I be of assistance to you?", chat_transcript$V1),]) # Opening Statement of Ananya

top_terms_by_topic_LDA(cleaned_documents$terms, number_of_topics = 4)

Corpus <- Corpus(VectorSource(chat_transcript$V1)) # Creating Corpus of text column
Corpus <-  tm_map(Corpus, removePunctuation) # Removing Punctuation
Corpus <- tm_map(Corpus,removeNumbers) # Removing Numbers
Corpus <-  tm_map(Corpus, stripWhitespace) # Removing Whitespaces

DTM <- DocumentTermMatrix(Corpus)
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

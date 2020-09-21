####################################################### Loading Required Libraries #########################################################

library(tidyr)
library(tm)
library(wordcloud)
library(RWeka)
library(textclean)
library(doSNOW)
library(dplyr)
library(ggplot2)
library(tidytext)
library(igraph)
library(ggraph)
library(textdata)
library(stringr)
library(widyr)

####################################################### LOading Given Data Sets and Seperating Visitors Chat ###############################

setwd("D:/Data Science Project/Conversation Mining/Chat Transcripts for Project")

chat_transcript <- read.csv("D:/Data Science Project/Conversation Mining/chat_transcript.csv", col.names = "Chat")

chat_transcript$Chat <- iconv(chat_transcript$Chat, from = 'UTF-8', to = 'ASCII//TRANSLIT')

visitor<-data.frame(Visitors_Chat = grep(chat_transcript$Chat,pattern = "Visitor (.*)",value = T))

#library(tidyr)
Clean_Chat<- visitor %>% separate(Visitors_Chat, c("A", "B"),": ",fill = "left")


####################################################### Creating Corpus ####################################################################

corpus <- Corpus(VectorSource(Clean_Chat$B))

#library(tm)
corpus_clean = tm_map(corpus, tolower)
corpus_clean = tm_map(corpus_clean, removeWords,c("hi","what","can","i","ananya","mounica"))
corpus_clean = tm_map(corpus_clean, removePunctuation)
corpus_clean <- tm_map(corpus_clean,removeNumbers)
corpus_clean = tm_map(corpus_clean, stripWhitespace)
#corpus_clean <- tm_map(corpus_clean, stemDocument)
inspect(corpus_clean[c(1:10)])

corpus_clean <- as.list(corpus_clean)
bigrams <- data.frame(text = sapply(corpus_clean, as.character), stringsAsFactors = FALSE) %>%  unnest_tokens(bigram, text, token = "ngrams", n = 2)
bigrams %>%  count(bigram, sort = TRUE)
bigrams_separated <- bigrams %>% separate(bigram, c("word1", "word2"), sep = " ")
bigrams_filtered <- bigrams_separated %>%  filter(!word1 %in% stop_words$word) %>%  filter(!word2 %in% stop_words$word)
bigram_counts <- bigrams_filtered %>%   count(word1, word2, sort = TRUE)
bigrams_united <- bigrams_filtered %>%  unite(bigram, word1, word2, sep = " ")
bigrams_separated %>%  filter(word1 == "not") %>%  count(word1, word2, sort = TRUE)
AFINN <- get_sentiments("afinn")
not_words <- bigrams_separated %>%  filter(word1 == "not") %>%  inner_join(AFINN, by = c(word2 = "word")) %>%  count(word2, value, sort = TRUE)

not_words %>%
  mutate(contribution = n * value) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(word2, n * value, fill = n * value > 0)) +
  geom_col(show.legend = FALSE) +
  xlab("Words preceded by \"not\"") +
  ylab("Sentiment value * number of occurrences") +
  coord_flip()


negation_words <- c("not", "no", "never", "without")

negated_words <- bigrams_separated %>%  filter(word1 %in% negation_words) %>%  inner_join(AFINN, by = c(word2 = "word")) %>%  count(word1, word2, value, sort = TRUE)


negated_words %>%  group_by(word1) %>%  ungroup() %>%  mutate(word2 = reorder(word2, n)) %>%  
  ggplot(aes(word2, n, fill = word1)) + geom_col(show.legend = FALSE) +  facet_wrap(~word1, scales = "free_y") + labs(y = "Contribution to word1",x = NULL) +
  coord_flip()

bigram_graph <- bigram_counts %>%  filter(n > 20) %>%  graph_from_data_frame()

set.seed(2017)
ggraph(bigram_graph, layout = "fr") +  geom_edge_link() +  geom_node_point() +  geom_node_text(aes(label = name), vjust = 1, hjust = 1)


set.seed(2016)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()


count_bigrams <- function(dataset) {
  dataset %>%
    unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% stop_words$word,
           !word2 %in% stop_words$word) %>%
    count(word1, word2, sort = TRUE)
}

visualize_bigrams <- function(bigrams) {
  set.seed(2016)
  a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
  
  bigrams %>%
    graph_from_data_frame() %>%
    ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = a) +
    geom_node_point(color = "lightblue", size = 5) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
    theme_void()
}



chat_bigrams <- data.frame(text = sapply(corpus_clean, as.character), stringsAsFactors = FALSE) %>%  count_bigrams()

# filter out rare combinations, as well as digits
chat_bigrams %>%  filter(n > 40, !str_detect(word1, "\\d"), !str_detect(word2, "\\d")) %>%  visualize_bigrams()


##Counting and correlating among sections

section_words <- data.frame(text = sapply(corpus_clean, as.character), stringsAsFactors = FALSE) %>%
  mutate(section = row_number() %/% 10) %>%
  filter(section > 0) %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word)


# count words co-occuring within sections
word_pairs <- section_words %>%  pairwise_count(word, section, sort = TRUE)
word_pairs %>%  filter(item1 == "data")
word_pairs %>%  filter(item1 == "location")
word_pairs %>%  filter(item1 == "training")

#Pairwise correlation
word_cors <- section_words %>%  group_by(word) %>%  filter(n() >= 20) %>%  pairwise_cor(word, section, sort = TRUE)
word_cors

word_cors %>%  filter(item1 == "data")

word_cors %>%
  filter(item1 %in% c("data", "pmp", "location", "details")) %>%
  group_by(item1) %>%  top_n(6) %>%  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ item1, scales = "free") +
  coord_flip()

set.seed(2016)

word_cors %>%
  filter(correlation > .15) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()

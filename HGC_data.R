
### Install all the packages

library(readr)
library(dplyr)
library(tidytext)
library(igraph)
library(tm)
library(SnowballC)
library(wordcloud)
library(stringr)

mydata<- read_csv("C:/Users/Shuting/Desktop/Fall 2017/BIA672_MarketingAnalysis/Team Project/RawData/HGCdata.csv")

View(mydata)

## Clean the review
# functions to remove symbles, special characters, and code
review <- mydata$Review

Removespecialchara <- function(review) {
  gsub('[^0-9A-Za-z///]', ' ',review)
}

Removecode <- function(review) {
  gsub('U613CU3E30', ' ',review)
}

Cleanreview <- function(review) {
  s1 <- Removespecialchara(review)
  s2 <- Removecode(s1)
  s2
}

# put cleaned reviews into a data frame
reviews <- as.vector(sapply(review, Cleanreview))
reviews[[1]]

# put the reviews into corpus for later cleanning
mydata.corpus <- Corpus(VectorSource(reviews))
inspect(mydata.corpus[1])

# tm_map cleaning 
stopwords_1 <- c(stopwords('english'), "duh", "whatever","<U+00A0>",'na', 'and','the','our','that','for','are','also','more','has','must','have','should','this','with', 't', 's','ve', 'can', 'm')

mydata.corpus <- tm_map(mydata.corpus, tolower) %>%
                  tm_map(removePunctuation) %>% 
                    tm_map(removeWords, stopwords_1) %>% 
                      tm_map(stripWhitespace)
                    

mydata.corpus[[1]]$content


# convert reviews into Document-Term Matrix: 
# documents as the rows, terms/words as the columns, frequency of the term in the document as the entries. 
# You can use bounds in control to remove rare and frequent terms

# reduce the dimension of the dataset
 
review_dtm = DocumentTermMatrix(mydata.corpus, control = list(bounds = list(global = c(1, Inf)))) 
inspect(review_dtm[1:10, sample(ncol(review_dtm), 10)])
findFreqTerms(review_dtm, 1000)
freq = as.data.frame(sort(colSums(as.matrix(review_dtm)), decreasing=TRUE)) 

#top words
freqwords <- freq %>% 
              mutate(word=rownames(freq)) %>% 
                rename(count="sort(colSums(as.matrix(review_dtm)), decreasing = TRUE)") %>% 
                  select(word,count) #  %>% top_n(50)

# plot word cloud
library(RColorBrewer)  
wordcloud(freqwords[,1], freqwords[,2], max.words=100, random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(4, "Dark2"))

# save words in csv 
# write.csv(freqwords,file = "C:/Users/Shuting/Desktop/Fall 2017/BIA672_MarketingAnalysis/Team Project/RawData/freq50data.csv")





##################################################################################################
# sentiment analysis
##################################################################################################

# Precompiled list of words with positive and negative meanings
# Source: http://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html
setwd("C:/Users/Shuting/Desktop/Fall 2017/BIA 658 Social Network/Lecture/sentiment_analysis")
neg_words = read.table("data/negative-words.txt", header = F, stringsAsFactors = F)[, 1]
pos_words = read.table("data/positive-words.txt", header = F, stringsAsFactors = F)[, 1]

neg.word <- as.data.frame(neg_words) 
pos.word <- as.data.frame(pos_words) 


# review matrix with all words as columns
reviews <- data_frame(name='HGC',text = get("content", mydata.corpus))
review.matrix <- cbind(reviews$text,as.matrix(review_dtm))
review.matrix <- as.data.frame(review.matrix) %>% rename(text=V1)

# summary table: each review contain how many neg words and pos words
reviews.summary <- reviews %>%
                      select(text) %>% 
                        mutate(rating=mydata$Rating)

reviews.summary$neg <- tm_term_score(review_dtm, neg_words)
reviews.summary$pos <- tm_term_score(review_dtm, pos_words)
reviews.summary$content = NULL

# seperate the words into single word and 2 words
review_unigrams <- reviews %>% unnest_tokens(word, text)
review_bigrams <- reviews %>% 
                    unnest_tokens(ngram, text, token="ngrams", n=2) %>% 
                      rename(word=ngram) 

# no space between 2 words
freqbiword <- str_replace_all(review_bigrams$word," ","_") %>% 
                as.data.frame() %>% 
                  rename(word='.') %>% 
                  group_by(word) %>%
                    tally() %>% 
                       rename(count=n) %>%
                          arrange(-count)  %>% as.data.frame() 

# 3 words together, get rid of space
review_trigrams <- reviews %>% 
  unnest_tokens(ngram, text, token="ngrams", n=3) %>% 
  rename(word=ngram)

freqtriword <- str_replace_all(review_trigrams$word," ","_") %>% 
                 as.data.frame() %>% 
                  rename(word='.') %>%  
                   group_by(word) %>%
                     tally() %>% 
                      rename(count=n) %>%
                        arrange(-count) %>% as.data.frame() 

# put all words together
#all.word<- rbind(freqwords,freqbiword,freqtriword)

# Draw plot
top20.biwords <- freqbiword %>% top_n(20) 
top20 <- melt(top20.biwords, id.vars = c("word", "count")) 

ggplot(top20, aes(x=reorder(word,count), y=count)) + 
  geom_bar(stat="identity", width=.5, fill="tomato3") + coord_flip() + labs(y='count',x='word')+
  labs(title="most frequent 2 words")
  
 
  
top10.triwords <- freqtriword %>% top_n(10) 
top10.triwords <- melt(top10.triwords, id.vars = c("word", "count")) 

ggplot(top10.triwords, aes(x=reorder(word,count), y=count)) + 
  geom_bar(stat="identity", width=.5, fill="blue") + coord_flip() + labs(y='count',x='word')+
  labs(title="most frequent 3 words")+theme_bw()



### lexicon dictionary words from tidytext package
#  include positive, negative, anger, anticipation, disgust, fear, joy, sadness, surprise, and trust.

nrc <- sentiments %>%
         filter(lexicon == "nrc") %>%
           select(word, sentiment)


### Plot top 10 sentiment words in each category
library("ggplot2") 

# only select positive/negative/joy/sadness words
nrc_word_counts <- review_unigrams %>%
                    inner_join(get_sentiments("nrc") %>%  
                      filter(sentiment == c("positive", "negative","joy","sadness"))) %>%
                        count(word, sentiment, sort = TRUE) %>%
                           ungroup() 


nrc_word_counts %>%
      group_by(sentiment) %>%
         top_n(15) %>%
          ungroup() %>%
           mutate(word = reorder(word, n)) %>%
             ggplot(aes(word, n, fill = sentiment)) + geom_col(show.legend = FALSE) +
               facet_wrap(~sentiment, scales = "free_y") + labs(y = "Contribution to sentiment",
       x = NULL) +coord_flip()



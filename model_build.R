library(tm)
library(qdap)
library(ngram) 
library(dplyr)
library(stringr)


my_datacorpus <- function(data, rlines){
        lines <- length(data)
        if(rlines < lines){data <- data[sample(lines,rlines)]}
        datas<-  concatenate(data)
        datacorpus <- Corpus(VectorSource(datas))
        datacorpus <- tm_map(datacorpus, tolower)
        datacorpus <- tm_map(datacorpus, removePunctuation, preserve_intra_word_dashes = TRUE)
        datacorpus <- tm_map(datacorpus, removeNumbers)
##        datacorpus <- tm_map(datacorpus, removeWords, stopwords(("english")))
        datacorpus <- tm_map(datacorpus, stripWhitespace)
        return(datacorpus)
}

my_models <- function(datacorpus, save = "N"){
        ## takes corpus and creates ngrams. returns list of ngrams  (smallest ngram first!)  
        ng4 <- get.phrasetable(ngram(datacorpus$content, n=4))
        ng4 <- subset(ng4, freq > 1)
        ng4 <- rename(ng4, words = ngrams)
        
        ng3 <- get.phrasetable(ngram(datacorpus$content, n=3))
        ng3 <- subset(ng3, freq > 1)
        ng3 <- rename(ng3, words = ngrams)
        
        ng2 <- get.phrasetable(ngram(datacorpus$content, n=2))
        ng2 <- subset(ng2, freq > 1)
        ng2 <- rename(ng2, words = ngrams)
        
        models <- list(ng2, ng3, ng4)
        if(save == "Y"){save(models, file = 'models_new.rda')}
        return(models)
}


set.seed(1406)

## read data
twitter <- readLines('en_US/en_US.twitter.txt', skipNul = TRUE)
blogs <- readLines('en_US/en_US.blogs.txt', skipNul = TRUE)
news <- readLines('en_US/en_US.news.txt', skipNul = TRUE)
tbl <- c(twitter, blogs, news)
rlines <- 80000

## create corpus
my_corpus <- my_datacorpus(tbl, rlines)

## create model
my_model <- my_models(my_corpus, "Y")


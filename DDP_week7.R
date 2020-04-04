# Data Science Capstone - Week 7

# Loading libraries needed
library(knitr)
library(stringi)
library(NLP)
library(tm)
library(RWeka)
library(data.table)

#### Preparing Directories
workingDir <- getwd()
dataDir <-file.path(workingDir, "../data/")

#### Checking files in directories
dir(path = dataDir)

# Exploratory Data Analysis

# Loading files
blogs_lines <- readLines(paste0(dataDir, "en_US.blogs.txt"),
                         encoding = "UTF-8", skipNul = TRUE)
news_lines <- readLines(paste0(dataDir, "en_US.news.txt"),
                        encoding = "UTF-8", skipNul= TRUE)
twitter_lines <- readLines(paste0(dataDir, "en_US.twitter.txt"),
                           encoding = "UTF-8", skipNul = TRUE)

# Sampling data with a 3% of each file
set.seed(12345)

s_blogs <- blogs_lines[sample(1:length(blogs_lines),
                              0.03*length(blogs_lines), replace=FALSE)]

s_news <- news_lines[sample(1:length(news_lines),
                            0.03*length(news_lines), replace=FALSE)]

s_twitter <- twitter_lines[sample(1:length(twitter_lines),
                                  0.03*length(twitter_lines), replace=FALSE)]

# Cleaning sample data removing unconvention characters
s_blogs <- iconv(s_blogs, "UTF-8", "ASCII", sub="")
s_news <- iconv(s_news, "UTF-8", "ASCII", sub="")
s_twitter <- iconv(s_twitter, "UTF-8", "ASCII", sub="")

# Joining 3 samples in 1 dataset
s_data <- c(s_blogs, s_news, s_twitter)

# Building corpus
s_corpus <- VCorpus(VectorSource(s_data))

# Cleaning corpus (to lower case, removing numbers, punctuations, etc)

s_corpus <- tm_map(s_corpus, tolower)
s_corpus <- tm_map(s_corpus, removeNumbers)
s_corpus <- tm_map(s_corpus, removePunctuation)
s_corpus <- tm_map(s_corpus, stripWhitespace)
s_corpus <- tm_map(s_corpus, PlainTextDocument)

# Building Term Document Matrix and tokenized using NGramTokenizer to four different categories: unigram, bigram trigram and tetragram

uni_tokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))
bi_tokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
tri_tokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
tetra_tokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))

# Build Term Document matrices with these four options
uni_matrix <- TermDocumentMatrix(s_corpus, control = list(tokenize = uni_tokenizer))
bi_matrix <- TermDocumentMatrix(s_corpus, control = list(tokenize = bi_tokenizer))
tri_matrix <- TermDocumentMatrix(s_corpus, control = list(tokenize = tri_tokenizer))
tetra_matrix <- TermDocumentMatrix(s_corpus, control = list(tokenize = tetra_tokenizer))

#Calculate frequency of n-grams
uni_corpus <- findFreqTerms(uni_matrix,lowfreq = 50)
bi_corpus <- findFreqTerms(bi_matrix,lowfreq=50)
tri_corpus <- findFreqTerms(tri_matrix,lowfreq=50)
tetra_corpus <- findFreqTerms(tetra_matrix,lowfreq=50)

uni_corpus_freq <- sort(rowSums(as.matrix(uni_matrix[uni_corpus,])), decreasing = TRUE)
uni_corpus_freq <- data.table(word=names(uni_corpus_freq), frequency=uni_corpus_freq)
bi_corpus_freq <- sort(rowSums(as.matrix(bi_matrix[bi_corpus,])), decreasing = TRUE)
bi_corpus_freq <- data.table(word=names(bi_corpus_freq), frequency=bi_corpus_freq)
tri_corpus_freq <- sort(rowSums(as.matrix(tri_matrix[tri_corpus,])), decreasing = TRUE)
tri_corpus_freq <- data.table(word=names(tri_corpus_freq), frequency=tri_corpus_freq)
tetra_corpus_freq <- sort(rowSums(as.matrix(tetra_matrix[tetra_corpus,])), decreasing = TRUE)
tetra_corpus_freq <- data.table(word=names(tetra_corpus_freq), frequency=tetra_corpus_freq)


save(uni_corpus_freq, file = "uni_gram.rds")
save(bi_corpus_freq , file = "bi_gram.rds")
save(tri_corpus_freq , file = "tri_gram.rds")
save(tetra_corpus_freq , file = "tetra_gram.rds")

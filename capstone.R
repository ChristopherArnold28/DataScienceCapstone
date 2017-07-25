## Store filenames and paths as varriables
twitterFile <- c("~/Data Science Capstone/en_US.twitter.txt")
blogFile <- c("~/Data Science Capstone/en_US.blogs.txt")
newsFile <- c("~/Data Science Capstone/en_US.news.txt")

set.seed(1234)

require(tm)
require(stringi)
require(slam)
require(quanteda)

require(RWeka)
require(tau)
require(openNLP)
require(ggplot2)
require(filehash)
require(scales)

#open connection to twitter file
twitter.connection <- file(twitterFile, open = "rb")
#read all the lines and return each line as a vector of characters
twitter.tweets <- readLines(twitter.connection, encoding = "UTF-8", skipNul = TRUE)
twitter.words <- stri_count_words(twitter.tweets)
twitter.wordcount <- sum(twitter.words)
twitter.meanwords <- mean(twitter.words) 
#want to get number of tweets
twitter.tweets.num <- length(twitter.tweets)
twitter.sample <- sample(twitter.tweets, size = twitter.tweets.num * .1, replace = FALSE)
twitter.sample.words <- stri_count_words(twitter.sample)
close(twitter.connection)


blog.connection <- file(blogFile, open ="rb")
blog.bloglines <- readLines(blog.connection, encoding = "UTF-8", skipNul = TRUE)
blog.words <- stri_count_words(blog.bloglines)
blog.wordcount <- sum(blog.words)
blog.meanwords <- mean(blog.words)
blog.bloglines.num <- length(blog.bloglines)
blog.sample <- sample(blog.bloglines, size = blog.bloglines.num * .1, replace = FALSE)
blog.sample.words <- stri_count_words(blog.sample)
close(blog.connection)

news.connection <- file(newsFile, open = "rb")
news.lines <- readLines(news.connection, encoding = "UTF-8", skipNul = TRUE)
news.words <- stri_count_words(news.lines)
news.wordcount <- sum(news.words)
news.meanwords <- mean(news.words)
news.lines.num <- length(news.lines)
news.sample <- sample(news.lines, size = news.lines.num * .1, replace = FALSE)
news.sample.words <- stri_count_words(news.sample)
close(news.connection)

#histograms of word counts
par(mfrow = c(1,3))
hist(twitter.sample.words, col = "blue")
hist(blog.sample.words, col = "green")
hist(news.sample.words, col = "red")

source <- c("Twitter", "Blogs", "News")
linecount <- c(twitter.tweets.num, blog.bloglines.num, news.lines.num)
wordcounts <- c(twitter.wordcount, blog.wordcount, news.wordcount)
meanwords <- c(twitter.meanwords, blog.meanwords, news.meanwords)
sumframe <- data.frame(source, linecount, wordcounts, meanwords)
colnames(sumframe) <- c("Source", "Number of Lines", "Number of Words", "Mean Words Per Line")


combinedData <- c(twitter.sample, blog.sample, news.sample)

corpus <- VCorpus(VectorSource(combinedData))
#corpus <- tm::Corpus(VectorSource(combinedData))

decimal.clean <- function(x) {gsub("([0-9]*)\\.([0-9]+)","\\1 \\2", x)}
hashtag.clean <- function(x) {gsub("#[a-zA-z0-9]+", " ", x)}
nonenglish.clean <- function(x) {gsub("\\W+", " ", x)}

corpus <- tm_map(corpus, decimal.clean)
corpus <- tm_map(corpus, hashtag.clean)
corpus <- tm_map(corpus, nonenglish.clean)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, tolower)
corpus2 <- corpus
corpus <- tm_map(corpus, removeWords, stopwords("english"))
weirdsletters <- c("b", "c", "d", "e", "f", "g", "h", "j", "k", "l", "m", "n", "p", "q", "r", "s", "t", "u", "v","w", "x", "y", "z", "www", "rt", "ll", "re", "ve", "ll", "th", "st", "pm", "lol")
corpus <- tm_map(corpus, removeWords, weirdsletters)
corpus2 <- tm_map(corpus2, removeWords, weirdsletters)
swearWords <- read.csv("~/Data Science Capstone/SwearWords.csv")
corpus <- tm_map(corpus, removeWords, swearWords)
corpus2 <- tm_map(corpus2, removeWords, swearWords)
#built the corpus now gonna get the ngrams

corpus <- tm_map(corpus, PlainTextDocument)
corpus2 <- tm_map(corpus2, PlainTextDocument)

NGramTokenizer1 <- function(x) unlist(lapply(NLP::ngrams(words(x), 1), paste,        collapse=" "), use.names=FALSE)

NGramTokenizer2 <- function(x) unlist(lapply(NLP::ngrams(words(x), 2), paste,        collapse=" "), use.names=FALSE)

NGramTokenizer3 <- function(x) unlist(lapply(NLP::ngrams(words(x), 3), paste,        collapse=" "), use.names=FALSE)

NGramTokenizer4 <- function(x) unlist(lapply(NLP::ngrams(words(x), 4), paste,        collapse=" "), use.names=FALSE)
NGramTokenizer5 <- function(x) unlist(lapply(NLP::ngrams(words(x), 5), paste,        collapse=" "), use.names=FALSE)
NGramTokenizer6 <- function(x) unlist(lapply(NLP::ngrams(words(x), 6), paste,        collapse=" "), use.names=FALSE)
NGramTokenizer7 <- function(x) unlist(lapply(NLP::ngrams(words(x), 7), paste,        collapse=" "), use.names=FALSE)

ng1 <- TermDocumentMatrix(corpus, control=list(tokenize=NGramTokenizer1))
ng2 <- TermDocumentMatrix(corpus, control=list(tokenize=NGramTokenizer2))
ng3 <- TermDocumentMatrix(corpus, control=list(tokenize=NGramTokenizer3))
ng4 <- TermDocumentMatrix(corpus, control=list(tokenize=NGramTokenizer4))
ng5 <- TermDocumentMatrix(corpus, control=list(tokenize=NGramTokenizer5))
ng6 <- TermDocumentMatrix(corpus, control=list(tokenize=NGramTokenizer6))


ng1sw <- TermDocumentMatrix(corpus2, control = list(tokenize = NGramTokenizer1))
ng2sw <- TermDocumentMatrix(corpus2, control = list(tokenize = NGramTokenizer2))
ng3sw <- TermDocumentMatrix(corpus2, control = list(tokenize = NGramTokenizer3))
ng4sw <- TermDocumentMatrix(corpus2, control = list(tokenize = NGramTokenizer4))
ng5sw <- TermDocumentMatrix(corpus2, control = list(tokenize = NGramTokenizer5))
ng6sw <- TermDocumentMatrix(corpus2, control = list(tokenize = NGramTokenizer6))
ng7sw <- TermDocumentMatrix(corpus2, control = list(tokenize = NGramTokenizer7))

w1 <- findFreqTerms(ng1, lowfreq = 350)
wf1 <- rowSums(as.matrix(ng1[w1,]))
wordGram1 <- data.frame(unigram = names(wf1), frequency = wf1, row.names = NULL)
save(wordGram1,file =  "WordGram1.RData")

w2 <- findFreqTerms(ng2, lowfreq = 60)
wf2 <- rowSums(as.matrix(ng2[w2,]))
wordGram2 <- data.frame(bigram = names(wf2), frequency = wf2, row.names= NULL)
save(wordGram2,file =  "WordGram2.RData")

w3 <- findFreqTerms(ng3, lowfreq = 10)
wf3 <- rowSums(as.matrix(ng3[w3,]))
wordGram3 <- data.frame(trigram = names(wf3), frequency = wf3, row.names = NULL)
save(wordGram3,file =  "WordGram3.RData")

w4 <- findFreqTerms(ng4, lowfreq = 10)
wf4 <- rowSums(as.matrix(ng4[w4,]))
wordGram4 <- data.frame(quadgram = names(wf4), frequency = wf4, row.names = NULL)
save(wordGram4, file = "WordGram4.RData")

#w5 <- findFreqTerms(ng5, lowfreq = 5)
#wf5 <- rowSums(as.matrix(ng5[w5,]))
#wordGram5 <- data.frame(pentgram = names(wf5), frequency = wf5, row.names = NULL)
#save(wordGram5, file = "WordGram5.RData")

#w6 <- findFreqTerms(ng6, lowfreq = 5)
#wf6 <- rowSums(as.matrix(ng6[w6,]))
#wordGram6 <- data.frame(hectgram = names(wf6), frequency = wf6, row.names = NULL)
#save(wordGram6, file = "WordGram6.RData")


w1s <- findFreqTerms(ng1sw, lowfreq = 500)
wf1s <- rowSums(as.matrix(ng1sw[w1s,]))
wordGram1s <- data.frame(unigram = names(wf1s), frequency = wf1s, row.names = NULL)
save(wordGram1s,file =  "WordGram1sw.RData")

w2s <- findFreqTerms(ng2sw, lowfreq = 300)
wf2s <- rowSums(as.matrix(ng2sw[w2s,]))
wordGram2s <- data.frame(bigram = names(wf2s), frequency = wf2s, row.names= NULL)
save(wordGram2s,file =  "WordGram2sw.RData")

w3s <- findFreqTerms(ng3sw, lowfreq = 100)
wf3s <- rowSums(as.matrix(ng3sw[w3s,]))
wordGram3s <- data.frame(trigram = names(wf3s), frequency = wf3s, row.names = NULL)
save(wordGram3s,file =  "WordGram3sw.RData")

w4s <- findFreqTerms(ng4sw, lowfreq = 24)
wf4s <- rowSums(as.matrix(ng4sw[w4s,]))
wordGram4s <- data.frame(quadgram = names(wf4s), frequency = wf4s, row.names = NULL)
save(wordGram4s, file = "WordGram4sw.RData")


w5s <- findFreqTerms(ng5sw, lowfreq = 9)
wf5s <- rowSums(as.matrix(ng5sw[w5s,]))
wordGram5s <- data.frame(pentgram = names(wf5s), frequency = wf5s, row.names = NULL)
save(wordGram5s, file = "WordGram5sw.RData")

w6s <- findFreqTerms(ng6sw, lowfreq = 5)
wf6s <- rowSums(as.matrix(ng6sw[w6s,]))
wordGram6s <- data.frame(hectgram = names(wf6s), frequency = wf6s, row.names = NULL)
save(wordGram6s, file = "WordGram6sw.RData")

w7s <- findFreqTerms(ng7sw, lowfreq = 4)
wf7s <- rowSums(as.matrix(ng7sw[w7s,]))
wordGram7s <- data.frame(septgram = names(wf7s), frequency = wf7s, row.names = NULL)


ggplot(wordGram2[1:15,], aes(x=reorder(bigram, frequency), y=frequency)) +
  geom_bar(stat = "identity", position = "dodge", fill = "blue") + 
  coord_flip() +
  xlab("Bi-gram words") + ylab("Sample Frequency") +
  ggtitle('Most Common Bi-grams')

ggplot(wordGram3[1:15,], aes(x= reorder(trigram, frequency),y=frequency))+
  geom_bar(stat = "identity", position = "dodge", fill = "green")+
  coord_flip()+
  xlab("trigram words") + ylab("sample frequency")+
  ggtitle("most common tri-grams")

               
               

require(tm)
require(quanteda)
require(stringi)
require(RWeka)
require(tau)
require(openNLP)
require(ggplot2)
require(filehash)
require(scales)


twitter <- readLines("~/Data Science Capstone/en_US.twitter.txt", encoding="UTF-8",skipNul=TRUE)
tSize <- file.info("~/Data Science Capstone/en_US.twitter.txt")$size / 1024 ^ 2 
length(twitter)
summary(nchar(twitter)) 

twitterWords <- stringi::stri_count_words(twitter)
summary(twitterWords)
twitterStats <- stringi::stri_stats_general(twitter)
twitterStats
range(twitterWords)
range(twitterStats)
sampleTdata <- twitter[sample(1:length(twitter), 30000)]
twitterStats1 <- stringi::stri_stats_general(sampleTdata)
twitterStats1

news <- readLines("~/Data Science Capstone/en_US.news.txt", encoding="UTF-8", skipNul=TRUE)
nSize <- file.info("~/Data Science Capstone/en_US.news.txt")$size / 1024 ^ 2 #file size
length(news)
summary(nchar(news)) 

newsWords <- stringi::stri_count_words(news)
summary(newsWords)
newsStats <- stringi::stri_stats_general(news)
newsStats
range(newsWords)
range(newsStats)
sampleNdata <- news[sample(1:length(news), 30000)]
newsStats1 <- stringi::stri_stats_general(sampleNdata)
newsStats1

blogs <- readLines("~/Data Science Capstone/en_US.blogs.txt", encoding="UTF-8", skipNul=TRUE)
bSize <- file.info("~/Data Science Capstone/en_US.blogs.txt")$size / 1024 ^ 2 #file size
length(blogs)
summary(nchar(blogs)) 

blogsWords <- stringi::stri_count_words(blogs)
summary(blogsWords)
blogsStats
range(blogsWords)
range(blogsStats)
sampleBdata <- blogs[sample(1:length(blogs), 30000)]
blogsStats1 <- stringi::stri_stats_general(sampleBdata)
blogStats1

data.frame(source = c("blogs", "news", "twitter"),
           file.size = c(bSize, nSize, tSize),
           length.lines = c(length(blogs), length(news), length(twitter)),
           amt.words = c(sum(blogsWords), sum(newsWords), sum(twitterWords)),
           mean.words = c(mean(blogsWords), mean(newsWords), mean(twitterWords)))
dataset <- c(twitter, news, blogs)

data.sample <-(sample(dataset, length(dataset) * 0.05))
corpus <- VCorpus(VectorSource(data.sample))
toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))

decimal.clean <- function(x) {gsub("([0-9]*)\\.([0-9]+)","\\1 \\2", x)}
hashtag.clean <- function(x) {gsub("#[a-zA-z0-9]+", " ", x)}
nonenglish.clean <- function(x) {gsub("\\W+", " ", x)}

corpus <- tm_map(corpus, decimal.clean)
corpus <- tm_map(corpus, hashtag.clean)
corpus <- tm_map(corpus, nonenglish.clean)
corpus <- tm_map(corpus, toSpace, "(f|ht)tp(s?)://(.*)[.][a-z]+")
corpus <- tm_map(corpus, toSpace, "@[^\\s]+")
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
weirdsletters <- c("b", "c", "d", "e", "f", "g", "h", "j", "k", "l", "m", "n", "p", "q", "r", "s", "t", "u", "v","w", "x", "y", "z", "www", "rt", "ll", "re", "ve", "ll", "th", "st", "pm", "lol")
corpus <- tm_map(corpus, removeWords, weirdsletters)
swearWords <- read.csv("~/Data Science Capstone/SwearWords.csv")
corpus <- tm_map(corpus, removeWords, swearWords)

corpus <- tm_map(corpus, PlainTextDocument)

NGramTokenizer1 <- function(x) unlist(lapply(NLP::ngrams(words(x), 1), paste,        collapse=" "), use.names=FALSE)

NGramTokenizer2 <- function(x) unlist(lapply(NLP::ngrams(words(x), 2), paste,        collapse=" "), use.names=FALSE)

NGramTokenizer3 <- function(x) unlist(lapply(NLP::ngrams(words(x), 3), paste,        collapse=" "), use.names=FALSE)

NGramTokenizer4 <- function(x) unlist(lapply(NLP::ngrams(words(x), 4), paste,        collapse=" "), use.names=FALSE)

ng1 <- TermDocumentMatrix(corpus, control=list(tokenize=NGramTokenizer1))
#save(ng1,file="NGram-1.RData")

ng2 <- TermDocumentMatrix(corpus, control=list(tokenize=NGramTokenizer2))
#save(ng2,file="NGram-2.RData")

ng3 <- TermDocumentMatrix(corpus, control=list(tokenize=NGramTokenizer3))
##save(ng3,file="NGram-3.RData")

ng4 <- TermDocumentMatrix(corpus, control=list(tokenize=NGramTokenizer4))

#load(file=paste("NGram-1.RData",sep=""))
#load(file=paste("NGram-2.RData",sep=""))
#load(file=paste("NGram-3.RData",sep=""))


w1 <- findFreqTerms(ng1, lowfreq = 50)
wf1 <- rowSums(as.matrix(ng1[w1,]))
wordGram1 <- data.frame(unigram = names(wf1), frequency = wf1, row.names = NULL)
save(wordGram1,file =  "WordGram1.RData")

w2 <- findFreqTerms(ng2, lowfreq = 50)
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

wf <- data.frame(trigram=names(wf), frequency=wf)
df <- data.frame(wf[,c('trigram','frequency')], row.names = NULL)
wordcloud(df$trigram, df$frequency, min.freq=50, colors=brewer.pal(6, "Dark2")) 

w <- findFreqTerms(ng2, lowfreq = 100)
wf <- rowSums(as.matrix(ng2[w,]))
df <- data.frame(word=names(wf), frequency=wf, row.names = NULL)

ggplot(df[1:15,], aes(x=reorder(word, frequency), y=frequency)) +
    geom_bar(stat = "identity", position = "dodge", fill = "darkred") + 
    coord_flip() +
    xlab("Bi-gram words") + ylab("Sample Frequency") +
    ggtitle('Most Common Bi-grams')



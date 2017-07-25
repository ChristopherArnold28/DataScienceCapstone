loadFiles <- function(){
    wg1 <- get(load(file = "WordGram1.RData"))
    wg2 <- get(load(file = "WordGram2.RData"))
    wg3 <- get(load(file = "WordGram3.RData"))
    wg4 <- get(load(file = "WordGram4.RData"))
    wg5 <- get(load(file = "WordGram5.RData"))
    wg6 <- get(load(file = "WordGram6.RData"))
    
    wg1 <- wg1[order(wg1$frequency, decreasing = TRUE),]
    wg2 <- wg2[order(wg2$frequency, decreasing = TRUE),]
    wg3 <- wg3[order(wg3$frequency, decreasing = TRUE),]
    wg4 <- wg4[order(wg4$frequency, decreasing = TRUE),]
    wg5 <- wg5[order(wg5$frequency, decreasing = TRUE),]
    wg6 <- wg6[order(wg6$frequency, decreasing = TRUE),]
    
    wg1$unigram <- as.character(wg1$unigram)
    wg2$bigram <- as.character(wg2$bigram)
    wg3$trigram<- as.character(wg3$trigram)
    wg4$quadgram <- as.character(wg4$quadgram)
    wg5$pentgram <- as.character(wg5$pentgram)
    wg6$hectgram <- as.character(wg6$hectgram)
    
    
    unigrams <- unlist(strsplit(wg1$unigram, " "))
    bigrams <- unlist(strsplit(wg2$bigram," "))
    trigrams <- unlist(strsplit(wg3$trigram," "))
    quadgrams <- unlist(strsplit(wg4$quadgram," "))
    pentgrams <- unlist(strsplit(wg5$pentgram," "))
    hectgrams <- unlist(strsplit(wg6$hectgram, " "))
    
    bigramsM <- matrix(data = bigrams, nrow = length(bigrams)/2,ncol = 2, byrow = TRUE)
    trigramsM <- matrix(data = trigrams, nrow = length(trigrams)/3,ncol = 3, byrow = TRUE)
    unigramsM <- matrix(data = unigrams, nrow = length(unigrams)/1, ncol = 1, byrow = TRUE)
    quadgramsM <- matrix(data = quadgrams, nrow = length(quadgrams)/4, ncol = 4, byrow = TRUE)
    pentgramsM <- matrix(data = pentgrams, nrow = length(pentgrams)/5,ncol = 5, byrow = TRUE)
    hectgramsM <- matrix(data = hectgrams, nrow = length(hectgrams)/6, ncol = 6, byrow = TRUE)
    
    wg1$first <- unigramsM[,1]
    
    wg2$first <- bigramsM[,1]
    wg2$second <- bigramsM[,2]
    
    wg3$first <- trigramsM[,1]
    wg3$second <- trigramsM[,2]
    wg3$third <- trigramsM[,3]
    
    wg4$first <- quadgramsM[,1]
    wg4$second <- quadgramsM[,2]
    wg4$third <- quadgramsM[,3]
    wg4$fourth <- quadgramsM[,4]
    
    wg5$first <- pentgramsM[,1]
    wg5$second <- pentgramsM[,2]
    wg5$third <- pentgramsM[,3]
    wg5$fourth <- pentgramsM[,4]
    wg5$fifth <- pentgramsM[,5]
    
    wg6$first <- hectgramsM[,1]
    wg6$second <- hectgramsM[,2]
    wg6$third <- hectgramsM[,3]
    wg6$fourth <- hectgramsM[,4]
    wg6$fifth <- hectgramsM[,5]
    wg6$sixth <- hectgramsM[,6]    
        
    wg1s <- get(load(file = "WordGram1sw.RData"))
    wg2s <- get(load(file = "WordGram2sw.RData"))
    wg3s <- get(load(file = "WordGram3sw.RData"))
    wg4s <- get(load(file = "WordGram4sw.RData"))
    wg5s <- get(load(file = "WordGram5sw.RData"))
    wg6s <- get(load(file = "WordGram6sw.RData"))
    
    wg1s <- wg1s[order(wg1s$frequency, decreasing = TRUE),]
    wg2s <- wg2s[order(wg2s$frequency, decreasing = TRUE),]
    wg3s <- wg3s[order(wg3s$frequency, decreasing = TRUE),]
    wg4s <- wg4s[order(wg4s$frequency, decreasing = TRUE),]
    wg5s <- wg5s[order(wg5s$frequency, decreasing = TRUE),]
    wg6s <- wg6s[order(wg6s$frequency, decreasing = TRUE),]
    
    wg1s$unigram <- as.character(wg1s$unigram)
    wg2s$bigram <- as.character(wg2s$bigram)
    wg3s$trigram<- as.character(wg3s$trigram)
    wg4s$quadgram <- as.character(wg4s$quadgram)
    wg5s$pentgram <- as.character(wg5s$pentgram)
    wg6s$hectgram <- as.character(wg6s$hectgram)
    
    unigrams <- unlist(strsplit(wg1s$unigram, " "))
    bigrams <- unlist(strsplit(wg2s$bigram," "))
    trigrams <- unlist(strsplit(wg3s$trigram," "))
    quadgrams <- unlist(strsplit(wg4s$quadgram," "))
    pentgrams <- unlist(strsplit(wg5s$pentgram," "))
    hectgrams <- unlist(strsplit(wg6s$hectgram, " "))
    
    
    bigramsMs <- matrix(data = bigrams, nrow = length(bigrams)/2,ncol = 2, byrow = TRUE)
    trigramsMs <- matrix(data = trigrams, nrow = length(trigrams)/3,ncol = 3, byrow = TRUE)
    unigramsMs<- matrix(data = unigrams, nrow = length(unigrams)/1, ncol = 1, byrow = TRUE)
    quadgramsMs <- matrix(data = quadgrams, nrow = length(quadgrams)/4, ncol = 4, byrow = TRUE)
    pentgramsMs <- matrix(data = pentgrams, nrow = length(pentgrams)/5,ncol = 5, byrow = TRUE)
    hectgramsMs <- matrix(data = hectgrams, nrow = length(hectgrams)/6, ncol = 6, byrow = TRUE)
    
    wg1s$first <- unigramsMs[,1]
    
    wg2s$first <- bigramsMs[,1]
    wg2s$second <- bigramsMs[,2]
    
    wg3s$first <- trigramsMs[,1]
    wg3s$second <- trigramsMs[,2]
    wg3s$third <- trigramsMs[,3]
    
    wg4s$first <- quadgramsMs[,1]
    wg4s$second <- quadgramsMs[,2]
    wg4s$third <- quadgramsMs[,3]
    wg4s$fourth <- quadgramsMs[,4]
    
    wg5s$first <- pentgramsMs[,1]
    wg5s$second <- pentgramsMs[,2]
    wg5s$third <- pentgramsMs[,3]
    wg5s$fourth <- pentgramsMs[,4]
    wg5s$fifth <- pentgramsMs[,5]
    
    wg6s$first <- hectgramsMs[,1]
    wg6s$second <- hectgramsMs[,2]
    wg6s$third <- hectgramsMs[,3]
    wg6s$fourth <- hectgramsMs[,4]
    wg6s$fifth <- hectgramsMs[,5]
    wg6s$sixth <- hectgramsMs[,6]    
    
    wordFiles <- list(wg1 = wg1, wg2 = wg2, wg3 = wg3,wg4 = wg4, wg5 = wg5, wg6 = wg6,wg1s = wg1s, wg2s = wg2s, wg3s = wg3s, wg4s = wg4s, wg5s = wg5s, wg6s = wg6s)
    return(wordFiles)
}

SelectWords <- function(wordArray, numWords, wg1, wg2, wg3,wg4, wg5,wg6){
    if(numWords>=5){
        if(nrow(wg6[wg6$first == wordArray[numWords-4] & wg6$second == wordArray[numWords-3] & wg6$third == wordArray[numWords - 2] & wg6$fourth == wordArray[numWords - 1] & wg6$fifth == wordArray[numWords], ])== 0){
            if(nrow(wg5[wg5$first == wordArray[numWords-3] & wg5$second == wordArray[numWords-2] & wg5$third == wordArray[numWords-1] & wg5$fourth == wordArray[numWords], ]) == 0){
                if(nrow(wg4[wg4$first == wordArray[numWords-2] & wg4$second == wordArray[numWords-1] & wg4$third == wordArray[numWords], ])==0){
                    if(nrow(wg3[wg3$first == wordArray[numWords - 1] & wg3$second == wordArray[numWords], ])==0){
                        if(nrow(wg2[wg2$first == wordArray[numWords], ])==0){
                            pProbs <- wg1
                            #print("phrase was longer than 5 but found no match on final word")
                        }
                        else{
                            pProbs <- wg2[wg2$first == wordArray[numWords], ]
                            #print("phrase was longer than 5 but found a 1 word match")
                        }
                    }
                    else{
                        pProbs <- wg3[wg3$first == wordArray[numWords - 1] & wg3$second == wordArray[numWords], ]
                        #print("phrase was longer than 5 but found a 2 word match")
                    }
                }
                else{
                    pProbs <- wg4[wg4$first == wordArray[numWords-2] & wg4$second == wordArray[numWords-1] & wg4$third == wordArray[numWords], ]
                    #print("phrase was longer than 5 but found a 3 word match")
                }
            }
            else{
                pProbs <- wg5[wg5$first == wordArray[numWords-3] & wg5$second == wordArray[numWords-2] & wg5$third == wordArray[numWords-1] & wg5$fourth == wordArray[numWords], ]
                #print("phrase was longer than 5 but found a 4 word match")
            }
        }
        else{
            pProbs <- wg6[wg6$first == wordArray[numWords-4] & wg6$second == wordArray[numWords-3] & wg6$third == wordArray[numWords - 2] & wg6$fourth == wordArray[numWords - 1] & wg6$fifth == wordArray[numWords], ]
                #print("found a full 5 word match")
        }
    }
    else if(numWords >=4){
        if(nrow(wg5[wg5$first == wordArray[numWords-3] & wg5$second == wordArray[numWords-2] & wg5$third == wordArray[numWords-1] & wg5$fourth == wordArray[numWords], ]) == 0){
            if(nrow(wg4[wg4$first == wordArray[numWords-2] & wg4$second == wordArray[numWords-1] & wg4$third == wordArray[numWords], ])==0){
                if(nrow(wg3[wg3$first == wordArray[numWords - 1] & wg3$second == wordArray[numWords], ])==0){
                    if(nrow(wg2[wg2$first == wordArray[numWords], ])==0){
                        pProbs <- wg1
                        #print("phrase was longer than 4 but found no match on final word")
                    }
                    else{
                        pProbs <- wg2[wg2$first == wordArray[numWords], ]
                        #print("phrase was longer than 4 but found a 1 word match")
                    }
                }
                else{
                    pProbs <- wg3[wg3$first == wordArray[numWords - 1] & wg3$second == wordArray[numWords], ]
                    #print("phrase was longer than 4 but found a 2 word match")
                }
            }
            else{
                pProbs <- wg4[wg4$first == wordArray[numWords-2] & wg4$second == wordArray[numWords-1] & wg4$third == wordArray[numWords], ]
                #print("phrase was longer than 4 but found a 3 word match")
            }
        }
        else{
            pProbs <- wg5[wg5$first == wordArray[numWords-3] & wg5$second == wordArray[numWords-2] & wg5$third == wordArray[numWords-1] & wg5$fourth == wordArray[numWords], ]
            #print("found a full 4 word match")
        }
        
    }
    else if(numWords >=3){
        if(nrow(wg4[wg4$first == wordArray[numWords-2] & wg4$second == wordArray[numWords-1] & wg4$third == wordArray[numWords], ])==0){
            if(nrow(wg3[wg3$first == wordArray[numWords - 1] & wg3$second == wordArray[numWords], ])==0){
                if(nrow(wg2[wg2$first == wordArray[numWords], ])==0){
                    pProbs <- wg1
                    #print("phrase was longer than 3 but found no match on final word")
                }
                else{
                    pProbs <- wg2[wg2$first == wordArray[numWords], ]
                    #print("phrase was longer than 3 but found a 1 word match")
                }
            }
            else{
                pProbs <- wg3[wg3$first == wordArray[numWords - 1] & wg3$second == wordArray[numWords], ] 
                #print("phrase was longer than 3 but found a 2 word match")
            }
        }
        else{
            pProbs <- wg4[wg4$first == wordArray[numWords-2] & wg4$second == wordArray[numWords-1] & wg4$third == wordArray[numWords], ]
            #print("found a full 3 word match")
        }
    }
    else if(numWords >= 2){
        if(nrow(wg3[wg3$first == wordArray[numWords - 1] & wg3$second == wordArray[numWords], ])==0){
            if(nrow(wg2[wg2$first == wordArray[numWords], ])==0){
                pProbs <- wg1
                #print("phrase was longer than 2 but found no match on final word")
            }
            else{
                pProbs <- wg2[wg2$first == wordArray[numWords], ]
                #print("phrase was longer than 2 but found a 1 word match")
            }
        }
        else{
            pProbs <- wg3[wg3$first == wordArray[numWords - 1] & wg3$second == wordArray[numWords], ] 
            #print("found a full 2 word match")
        }
    }
    else if(numWords >= 1){
        if(nrow(wg2[wg2$first == wordArray[numWords], ])==0){
            pProbs <- wg1
            #print("phrase was 1 word and found no match")
        }
        else{
            pProbs <- wg2[wg2$first == wordArray[numWords], ]
            #print("found a 1 word match on 1 word phrase")
        }
    }
    else{
        pProbs <- wg1
    }
    
    pProbs$probability <- pProbs$frequency/sum(pProbs$frequency)
    wordIndex <- ncol(pProbs) - 1
    chosenWord<-sample(pProbs[,wordIndex], size = 10, replace = TRUE, prob = (pProbs$frequency/sum(pProbs$frequency)))
    
    return(pProbs)
}
    
NGramPrediction <- function(input, wordFiles){
    library(quanteda)
    input <- toLower(input)
    stops <- stopwords()
    wordArray <- unlist(strsplit(input, " "))
    wordArray2 <- wordArray[! wordArray %in% stops]
    
    numWords <- length(wordArray)
    numWords2 <- length(wordArray2)
    
    wg1 <- wordFiles$wg1
    wg2 <- wordFiles$wg2
    wg3 <- wordFiles$wg3
    wg4 <- wordFiles$wg4
    wg5 <- wordFiles$wg5
    wg6 <- wordFiles$wg6
    
    wg1s <- wordFiles$wg1s
    wg2s <- wordFiles$wg2s
    wg3s <- wordFiles$wg3s
    wg4s <- wordFiles$wg4s
    wg5s <- wordFiles$wg5s
    wg6s <- wordFiles$wg6s
    
    chosenWordsSW <- SelectWords(wordArray, numWords, wg1s,wg2s,wg3s,wg4s,wg5s, wg6s)
    
    
    
    if(chosenWordsSW[1,length(colnames(chosenWordsSW))-1] %in% stops && chosenWordsSW[1,length(colnames(chosenWordsSW))] > .125){
        wordArray2 <- c(wordArray2, chosenWordsSW[1,length(colnames(chosenWordsSW))-1])
        numWords2 <- length(wordArray2)
    }
    chosenWords <- SelectWords(wordArray2, numWords2, wg1,wg2,wg3,wg4,wg5,wg6)


    allWords <- list(chosenWords = chosenWords, chosenWordsSW = chosenWordsSW)
    #table(allWords)
    return(allWords)
    
    
}


createWordList <- function(nGramList){
    noStopWords <- nGramList$chosenWords
    stopWords   <- nGramList$chosenWordsSW
    choicesSW <- as.character(stopWords[,length(colnames(stopWords))-1])
    choicesSW.prob <- stopWords$probability
    
    choicesNSW <- as.character(noStopWords[,length(colnames(noStopWords))-1])
    choicesNSW.prob <- noStopWords$probability
    
    probabilities <- c(choicesSW.prob, choicesNSW.prob)
    choices <- c(choicesSW, choicesNSW)
    
    wordFrame <- data.frame(choices, probabilities)
    wordFrame <- wordFrame[order(wordFrame$probabilities, decreasing = TRUE),]
    #print(head(wordFrame))
    return(head(wordFrame))
    
}


getWord <- function(input,rank, wordFiles){
    nGramList <- NGramPrediction(input, wordFiles)
    topWords <- createWordList(nGramList)
    topWords <- aggregate(.~choices, data = topWords, FUN = sum)
    topWords <- topWords[order(topWords$probabilities, decreasing = TRUE),]
    return(as.character(topWords$choices[rank]))
}



testRow <- function(string, wordFiles){
    wordlist <- unlist(strsplit(string," "))
    answer <- as.character(wordlist[length(wordlist)])
    input <- wordlist[-length(wordlist)]
    input <- as.character(paste(input, collapse = " "))
    prediction <- getWord(input, 1, wordFiles)
    #return(prediction)
    errorFrame <- data.frame(originalString = string, input = input, answer = answer, predicted = prediction )
    if(as.character(errorFrame$answer) == as.character(errorFrame$predicted)){
        errorFrame$check = 1
    }
    else{
        errorFrame$check = 0
    }
    return(errorFrame)
}

testList <- function(strings, wordFiles){
    resultsFrame <- data.frame()   
    for(i in 1:length(strings)){
            newError <- testRow(strings[i], wordFiles)
            resultsFrame <- rbind(resultsFrame, newError)
    }
    #return(resultsFrame)
    errorRate <- sum(resultsFrame$check)/length(resultsFrame$check)
    
    errors <- list(errorFrame = resultsFrame, correctRate = errorRate)
    return(errors)
}


bigramlist <- sample(wg2s$bigram, size = 200, replace = FALSE)
trigramlist <- sample(wg3s$trigram, size = 200, replace = FALSE)
quadgramlist <- sample(wg4s$quadgram, size = 200, replace = FALSE)
pentgramlist <- sample(wg5s$pentgram, size = 200, replace = FALSE)
hectgramlist <- sample(wg6s$hectgram, size = 200, replace = FALSE)

bigramerror <- testList(bigramlist, wordfiles)
trigramerror <- testList(trigramlist, wordfiles)
quadgramerror <- testList(quadgramlist, wordfiles)
pentgramerror <- testList(pentgramlist, wordfiles)
hectgramerror <- testList(hectgramlist, wordfiles)

bigramerror$correctRate
trigramerror$correctRate
quadgramerror$correctRate
pentgramerror$correctRate
hectgramerror$correctRate







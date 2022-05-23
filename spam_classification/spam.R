## Functions for Sample Classification

## split message into header and body
splitMessage <- function(email) {
    ## find first balnk line
    split <- which(email == "")[1]
    ## everything before first blank line is header
    header <- email[1:(split-1)]
    ## everything after first blank line is body
    body <- email[(split+1):length(email)]
    return( list(header=header,
                 body=body) )
}

## get distributon of email lengths
lengthDist <- function(emails) {
    dist <- sapply(emails, function(x) {
        s <- splitMessage(x)
        ## get length of the split
        length( s[[1]] )
    })
    return(dist)
}

## does email have attachment
hasAttachment <- function(header) {
    ## find index with Content-Type
    contentType <- grep("Content-Type", header, value=TRUE)
    ## find the lines w/ "multipart"
    if(length(contentType) == 0) {
        return(FALSE)
    }
    return( grepl("multipart", contentType, ignore.case=TRUE) )
}

getBoundary <- function(header) {
    boundaryLine <- grep("boundary=", header, value = TRUE)
    if (length(boundaryLine) == 0) {
        return("")
    }
    
    ## remove boundary= with possibly a space or "
    boundaryLine <- gsub(".*boundary=\\s?\"?", "", boundaryLine)

    ## remove end with possibly a " or ;
    boundaryLine <- gsub("\";?.*$", "", boundaryLine)
                         
    return(boundaryLine)
}

## seperate the boundary from the body text
extractBodyText <- function(email) {
    split <- splitMessage(email)
    header <- split$header
    body <- split$body

    ## return the body, if the e-mail does not have an attachment
    if(!hasAttachment(header)) {
       return(body)
    }

    boundaryLine <- getBoundary(header)
    boundary <- grep(boundaryLine, body, fixed=TRUE)

    ## if there is only one boundary line, get all the text afterwards
    if(length(boundary) == 1) {
        return( body[(boundary+1):length(boundary)] )
    }

    ## if there are more than one boundary line, go beyond until you
    ## reach the second boundary text
    if(length(boundary) > 1) {
        return( body[(boundary[1]+1):(boundary[2]-1)] )
    }
    
    return(body)    
}

extractWords <- function(body, unique=FALSE) {
    ## make vector into body text, adding space between every line
    body <- paste(body, collapse = ' ')
    ## deparse and remove all resulting backslashes
    body <- deparse(body)
    body <- gsub('\\\\...', '', body)
    ## replace punctuation with spaces
    body <- gsub("[[:punct:]]", ' ', body)
    ## remove all digits
    body <- gsub("[[:digit:]]", '', body)
    ## make all words lowercase
    body <- tolower(body)
    ## replace single letter words with just space
    body <- gsub("\\s[[:alpha:]]\\s", ' ', body)

    ## remove any excessive spaces between words
    ## and any spaces at the start or end of the string
    body <- gsub("\\s{2,}", ' ', body)
    body <- gsub("^\\s", '', body)
    body <- gsub("\\s$", '', body)
    
    ## get all the words unlisted    
    words <- unlist(strsplit(body, " "))
    if(unique) {
        return( unique(words) )
    }
    return(words)
}

readEmailDirectory <- function(directory) {
    files <- list.files(directory, full.names = TRUE)
    return(lapply(files, readLines))
}

## the words of every email in one list
## order: all the hame emails, all the spam emails
dirNames <- c("data/messages/easy_ham", "data/messages/easy_ham_2", "data/messages/hard_ham", "data/messages/spam", "data/messages/spam_2")
directories <- lapply(dirNames, readEmailDirectory)
emails <- unlist(directories, recursive=FALSE)
bodyTexts <- lapply(emails, extractBodyText)
emailsAll <- lapply(bodyTexts, function(body) {
    extractWords(body, unique=TRUE)
})

## get FALSE for every ham email
hamNames <- c("data/messages/easy_ham", "data/messages/easy_ham_2", "data/messages/hard_ham")
directories <- lapply(hamNames, readEmailDirectory)
emails <- unlist(directories, recursive=FALSE)
false <- sapply(1:length(emails), function(x) {
    return(FALSE)
})

## get TRUE for every spam email
spamNames <- c("data/messages/spam", "data/messages/spam_2")
directories <- lapply(spamNames, readEmailDirectory)
emails <- unlist(directories, recursive=FALSE)
true <- sapply(1:length(emails), function(x) {
    return(TRUE)
})

## object with true/false labels corresponding to allEmails
isSpam <- append(false, true)

## training / test sets
## mak cutoff at 2/3
trainingFrac <- 2/3

## divide emails and isSpam into ham and spam
hamEmails <- emailsAll[!isSpam]
spamEmails <- emailsAll[isSpam]
hamIsSpam <- isSpam[!isSpam]
spamIsSpam <- isSpam[isSpam]

## get cut off for train/test in ham/spam vectors
hamCutoff <- trainingFrac * length(hamIsSpam)
hamTrain <- 1:hamCutoff
hamTest <- (hamCutoff+1):length(hamIsSpam)
spamCutoff <- trainingFrac * length(spamIsSpam)
spamTrain <- 1:spamCutoff
spamTest <- (spamCutoff+1):length(spamIsSpam)

## apply cutoff
emailsTrain <- append(hamEmails[hamTrain], spamEmails[spamTrain])
emailsTest <- append(hamEmails[hamTest], spamEmails[spamTest])
isSpamTrain <- append(hamIsSpam[hamTrain], spamIsSpam[spamTrain])
isSpamTest <- append(hamIsSpam[hamTest], spamIsSpam[spamTest])

## create bag of words from the training emails 
bow <- unique(unlist(emailsTrain))

## native bayesian classifer:
## 1. words counts
## it's only unqiue words, so we can just use table
wordSpam <- table(unlist(emailsTrain[isSpamTrain]))
wordHam <- table(unlist(emailsTrain[!isSpamTrain]))

## 2. word probabalities
probSpam <- sapply(bow, function(word) {
    if(word %in% names(wordSpam)) {
        return( (wordSpam[word]+0.1) / (length(emailsTrain[isSpamTrain])+0.1) )
    }
    ## if it's not in the e-mails, the probability is simply 0.1 / length
    return( 0.1 / (length(emailsTrain[isSpamTrain])+0.1) )
})

probHam <- sapply(bow, function(word) {
    if(word %in% names(wordHam)) {
        return( (wordHam[word]+0.1) / (length(emailsTrain[!isSpamTrain])+0.1) )
    }
    ## if it's not in the e-mails, the probability is simply 0.1 / length
    return( 0.1 / (length(emailsTrain[!isSpamTrain])+0.1) )
})

## 3. log probabilites
logProbPresentSpam <- sapply(probSpam, function(prob) {
    log(prob)
})
logProbPresentHam <- sapply(probHam, function(prob) {
    log(prob)
})
logProbAbsentSpam <- sapply(probSpam, function(prob) {
    log(1 - prob)
})
logProbAbsentHam <- sapply(probHam, function(prob) {
    log(1 - prob)
})

## returns the bayes factor for the unique words in an e-mail body
computeBF <- function(uniqueBodyWords) {
    inBody <- which(bow %in% uniqueBodyWords)
    notInBody <- which(!(bow %in% uniqueBodyWords))

    ## get probabilites,depending on wether it's in the body
    spamEvidence <- sum(logProbPresentSpam[inBody]) + sum(logProbAbsentSpam[notInBody])
    hamEvidence <- sum(logProbPresentHam[inBody]) + sum(logProbAbsentHam[notInBody])

    return( spamEvidence - hamEvidence )
}

## calculate type 1 and type 2 errors
type1Type2 <- function(testBF, cValues) {
    spamBF <- testBF[isSpamTest]
    hamBF <- testBF[!isSpamTest]

    ## how often does a ham e-mail incorrectly get specified as spam?
    type1 <- sapply(cValues, function(c) {
        length(which(hamBF >= c))/length(hamBF)
    })

    ## how often does a spam e-mail incorrectly get specified as not spam?
    type2 <- sapply(cValues, function(c) {
        length(which(spamBF < c))/length(spamBF)
    })
    return( list(type1, type2) )
}

## plot the type 1 and type 2 errors along the c-values
plotErrorRates <- function(testBF, cValues) {
    t1t2 <- type1Type2(testBF, cValues)
    type1 <- t1t2[[1]]
    type2 <- t1t2[[2]]
    
    plot(x=cValues, y=type1, col="red", type="l", ylim=c(0, max(c(type1, type2))),
         xlab="C values", ylab="Error rate", main="Rate of type 1 & type 2 errors")
    lines(x=cValues, y=type2, col="blue")
    legend(100, 0.95, c("Type 1", "Type 2"), lwd=1, col=c("red", "blue"))
}

## get c-value that minmizes type 1 error rate to at lest 0.1%
## or that equates type 1 and type 2 error rates
fitErrorRates <- function(testBF, cValues, equal=FALSE) {
    t1t2 <- type1Type2(testBF, cValues)
    type1 <- t1t2[[1]]
    type2 <- t1t2[[2]]

    if(equal) {
        ## get indecies at the closest values of type1 and type2
        intersect <- sapply(1:length(cValues), function(i) {
            abs(type1[i] - type2[i])                        
        })

        equal <- which.min(intersect)
        return( c("c"=cValues[equal], "type1"=type1[equal], "type2"=type2[equal]) )
    }

    ## get the cvalue that minimizes type 1 errors to at least 0.001 (0.1%)
    minType1 <- which(type1 < 0.001)[1]
    return( c("c"=cValues[minType1], "type1"=type1[minType1], "type2"=type2[minType1]) )
}

## extension
## extract information form the e-mail header on e-mail adresss, username,
## domain, domain site, domain extensions
## returnExtensions to get a detailed list of all the extensions for every e-mail
headerInfo <- function(email, returnExtensions=FALSE) {
    ## get first line of email
    split <- splitMessage(email)
    header <- split$header
    fromLine <- strsplit(header[1], " ")[[1]]
    fromLine <- tolower(fromLine)
        
    ## get adress of email
    adress <- fromLine[2]
    adressSplit <- strsplit(adress, "@")[[1]]
    ## split into name and domain
    username <- adressSplit[1]
    domain <- adressSplit[2]

    ## split domain into site name and extension(s)
    domainSplit <- strsplit(domain, "\\.")[[1]]
    site <- domainSplit[1]
    extensions <- domainSplit[2:length(domainSplit)]
    extensions <- gsub("[^[:alnum:]]", "", extensions) ## remove all non-alpha-num chars
    extension <- paste(extensions , collapse=".")

    ## possiblity to return vector of all the different extensions
    if(returnExtensions) {
        return(extensions)
    }

    return(c("adress"=adress, "username"=username, "domain"=domain,
             "site"=site, "extension"=extension))
}

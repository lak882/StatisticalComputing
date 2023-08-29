## Deliverables For Spam Classification 

## 1.
## ham distribution
hamFiles <- list.files("data/messages/easy_ham", full.names = TRUE)
hamFiles <- append(hamFiles, list.files("data/messages/ham", full.names = TRUE))
hamFiles <- append(hamFiles, list.files("data/messages/easy_ham", full.names = TRUE))

ham <- lapply(hamFiles, readLines)
hamDist <- lengthDist(ham)
hist(hamDist, main="Header length in ham e-mails")
mean(hamDist)
sd(hamDist)

## spam distribution
spamFiles <- list.files("data/messages/spam", full.names = TRUE)
spam <- lapply(spamFiles, readLines)
spamDist <- lengthDist(spam)
hist(spamDist, main="Header length in spam e-mails")
mean(spamDist)
sd(spamDist)

## 2.
files <- list.files("data/messages/easy_ham", full.names = TRUE)
sampleIndex <- 1:500
sampleFiles <- files[sampleIndex]
sampleEmails <- lapply(sampleFiles, readLines)
sampleHeaders <- sapply(sampleEmails, function(x) splitMessage(x)$header)
sampleHasAttachments <- sapply(sampleHeaders, function(x) hasAttachment(x))
emailIndWithAttachments <- which(sampleHasAttachments)

boundaries <- sapply(sampleHeaders[emailIndWithAttachments], getBoundary)
test <- c("==_Exmh_-1317289252P", "----=_NextPart_000_00C1_01C25017.F2F04E20", "Apple-Mail-2-874629474", "_NextPart_1_bvfoDiTVghtoCXFdvJNKcuWblFV", "------------080808010909060409040405", "==_Exmh_1547759024P", "----------=_1034083278-26594-4", "==_Exmh_-1317289252P", "==_Exmh_-518574644P", "------------090602010909000705010009", "==_Exmh_-403670396P", "==_Exmh_-398538836P", "==_Exmh_-763629846P", "==_Exmh_-451422450P")
test <- sapply(1:length(test), function(t) {
    return( test[1] == boundaries[1] )
})

test

## 3.
dirNames <- c("data/messages/easy_ham", "data/messages/easy_ham_2", "data/messages/hard_ham", "data/messages/spam", "data/messages/spam_2")
dirNames2 <- c("easy_ham", "easy_ham_2", "hard_ham", "spam", "spam_2")
directories <- lapply(dirNames, readEmailDirectory)
box <- lapply(directories, function(x)
    lapply(x, function(y) length(extractWords(extractBodyText(y))))
    )
box <- lapply(boxplot, unlist)
names(box) <- dirNames2
boxplot(box, main="# of words in e-mail body")

boxlog <- lapply(box, log)
boxplot(boxlog, main="log of # of words in e-mail body")

## 4.
emailsUnique <- lapply(emailsAll, unique)
spamLength <- sapply(emailsUnique[which(isSpam)], length)
hamLength <- sapply(emailsUnique[which(!isSpam)], length)

spamHist <- hist(spamLength,
hamHist <- hist(hamLength, xlim=c(0,750), breaks=50)
c1 <- rgb(173,216,230,max = 255, alpha = 80, names = "lt.blue")
c2 <- rgb(255,192,203, max = 255, alpha = 80, names = "lt.pink")
plot(hamHist, col=c1, freq=FALSE, xlim=c(0,750), breaks=50, main="# of unqiue words in e-mails", xlab="# of unique words")
plot(spamHist, col=c2, freq=FALSE,  xlim=c(0,750), breaks=50, add=TRUE)
legend(600, 0.005, c("Ham", "Spam"), lwd=15, col=c(c1, c2))

## 6.
## a.
probHam["monday.monday"] / probSpam["monday.monday"]

## b.
probSpam["buy.buy"] / probHam["buy.buy"]

## 7.
## a.
## compute the BF for the test emails
testBF <- sapply(emailsTest, computeBF)
spamBF <- testBF[isSpamTest]
hamBF <- testBF[!isSpamTest]

## b.
boxplot(spamBF, hamBF,
        main = "Log bayes factor of spam & ham e-mails",
        names = c("Spam BF", "Ham BF"))

##8.
cValues <- -200:2000
plotErrorRates(testBF, cValues)

## 9.
fitErrorRates(testBF, cValues, equal=TRUE)
plotErrorRates9 <- function(testBF, cValues) {
    t1t2 <- type1Type2(testBF, cValues)
    type1 <- t1t2[[1]]
    type2 <- t1t2[[2]]
    
    plot(x=cValues, y=type1, col="red", type="l",
         xlab="C values", ylab="Error rate", main="Rate of type 1 & type 2 errors",
         xlim=c(-40, 0), ylim=c(0.05,0.2))
    lines(x=cValues, y=type2, col="blue")
    legend(900, 0.85, c("Type 1", "Type 2"), lwd=1, col=c("red", "blue"))
}
plotErrorRates9(testBF, cValues)

## 10.
cValues <- -150:1000
fitErrorRates(testBF, cValues)

plotErrorRates10 <- function(testBF, cValues) {
    t1t2 <- type1Type2(testBF, cValues)
    type1 <- t1t2[[1]]
    type2 <- t1t2[[2]]
    
    plot(x=cValues, y=type1, col="red", type="l", ylim=c(0, max(c(type1, type2))),
         xlab="C values", ylab="Error rate", main="Rate of type 1 & type 2 errors")
    lines(x=cValues, y=type2, col="blue")
    
    c <- fitErrorRates(testBF, cValues)["c"]
    abline(v=c, col="olivedrab", lty=2, lwd=3)
    
    legend(800, 0.85, c("Type 1", "Type 2", "Best C"), lty=c(1,1,2), lwd=c(1,1,3), col=c("red", "blue", "olivedrab"))
}
plotErrorRates10(testBF, cValues)

## extension

dirNames <- c("data/messages/easy_ham", "data/messages/easy_ham_2", "data/messages/hard_ham", "data/messages/spam", "data/messages/spam_2")
directories <- lapply(dirNames, readEmailDirectory)
emails <- unlist(directories, recursive=FALSE)

## 1. domain extensions
extensions <- lapply(emails, function(e) {
    headerInfo(e, returnExtensions=TRUE)
})

ext <- table(unlist(extensions))
topExtNames <- names(sort(ext, decreasing=TRUE)[1:10])

## ham / spam
hamExt <- table(unlist(extensions[!isSpam]))
spamExt <- table(unlist(extensions[isSpam]))

## plot ham / spam e-mails
topExt <- rbind(hamExt[topExtNames], spamExt[topExtNames])
barplot(topExt, beside=T, xlab="Extension", ylab="Frequency", main="Most frequent sender e-mail domain extensions", col=c("dodgerblue", "firebrick1"))
legend(25, 2500, legend=c("Ham", "Spam"), col=c("dodgerblue", "firebrick1"), lwd=5)

## # of spam e-mails with .com
spamExt["com"] / length(extensions[isSpam])

## 2. Usernames 
usernames <- lapply(emails, function(e) {
    headerInfo(e)["username"]
})

usernames <- unlist(usernames)
spamLength <- sapply(usernames[isSpam], nchar)
hamLength <- sapply(usernames[!isSpam], nchar)
boxplot(spamLength, hamLength,
        main = "Length of username",
        names = c("Spam", "Ham"))

uniqchars <- function(x) nchar(unique(strsplit(x, "")[[1]])
spamLength <- sapply(usernames[isSpam], uniqchars)
hamLength <- sapply(usernames[!isSpam], uniqchars)
boxplot(spamLength, hamLength,
        main = "Number of unique characters username",
        names = c("Spam", "Ham"))

subAlpha <- function(x) nchar(gsub("[^[:digit:]]", "", x))
spamLength <- na.omit(sapply(usernames[isSpam], subAlpha))
hamLength <- sapply(usernames[!isSpam], subAlpha)
boxplot(spamLength, hamLength,
        main = "Number of digits in username",
        names = c("Spam", "Ham"))

## tradiontal mean
mean(hamLength)
mean(spamLength)
## remove outliers 
mean(spamLength[!spamLength %in% boxplot.stats(spamLength)$out])
mean(hamLength[!hamLength %in% boxplot.stats(hamLength)$out])

## 3.
usernames <- lapply(emails, function(e) {
    headerInfo(e)["username"]
})

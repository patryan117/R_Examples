
# Author: Patrick Ryan
# Class: CS688 (Boston University: Metropolitian College)


# 1. Create a Corpus from all of the text for the 2 newsgroups you used in the Newsgroops assignment. (alternatively use the code on Blackboard)
# 2. Implement just the following preprocessing:
#   • Remove Whitespace
# • Convert to Lowercase
# • Remove Punctuation

# 3.  Create a document term matrix and find  the 20 most frequent  words.
# •  Find their frequency
# •  Find  their  frequency  %

# 4.  Create a linear model from the  data and find what is  Zipf’s  exponent for this dataset. 
# •  Familiarize yourself with the R fitting function “  lm  ()” from “stats” package  .  Use  lm  (Y ~ X , data  )
# •  Take X to be the natural logarithm  of the  Frequency
# •  Take  Y  to be the  natural logarithm  of the  Rank (the order from most to list  popular words  )

# 5.  Plot  Percentage  of all words vs unique  words.
# •  What is the percentage  of  single  words (appearing only once)

# 6.  Check is  half  of ANY text is 50 to 100 same  words.
# •  Plot  Cumulative Percent vs  the  100 most frequent words

# 7.  Submit your  R  script code.

library(tm)
getwd()
load('Nresgroup.Dtm.RData')

#3) Find frequency and frequency percentage for the top 20 words
freq <- colSums(dtm)
head(freq)
ord <- order(freq, decreasing = TRUE)
frequency <- freq[ord]
freq.percent <- frequency/sum(frequency)*100
head(freq.percent)

#4) Create a linear model from the  data and find what is  Zipf’s  exponent .
total.num.words <- dim(dtm)[2]
rank <- 1:total.num.words
x <- log(rank)
y <- log(frequency)
data <- data.frame(X=x, Y=y)
plot(data, col = "red")
model <- lm(Y ~ X, data)
model$coefficients
s <- round(model$coefficients[2],4)
s

predictY <- predict(model, data)
predictY
lines(data$X, predictY, col = "blue", pch=4)
newsgroup1 <- "newsgroup1"
newsgroup2 <- "newsgroup2"
Title <- paste0("Zipf's law (s=", s, ") for Newsgroups ",newsgroup1," & " , newsgroup2)
title(Title)

Zipf_plot(dtm, type = "p", col = "red")
yy <- cumsum(freq.percent)
xx <- rank
data1 <- data.frame(X=xx, Y=yy)
plot(data1, col = "red", xlab = "Rank of Unique Words", ylab = "Cumulative Percent (%)")
grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted")


#5)#  What is the percentage  of  single  words (appearing only once)
ix <- which(frequency<2)
single.words <- sum(frequency[ix])/total.num.words*100
single.words

#6) Plot  Cumulative Percent vs  the  100 most frequent words
Title1 <- "The 100 most frequent words comprise 50% of the entire text!"
Title2 <- "Half (50%) na ANY test is the same (most frequent) words!"
Title <- paste(Title1, Title2, sep = "\n")
plot(data1[1:100,], col="red",  xlab ="Rank", ylab = "Cumulative Percent(%)" )
grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted")
title(main = Title, sub = "The 100 Most Frequent Words")


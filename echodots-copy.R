library("rvest")
pacman::p_load(XML, dplyr, stringr, rvest, audio)

#Remove all white space
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

prod_code = "B01DFKC2SO"
url <- paste0("https://www.amazon.com/dp/", prod_code)
doc <- read_html(url)

#obtain the text in the node, remove "\n" from the text, and remove white space
prod <- html_nodes(doc, "#productTitle") %>% html_text() %>% gsub("\n", "", .) %>% trim()
prod

#Source funtion to Parse Amazon html pages for data
source("https://raw.githubusercontent.com/rjsaito/Just-R-Things/master/Text%20Mining/amazonscraper.R")

pages <- 10

reviews_all <- NULL
for(page_num in 1:pages){
  url <- paste0("http://www.amazon.com/product-reviews/",prod_code,"/?pageNumber=", page_num)
  doc <- read_html(url)
  
  reviews <- amazon_scraper(doc, reviewer = F, delay = 2)
  reviews_all <- rbind(reviews_all, cbind(prod, reviews))
}

# install.packages("sentimentr")
library("sentimentr")
pacman::p_load_gh("trinker/sentimentr")

sent_agg <- with(reviews_all, sentiment_by(comments))
head(sent_agg)

par(mfrow=c(1,2))
with(reviews_all, hist(stars))
with(sent_agg, hist(ave_sentiment))

#Ratings and reviews distribution
mean(reviews_all$stars)
mean(sent_agg$ave_sentiment)

library(tm)
# update.packages("tm",  checkBuilt = TRUE)
# install.packages("SnowballC")
library(SnowballC)
# setwd("~/Desktop/Dihuni")

#read in data
reviews = VCorpus(VectorSource(reviews_all[,2]))

docs_corpus = VCorpus(VectorSource(reviews))

docs_corpus = tm_map(docs_corpus, stripWhitespace)

docs_corpus = tm_map(docs_corpus, tolower)

docs_corpus = tm_map(docs_corpus, removePunctuation)

#remove stop words
docs_corpus = tm_map(docs_corpus, removeWords, stopwords("english"))

#stemming
docs_corpus = tm_map(docs_corpus, stemDocument)

#construct document term matrix
docs_corpus = tm_map(docs_corpus, PlainTextDocument)

dtm = DocumentTermMatrix(docs_corpus)
inspect(dtm)

findFreqTerms(dtm, 3)
findFreqTerms(dtm, 5)
findFreqTerms(dtm, 10)

#Generate Word Cloud
# Install
# install.packages("tm")  # for text mining
# install.packages("SnowballC") # for text stemming
# install.packages("wordcloud") # word-cloud generator 
# install.packages("RColorBrewer") # color palettes
# Load
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

dtm <- TermDocumentMatrix(docs_corpus)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

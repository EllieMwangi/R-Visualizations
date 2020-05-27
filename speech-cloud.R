library(tm) #For text mining
library(SnowballC) #For text stemming
library(wordcloud) #wordcloud generator
library(RColorBrewer) #color palettes

library(tm) #For text mining
library(SnowballC) #For text stemming
library(wordcloud) #wordcloud generator
library(RColorBrewer) #color palettes

text <- readLines("./data/commencement-speeches.txt")

# Load the data as a corpus
docs <- Corpus(VectorSource(text))

#Remove special characters and replace with space
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")

# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))

# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
# Text stemming
#docs <- tm_map(docs, stemDocument)

#Create a document matrix
dtm <- TermDocumentMatrix(docs)
term_matrix <- as.matrix(dtm)

# Arrange words in terms of frequency of occurence
sorted_matrix <- sort(rowSums(term_matrix), decreasing=T)

# Create word frame
word_frame <- data.frame(word = names(sorted_matrix), freq = sorted_matrix)

head(word_frame, 10)

#Create word cloud

set.seed(200)

wordcloud(words = word_frame$word, freq = word_frame$freq,scale = c(2.5,0.35),min.freq = 1, max.words = 100,
          random.order=F, colors = brewer.pal(8,"Dark2"))

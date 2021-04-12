##importing our data
sms_raw <- read.csv("spam.csv", stringsAsFactors = FALSE)
##take a look on the data
str(sms_raw)
##Manipulate our data
sms_raw=sms_raw[,c(1,2)]
colnames(sms_raw )= c("type","text")
#Turn type into a factor
sms_raw$type <- factor(sms_raw$type)
str(sms_raw$type)
##check how many spam and legit SMS we got
table(sms_raw$type)
##cleaning and standardizing text data
library(NLP)
library(tm)
##creating a corpus, which is a collection of text documents
sms_corpus <- VCorpus(VectorSource(sms_raw$text))
print(sms_corpus)
#accessing the corpus
inspect(sms_corpus[1:2])
#viewing the actual message
as.character(sms_corpus[[2]])
#viewing mulitple messages
lapply(sms_corpus[2:12], as.character)
##standardize the messages to use only lowercase characters
sms_corpus_clean <- tm_map(sms_corpus,
                           content_transformer(tolower))
#check result
as.character(sms_corpus[[1]])
as.character(sms_corpus_clean[[1]])
##removing numbers from the SMS messages
sms_corpus_clean <- tm_map(sms_corpus_clean, removeNumbers)
##remove filler words such as to, and, but, and or from our SMS messages. 
##These terms are known as stop words
sms_corpus_clean <- tm_map(sms_corpus_clean,
                           removeWords, stopwords())
##eliminate any punctuation from the text messages
sms_corpus_clean <- tm_map(sms_corpus_clean, removePunctuation)
## stemming. The stemming process takes words like learned,
## learning, and learns, and strips the sufix in order to transform them into the base
## form, learn. This allows machine learning algorithms to treat the related terms as a
## single concept
library(SnowballC)
sms_corpus_clean <- tm_map(sms_corpus_clean, stemDocument)
##remove additional whitespace
sms_corpus_clean <- tm_map(sms_corpus_clean, stripWhitespace)
##Creating a DTM sparse matrix
sms_dtm <- DocumentTermMatrix(sms_corpus_clean)
#In case we have not done the preprocessing before
sms_dtm2 <- DocumentTermMatrix(sms_corpus, control = list(
  tolower = TRUE,
  removeNumbers = TRUE,
  stopwords = TRUE,
  removePunctuation = TRUE,
  stemming = TRUE
))
###### WE HAVE 2 SLIGHTLY DIFFERENT RESULTS DUE TO THE ORDER OF DATA PROCESSING 

## Creating training and test sets
sms_dtm_train <- sms_dtm[1:4169, ]
sms_dtm_test <- sms_dtm[4170:5559, ]
sms_train_labels <- sms_raw[1:4169, ]$type
sms_test_labels <- sms_raw[4170:5559, ]$type
##comparing the proportions in training and test sets
prop.table(table(sms_train_labels))
prop.table(table(sms_test_labels))
## Visualizing text data - word clouds
library(wordcloud) 
wordcloud(sms_corpus_clean, min.freq =50, random.order = FALSE)
##compare visually the ham and spam clouds
spam <- subset(sms_raw, type == "spam")
ham <- subset(sms_raw, type == "ham")
wordcloud(spam$text, max.words = 40, scale = c(3, 0.5))
wordcloud(ham$text, max.words = 40, scale = c(3, 0.5))
##Data preparation - creating indicator features for frequent words
sms_freq_words <-findFreqTerms(sms_dtm_train, 5)
##removing unfrequent features
sms_dtm_freq_train<- sms_dtm_train[ , sms_freq_words]
sms_dtm_freq_test <- sms_dtm_test[ , sms_freq_words]
##Convert the sparse matrix variables to categorical
convert_counts <- function(x) {
  x <- ifelse(x > 0, "Yes", "No")
}
sms_train <- apply(sms_dtm_freq_train, MARGIN = 2,
                   convert_counts)
sms_test <- apply(sms_dtm_freq_test, MARGIN = 2,
                    convert_counts)
##training a model on the data
library(e1071)
sms_classifier <- naiveBayes(sms_train, sms_train_labels)
##evaluating model performance
sms_test_pred <- predict(sms_classifier, sms_test)
library(gmodels)
CrossTable(sms_test_pred, sms_test_labels,
             prop.chisq = FALSE, prop.t = FALSE,
             dnn = c('predicted', 'actual'))
##improving model performance ( using Laplace estimator )
sms_classifier2 <- naiveBayes(sms_train, sms_train_labels,
                              laplace = 1)
sms_test_pred2 <- predict(sms_classifier2, sms_test)
CrossTable(sms_test_pred2, sms_test_labels,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))

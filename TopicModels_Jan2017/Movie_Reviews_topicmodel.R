###################################################
##
## Topic modeling of movie reviews
##
## Janae Nicholson, 2015
## janae.nicholson@cerner.com
##
###################################################

library(shiny)
library(tm); library(SnowballC)
library(topicmodels)
library(LDAvis)
#devtools::install_github("cpsievert/LDAvisData")
library(LDAvisData)
library(dplyr)
library(ggplot2)

#data(reviews, package = "LDAvisData")

#Preprocess the text and convert to document-term matrix
dtm.control <- list(
  tolower = TRUE,
  removePunctuation = TRUE,
  removeNumbers = TRUE,
  stopwords = c(stopwords("english"), "will", "can", "also", "one", "two", "get", "use", "thing"),
  stemming = TRUE,
  wordLengths = c(3, Inf),
  weighting = weightTf,
  stripWhitespace = TRUE
)

my_reviews <- reviews
names(my_reviews) <- NULL

#use tm package to create a corpus and document term matrix
corpus <- VCorpus(VectorSource(reviews))
dtm <- DocumentTermMatrix(corpus, control=dtm.control)
dtms <- removeSparseTerms(dtm,0.97)
#remove documents that are now empty
rowTotals <- apply(dtms, 1, sum)
dtms <- dtms[rowTotals > 0, ]
dtms
dim(dtms)
sum(rowTotals)

#set up data for a plot
u.m <- as.matrix(dtms)
dim(u.m)
u.v <- sort(colSums(u.m), decreasing=TRUE)
u.d <- data.frame(word=names(u.v), freq=u.v)
u.d$word <- reorder(u.d$word, u.d$freq)

#plot the most used words
ggplot(u.d[1:30,], aes(x=word, freq)) + geom_point(size=4, colour="red")+coord_flip() +
  ggtitle("Top 30 Words in Documents") +
  ylab("Frequency") +
  theme(axis.text.x=element_text(size=13,face="bold", colour="black"), 
        axis.text.y=element_text(size=13,colour="black", face="bold"), 
        axis.title.x=element_text(size=14, face="bold"), axis.title.y=element_text(size=14,face="bold"),
        plot.title=element_text(size=24,face="bold"))

#model parameters
my.k <- 20
alpha <- 0.02
#parameters for Gibbs Sampling
burnin <- 10000
iter <- 1000
keep <- 100

#set a seed because Gibbs Sampling starts at a random point and itterates to a solution for the liklihood
set.seed(3456)
# system.time(
#   model_lda2 <- LDA(dtms, my.k, method = "Gibbs", control = list(alpha = alpha, burnin = burnin, iter = iter, keep = keep))
# )
# str(model_lda2)
# saveRDS(model_lda2, file = "data/Review_LDA_20_1_14_17.rds")
model_lda2 <- readRDS("data/Review_LDA_20_1_14_17.rds")
tt <- terms(model_lda2, 5)
tt

#see if the algorithm converged
MC <- data.frame(itteration = seq(keep, burnin+iter, by = keep), logLiks = model_lda2@logLiks)
ggplot(MC, aes(itteration, logLiks)) + geom_line() + ggtitle("Gibbs Sampler Evolution") +
  theme(plot.title=element_text(size=24,face="bold"),
        axis.title.x=element_text(size=14, face="bold"), 
        axis.title.y=element_text(size=14,face="bold"))

#function to take a fitted LDA model from topicmodels and create the json object need for LDAvis
ldavis_topicmodels_json <- function(fitted.model){
  #phi is the k X W matrix of word probabilities for each topic
  phi <- posterior(fitted.model)$terms %>% as.matrix
  #theta is the D X k matrix of the pmf over the k topics in each document
  theta <- posterior(fitted.model)$topics %>% as.matrix
  #the vocabulary or words in the corpus
  vocab <- colnames(phi)
  #find the number of words in each document of the document term matrix
  #the i in wordassignments is the document of the corpus the word belongs to
  token.id <- fitted.model@wordassignments$i  
  doc_length <- as.numeric(table(token.id))
  
  #find the word frequencies in the corpus
  #the j in wordassignments is the word.id in the list
  word.id <- fitted.model@wordassignments$j
  term.frequency <- as.numeric(table(word.id))
  
  json_lda <- LDAvis::createJSON(phi = phi, theta = theta,
                                 vocab = vocab,
                                 doc.length = doc_length,
                                 term.frequency = term.frequency)
  return(json_lda)
}

json.obj <- ldavis_topicmodels_json(model_lda2)

#run the shiny app locally
serVis(json.obj)

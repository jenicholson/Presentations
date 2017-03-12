###################################################
##
## Run Topic Model Vis of movie reviews
##
## Janae Nicholson, 2015
## janae.nicholson@cerner.com
##
###################################################

library(shiny)
library(tm); library(SnowballC)
library(topicmodels)
library(LDAvis)
library(LDAvisData)
library(dplyr)
library(ggplot2)

model_lda2 <- readRDS("data/Review_LDA_20.rds")

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

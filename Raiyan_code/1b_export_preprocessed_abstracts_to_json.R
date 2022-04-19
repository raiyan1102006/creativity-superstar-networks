mytextProcessor <- function(documents, metadata=NULL, 
                          lowercase=TRUE, removestopwords=TRUE, removenumbers=TRUE, 
                          removepunctuation=TRUE, ucp=FALSE, 
                          stem=TRUE, wordLengths=c(3,Inf),sparselevel=1, language="en",
                          verbose=TRUE, onlycharacter=FALSE,striphtml=FALSE,
                          customstopwords=NULL, custompunctuation=NULL, v1=FALSE) {
  if(!requireNamespace("tm",quietly=TRUE)) stop("Please install tm package to use this function. You will also need SnowballC if stemming.")
  if(!(utils::packageVersion("tm")>=0.6)) stop("Please install at least version 0.6 of the tm package.")
  if(stem) {
    if(!requireNamespace("SnowballC", quietly=TRUE)) stop("Please install SnowballC to use stemming.")
  }
  
  documents <- as.character(documents)
  
  if(striphtml){
    documents <- gsub('<.+?>', ' ', documents)
  }
  
  #remove non-visible characters
  documents <- stringr::str_replace_all(documents,"[^[:graph:]]", " ")
  
  if(onlycharacter){
    documents <- gsub("[^[:alnum:]///' ]", " ", documents)
  }
  
  if(verbose) cat("Building corpus... \n")
  txt <- tm::VCorpus(tm::VectorSource(documents), readerControl=list(language= language))
  #Apply filters
  txt <- tm::tm_map(txt, tm::stripWhitespace)
  
  if(lowercase){
    if(verbose) cat("Converting to Lower Case... \n")
    #Convert to Lower case
    #(Note that this is slightly more complicated due to API change in tm)
    if(utils::packageVersion("tm") >= "0.6") {
      txt <- tm::tm_map(txt, tm::content_transformer(tolower)) 
    } else {
      txt <- tm::tm_map(txt, tolower)
    }
  }
  
  if(!v1) {
    if(removepunctuation){
      if(verbose) cat("Removing punctuation... \n")
      txt <- tm::tm_map(txt, tm::removePunctuation, preserve_intra_word_dashes = TRUE,ucp=ucp) #Remove punctuation
    }
    if(!is.null(custompunctuation)) {
      if(verbose) cat("Removing custom punctuation... \n")
      
      if(length(custompunctuation)==1 && 
         substr(custompunctuation,0,1)=="[") {
        #if there is only one entry and it starts with open bracket
        #we are going to assume its a regular expression and let it
        #through
        punct_pattern <- custompunctuation
      } else {
        punct_pattern <-sprintf("[%s]",paste0(custompunctuation,collapse=""))
      }
      txt<- tm::tm_map(txt, tm::content_transformer(function(x, pattern) gsub(pattern, "", x)), 
                       punct_pattern)
    }
  }
  if(removestopwords){
    if(verbose) cat("Removing stopwords... \n")
    txt <- tm::tm_map(txt, tm::removeWords, tm::stopwords(language)) #Remove stopwords
  }
  if(!is.null(customstopwords)) {
    if(verbose) cat("Remove Custom Stopwords...\n")
    txt <- tm::tm_map(txt, tm::removeWords, customstopwords)
  }
  if(removenumbers){
    if(verbose) cat("Removing numbers... \n")
    txt <- tm::tm_map(txt, tm::removeNumbers) #Remove numbers
  }
  
  if(v1) {
    #return to the v1 style of removing punctuation right before stemming
    if(removepunctuation){
      if(verbose) cat("Removing punctuation... \n")
      txt <- tm::tm_map(txt, tm::removePunctuation, preserve_intra_word_dashes = TRUE,ucp=ucp) #Remove punctuation
    }
    if(!is.null(custompunctuation)) {
      if(verbose) cat("Removing custom punctuation... \n")
      
      if(length(custompunctuation)==1 && 
         substr(custompunctuation,0,1)=="[") {
        #if there is only one entry and it starts with open bracket
        #we are going to assume its a regular expression and let it
        #through
        punct_pattern <- custompunctuation
      } else {
        punct_pattern <-sprintf("[%s]",paste0(custompunctuation,collapse=""))
      }
      txt<- tm::tm_map(txt, tm::content_transformer(function(x, pattern) gsub(pattern, "", x)), 
                       punct_pattern)
    }
  }
  
  if(stem){
    if(verbose) cat("Stemming... \n")
    txt <- tm::tm_map(txt, tm::stemDocument, language=language)
  }
  
  if(!is.null(metadata)) {
    for(i in 1:ncol(metadata)) {
      NLP::meta(txt, colnames(metadata)[i]) <- metadata[,i]
    }
  }
  return(txt)
}


#####################

require("stm")
require("parallel")
require(gtools)
require(stringr)


# Load data
data <- read.csv("data/all_abstracts_year.csv")

# preprocess for export
processed_my <- mytextProcessor(data$abstract, metadata = data)

# prepare for export
mylist <- list()
for (adoc in processed_my$content){
  mylist <- append(mylist,adoc['content']$content)
}

# save in python readable JSON format
library(jsonlite)
l2 = toJSON(mylist, pretty = TRUE, auto_unbox = TRUE)
write(l2, "data/processed_abstracts.json")

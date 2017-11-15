#####################################################################################
### Robert Schnitman
### 2017-11-14
###
###
### PURPOSE: Convert raw text into data frame with word frequencies.
### LIBRARY DEPENDENCY: tm (>= v.0.7.).
###
### RECOMMENDED CITATION:
###  Schnitman, Robert (2017). wfreqdf.r. https://github.com/robertschnitman/schnitr
#####################################################################################

##### === BEGIN === #####

library(tm)

#### clean_corpus() - to be used in wfreqdf ####

clean_corpus <- function(corpus) {
  bad_leftovers <- c('the', 'this', 'but', 
                     'and', 'that', 'with', 
                     'for')                       # leftover words not removed normally from tm_map()
  
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, stopwords('english'))
  corpus <- tm_map(corpus, removeWords, bad_leftovers)
  
  corpus
}


#### wfreqdf() - Convert corpus into data frame of word frequencies. ####

wfreqdf <- function(filename) {                              # filename should be a txt file.
  bad_leftovers <- c('the', 'this', 'but', 
                     'and', 'that', 'with', 
                     'for')                                  # leftover words not removed normally from tm_map()
  
  text    <- readLines(filename)                             # Get text file,
  corpus  <- Corpus(VectorSource(text))                      # Convert vectorized text into a corpus.
  ccorpus <- clean_corpus(corpus)                            # Filter out useless words.
  tdmtext <- TermDocumentMatrix(corpus)                      # Convert corpus to Term Document Matrix.
  mtext   <- as.matrix(tdmtext)                              # Convert TDM to normal matrix.
  vtext   <- sort(rowSums(mtext), decreasing = TRUE)         # vector of words with frequency count.
  dtext   <- data.frame(word = names(vtext), freq = vtext)   # Dataframe of vtext.
  
  ### Filter out bad_leftovers ####  
  d2text <- subset(dtext, !word %in% bad_leftovers)
  
  ### Print dataframe - descending order ###
  row.names(d2text) <- NULL
  d2text                      # should already be in descending order.
  
}

##### === END === #####

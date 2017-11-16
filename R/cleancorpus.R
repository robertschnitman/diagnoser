#' Clean corpus by removing punctuation, stripping whitespace, and others.
#'
#' @param corpus A corpus document.
#' @param stopwords Specification for stopwords.
#' @return A corpus.
#' @examples
#' library(tm)
#' cleancorpus(my_corpus, 'english')
#' @seealso \url{https://github.com/robertschnitman/schnitr}


#####################################################################################
### Robert Schnitman
### 2017-11-14
###
###
### PURPOSE: Remove punctuation, stip whitespace,
###   stopwords, and others from corpus.
### LIBRARY DEPENDENCY: tm (>= v.0.7.).
###
### RECOMMENDED CITATION:
###  Schnitman, Robert (2017). wfreqdf.r. https://github.com/robertschnitman/schnitr
#####################################################################################

### 2017-11-14: Added stopwords argument, but I am only confident that these functions work for English only. ( ' ^ ')

##### === BEGIN === #####

#### cleancorpus() - to be used in wfreqdf ####

cleancorpus <- function(corpus, stopwords) {
  badleftovers <- c('the', 'this', 'but', 'about',
                     'and', 'that', 'with',
                     'for')                       # leftover words not removed normally from tm_map()

  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, stopwords(stopwords))
  corpus <- tm_map(corpus, removeWords, badleftovers)

  corpus
}

##### === END === #####

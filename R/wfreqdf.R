#' Convert raw text into a word-frequency data frame.
#'
#' @param filename A raw text document, preferably in a .txt file.
#' @param stopwords Specification for stopwords.
#' @return A data frame.
#' @examples
#' library(tm)
#' wfreqdf('nonsense.txt', 'english')
#' @seealso \url{https://github.com/robertschnitman/schnitr}


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

### 2017-11-14: Added stopwords argument, but I am only confident that these functions work for English only. ( ' ^ ')

##### === BEGIN === #####

#### wfreqdf() - Convert corpus into data frame of word frequencies. ####

wfreqdf <- function(filename, stopwords) {                   # filename should be a txt file.
  badleftovers <- c('the', 'this', 'but', 'about',
                     'and', 'that', 'with',
                     'for')                                  # leftover words not removed normally from tm_map()

  text    <- readLines(filename)                             # Get text file,
  corpus  <- Corpus(VectorSource(text))                      # Convert vectorized text into a corpus.
  ccorpus <- cleancorpus(corpus, stopwords)                 # Filter out useless words.
  tdmtext <- TermDocumentMatrix(corpus)                      # Convert corpus to Term Document Matrix.
  mtext   <- as.matrix(tdmtext)                              # Convert TDM to normal matrix.
  vtext   <- sort(rowSums(mtext), decreasing = TRUE)         # vector of words with frequency count.
  dtext   <- data.frame(word = names(vtext), freq = vtext)   # Dataframe of vtext.

  ### Filter out badleftovers ####
  d2text <- subset(dtext, !word %in% badleftovers)

  ### Print dataframe - descending order ###
  row.names(d2text) <- NULL
  d2text                      # should already be in descending order.

}


##### === END === #####

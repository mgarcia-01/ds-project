textdata <- setRefClass("textdata",
                        fields = list(filepath="character", linecount="ANY", charcount="ANY", wordratio="ANY", wordsearchres="ANY", statementcount="ANY", dfwrangle="ANY", wordplot="ANY"), 
                        methods = list(
                          initialize = function(filepath) {
                            .self$filepath <- filepath
                            .self$linecount <- .self$lineCounter()
                            .self$charcount <- .self$charCounter(countbreak = 0)
                            .self$wordratio <- .self$wordDiv()
                            .self$wordsearchres <- .self$wordSearch()
                            .self$statementcount <- .self$statementCount()
                            .self$dfwrangle  <- .self$datawrangle()
                            .self$wordplot <- .self$wordPlot()
                          },
                          
                          lineCounter = function(.self){
                            f <- file(.self$filepath, open="rb")
                            nlines <- 0L
                            while (length(chunk <- readBin(f, "raw", 65536)) > 0){
                              nlines <- nlines + sum(chunk == as.raw(10L))
                            }
                            close(f)
                            sprintf("The file has %s lines.", nlines)
                            return(nlines)
                          }
                          ,
                          charCounter = function(.self,countbreak=NULL){
                            if (is.null(countbreak)==TRUE){
                              return(NULL)
                            } else {
                              counter = 0
                              charct = 0L
                              for (line in readLines(.self$filepath)){
                                #print(line)
                                current <-  nchar(line)
                                if (current > charct){
                                  charct = current
                                }
                                counter = counter + 1
                                if (counter == countbreak & countbreak !=0){
                                  break
                                } 
                              }
                              .self$charcount = charct
                              return(charct)
                            }
                          }
                          ,
                          wordDiv = function(.self,word1=NULL,word2=NULL){
                            if (is.null(word1)==TRUE || is.null(word2)==TRUE){
                              return(NULL)
                            } else {
                              f <- file(.self$filepath, open="rb")
                              filelines <- readLines(f)
                              div <- length(grep(word1, filelines))/length(grep(word2, filelines))
                              #print(div)
                              close(f)
                              sprintf("We get %s",div)
                              .self$wordratio = div
                              return(div)
                            }
                          }
                          ,
                          wordSearch = function(.self,word=NULL){
                            if (is.null(word)==TRUE){
                              return(NULL)
                            } else {
                              f <- file(.self$filepath, open="rb")
                              filelines <- readLines(f)
                              res <- grep(word, filelines, value = T)
                              close(f)
                              print(res)
                              .self$wordsearchres = res
                              return(res)
                            }
                          }
                          ,
                          statementCount = function(.self,statement=NULL){
                            if (is.null(statement)==TRUE){
                              return(NULL)
                            } else{
                              f <- file(.self$filepath, open="rb")
                              filelines <- readLines(f)
                              lns <- grep(statement, filelines)
                              close(f)
                              sprintf("Frequency of statement %s", length(lns))
                              .self$statementcount = lns
                              return(lns)
                            }
                          }
                          ,
                          datawrangle = function(.self){
                            strfile <- readLines(.self$filepath)
                            df_source <- tibble(line = 1:length(strfile), text = strfile)
                           
                            df_source_unnest <- df_source %>% 
                                   unnest_tokens(word, text) %>% 
                                   mutate(source = toString(.self$filepath))
                            
                            #Clean out numbers
                            df_source_unnest <- df_source_unnest %>% filter(!grepl("[0-9]", word))
                            df <- df_source_unnest
                            
                            df <- df %>% anti_join(stop_words, by = "word")
                            return(df)
                          }
                          ,
                          wordPlot = function(.self){
                            df <- .self$dfwrangle
                            df_wordcount <- df %>% count(word, sort = TRUE)     
                            df_wordcount %>% head(n = 20) %>% mutate(word = reorder(word, n)) %>% 
                                   
                                   ggplot(aes(x = n, y = word)) +
                                   geom_col(fill = "#00abff") +
                                   theme_bw() +
                                   ggtitle("Most frequently used words in the English language")
                          }
                        )
                        )

combineTibbles <- function(tib1,tib2,tib3){
    df <- rbind(tib1, tib2, tib3)
}



zipdownloader <- function(data_url,zip_file){
    if (!file.exists('data')) {
      dir.create('data')
      }
  
    if (!file.exists("data/final/en_US")) {
        tempFile <- tempfile()
        download.file(data_url, tempFile)
        unzip(tempFile, exdir = "data")
        unlink(tempFile)
        }
}

txtdownloader <- function(data_url,text_file){
    if (!file.exists('data')) {
      dir.create('data')
      }
  
    if (!file.exists(text_file)) {
        #tempFile <- tempfile()
        download.file(data_url, text_file)
        #unzip(tempFile, exdir = "data")
        #unlink(tempFile)
        }
}
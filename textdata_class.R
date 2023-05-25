textdata <- setRefClass("textdata",
                        fields = list(filepath="character", linecount="ANY", charcount="ANY", wordratio="ANY", wordsearchres="ANY", statementcount="ANY"), 
                        methods = list(
                          initialize = function(filepath) {
                            .self$filepath <- filepath
                            .self$linecount <- .self$lineCounter()
                            .self$charcount <- .self$charCounter()
                            .self$wordratio <- .self$wordDiv()
                            .self$wordsearchres <- .self$wordSearch()
                            .self$statementcount <- .self$statementCount()
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
                          charCounter = function(.self,countbreak){
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
                            return(charct)
                          }
                          ,
                          wordDiv = function(.self,word1,word2){
                            try
                            f <- file(.self$filepath, open="rb")
                            filelines <- readLines(f)
                            div <- length(grep(word1, filelines))/length(grep(word2, filelines))
                            #print(div)
                            close(f)
                            sprintf("We get %s",div)
                            return(div)
                          }
                          ,
                          wordSearch = function(.self,word){
                            f <- file(.self$filepath, open="rb")
                            filelines <- readLines(f)
                            res <- grep(word, filelines, value = T)
                            close(f)
                            print(res)
                            return(res)
                          }
                          ,
                          statementCount = function(.self,statement){
                            f <- file(.self$filepath, open="rb")
                            filelines <- readLines(f)
                            lns <- grep(statement, filelines)
                            close(f)
                            sprintf("Frequency of statement %s", length(lns))
                            return(lns)
                            #print(lns)
                          }
                        )
)
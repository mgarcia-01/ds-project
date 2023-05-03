data_url <-  "https://www.coursera.org/learn/data-science-project/supplement/Iimbd/task-0-understanding-the-problem#:~:text=websites%20to%20start.-,Capstone%20Dataset,-Your%20original%20exploration"
#dest <- file.path(getwd(),paste("file_data",".zip",sep = ""))
#dest <- file.path(getwd(),paste("file_data"))
#temp <- tempfile()
download.file(data_url,dest)

#unzip("file_data.zip",files = "./")
unzip("Coursera-SwiftKey.zip")
#unlink(temp)



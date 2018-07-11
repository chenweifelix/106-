source('pttTestFunction.R')
id = c(1110:1118)
URL = paste0("https://www.ptt.cc/bbs/TOEIC/index", id, ".html")
filename = paste0(id, ".txt")
pttTestFunction(URL[1], filename[1])
mapply(pttTestFunction, 
       URL = URL, filename = filename)

install.packages("jiebaR")

library("jiebaR")
Sys.setlocale(category = "LC_ALL", locale = "cht")
cc = worker()
words = cc["Voices_in_My_Head.txt"]
table(words)
mlb <-table(words)
mlb

install.packages("wordcloud")
library(wordcloud)

mixseg = worker()
jieba_tokenizer=function(d){
  unlist(segment(d[[1]],mixseg))
}
seg = lapply("Voices_in_My_Head.txt", jieba_tokenizer)
freqFrame = as.data.frame(table(unlist(seg)))
freqFrame = freqFrame[-c(1:34),]
wordcloud(freqFrame$Var1,freqFrame$Freq,
          scale=c(5,0.5),min.freq=10,max.words=50,
          random.order=FALSE, random.color=TRUE, 
          rot.per=0, colors=brewer.pal(8, "Dark2"),
          ordered.colors=FALSE,use.r.layout=FALSE,
          fixed.asp=TRUE)


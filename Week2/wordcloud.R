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
text = readLines("Voices_in_My_Head.txt")
words = cc[text]
words

library(tm)
install.packages("tm")
docs <- Corpus(VectorSource(words)) 
docs
docs <- tm_map(docs, toSpace, "的")
docs <- tm_map(docs, toSpace, "我")
docs <- tm_map(docs, toSpace, "了")
docs <- tm_map(docs, toSpace, "和")
docs <- tm_map(docs, toSpace, "去")
docs <- tm_map(docs, toSpace, "而")
docs <- tm_map(docs, toSpace, "也")
docs <- tm_map(docs, toSpace, "們")
docs <- tm_map(docs, toSpace, "又")
docs <- tm_map(docs, toSpace, "是")



toSpace <- content_transformer(function(x, pattern) {
  return (gsub(pattern, " ", x))}
)




install.packages("wordcloud")
library(wordcloud)

mixseg = worker()
jieba_tokenizer=function(d){
  unlist(segment(d[[1]],mixseg))
}
seg = lapply(docs, jieba_tokenizer)
freqFrame = as.data.frame(table(unlist(seg)))
freqFrame = freqFrame[-c(1:33),]
wordcloud(freqFrame$Var1,freqFrame$Freq,
          scale=c(5,0.5),min.freq=1,max.words=65,
          random.order=FALSE, random.color=TRUE, 
          rot.per=0, colors=brewer.pal(8, "Dark2"),
          ordered.colors=FALSE,use.r.layout=FALSE,
          fixed.asp=TRUE)


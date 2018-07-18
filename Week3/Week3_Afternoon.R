library(bitops)
library(httr)

library(RCurl)
install.packages("XML")
library(XML)
library(tm)

library(NLP)
library(tmcn)
library(jiebaRD)
library(jiebaR)

from <- 1
to <- 10 
prefix = "https://www.psychologytoday.com/"


data <- list()

  url  <- ("https://www.reddit.com/r/psychologystudents/")
  html <- htmlParse( GET(url) )
  url.list <- xpathSApply( html, "//div[@class='title']/a[@href]", xmlAttrs )
  data <- rbind( data, as.matrix('https://www.reddit.com/r/psychologystudents/', url.list, sep='') )
data <- unlist(data)

head(data)

######

library(dplyr)
getdoc <- function(url)
{
  html <- htmlParse( getURL(url) )
  doc  <- xpathSApply( html, "//p", xmlValue )
  
  #print(hour)
  name <- "psy.txt"
  write(doc, name, append = TRUE)
}

sapply(data, getdoc)

###
d.corpus <- Corpus( DirSource("./Data") )
d.corpus <- tm_map(d.corpus, removePunctuation)
d.corpus <- tm_map(d.corpus, removeNumbers)
d.corpus <- tm_map(d.corpus, function(word) {
  gsub("[A-Za-z0-9]", "", word)
})


###

mixseg = worker()
jieba_tokenizer = function(d)
{
  unlist( segment(d[[1]], mixseg) )
}
seg = lapply(d.corpus, jieba_tokenizer)

count_token = function(d)
{
  as.data.frame(table(d))
}
tokens = lapply(seg, count_token)

n = length(seg)
TDM = tokens[[1]]
colNames <- names(seg)
colNames <- gsub(".txt", "", colNames)
for( id in c(2:n) )
{
  TDM = merge(TDM, tokens[[id]], by="d", all = TRUE)
  names(TDM) = c('d', colNames[1:id])
}
TDM[is.na(TDM)] <- 0
library(knitr)
kable(head(TDM))

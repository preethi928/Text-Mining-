setwd('/Data_science/EXCLER/My Assignments/Text Mining/amazon_reviews')
library(rvest)
library(XML)
library(xml2)
library(magrittr)

#Amazon Reviews

# extracting oneplus 7t Reviews.

      aurl<-"https://www.amazon.in/Test-Exclusive-748/product-reviews/B07DJLVJ5M/ref=cm_cr_arp_d_paging_btm_next_2?ie=UTF8&reviewerType=all_reviews&pageNumber"
      amazon_reviews<-NULL
      for(i in 1:10){
        murl <- read_html(as.character(paste(aurl,i,sep="=")))
        rev <- murl %>% html_nodes(".review-text") %>% html_text()
        amazon_reviews <- c(amazon_reviews,rev)
      }

length(amazon_reviews)            
write.table(amazon_reviews,"onePlus7T.txt",row.names = F)
getwd()

#still now it was extraction of reviews.

#-------------------Text Mining----------------------------------------------

library('tm')#text mining
library("SnowballC")#for stemming
library("wordcloud")#word cloud generator 
library("RColorBrewer")#color palettes
library('textstem')

x<- as.character(amazon_reviews)

x<-iconv(x,"UTF-8")

#lets put all the data into one called as CORPUS

x<-Corpus(VectorSource(x))
inspect(x[1])

#convert all the text to lower 

x1<-tm_map(x,tolower)
inspect(x[1])

#remove Numbers
x1<-tm_map(x,removeNumbers)

#Remove Punctuations
x1<-tm_map(x,removePunctuation)

#remove english common stop words 
x1<-tm_map(x,removeWords,stopwords('english'))

#now lets remove our own stop words which are there in the data 
x1<-tm_map(x,removeWords,c("7t","will","the"))
#by removing this stop words it would leave empty spaces there ,hence white space should
#be stripped

x1<-tm_map(x,stripWhitespace)
inspect(x[1])

#text stemming
x1<-lemmatize_words(x1)

#TEXT DOCUMENT MATRIX
#converting unstructured data to structured format using TDM
tdm<-TermDocumentMatrix(x1)
tdm<-as.matrix(tdm)
tdm 

#Frequency
v<-sort(rowSums(tdm),decreasing = TRUE)
d<-data.frame(word = names(v),freq=v)
head(d,10)

#bar plot
w<-rowSums(tdm)
w_subs<-subset(w,w>=10)
barplot(w_subs,las=3,col=rainbow(20))

#after seeing the barplot remove highly not needed revews
x1<-tm_map(x1,removeWords,c('the','and'))
x1<-tm_map(x1,stripWhitespace)
tdm<-TermDocumentMatrix(x1)
tdm<-as.matrix(tdm)
w1<-rowSums(tdm)

#word cloud
#with all the words
wordcloud(words = names(w1),freq = w1,
          random.order = F,colors = rainbow(20),
          scale = c(2,.4),rot.per = 0.3)

#+ve and -ve word list

pos.words = scan(file.choose(), what="character", comment.char=";")	# read-in positive-words.txt
neg.words = scan(file.choose(), what="character", comment.char=";") 	# read-in negative-words.txt
pos.words = c(pos.words,"wow", "kudos", "hurray") # including our own positive words to the existing list

# Positive wordcloud
pos.matches = match(names(w), c(pos.words))
pos.matches = !is.na(pos.matches)
freq_pos <- w[pos.matches]
p_names <- names(freq_pos)

wordcloud(p_names,freq_pos,scale=c(3.5,.5),colors = rainbow(20))

# Negative wordcloud
neg.matches = match(names(w), c(neg.words))
neg.matches = !is.na(neg.matches)
freq_neg <- w[neg.matches]
n_names <- names(freq_neg)
wordcloud(n_names,freq_neg,scale=c(3.5,.5),colors = brewer.pal(8,"Dark2"))

#Association between words
tdm <- TermDocumentMatrix(x1)
findAssocs(tdm, c("issue"),corlimit = 0.3)

#text mining is all abput gaining insights about the data (what are the issues customer is facing)
#what satisfied customer.
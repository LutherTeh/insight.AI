
##trigram input must be a vector of text/character, for example twitter text 

trigram<- function(string){
  
  require(RWeka)
  require(tm)
  require(wordcloud)
  require(RColorBrewer)
  
  # tutorial on rweka - http://tm.r-forge.r-project.org/faq.html
  
  corpus <- Corpus(VectorSource(string)) # create corpus for TM processing
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, PlainTextDocument)
  corpus <- tm_map(corpus, removeNumbers) 
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeWords, stopwords("english"))
  
  options(mc.cores=1) # http://stackoverflow.com/questions/17703553/bigrams-instead-of-single-words-in-termdocument-matrix-using-r-and-rweka/20251039#20251039
  BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3)) # create n-grams
  tdm <- TermDocumentMatrix(corpus, control = list(tokenize = BigramTokenizer)) # create tdm from n-grams
  
  ##remove sparse term
  tdm <- removeSparseTerms(tdm,0.99) 
  
  freq <- sort(rowSums(as.matrix(tdm)), decreasing=TRUE)  
  
  #create dataframe
  df <- data.frame(word=names(freq), freq=freq) 
  
  # #plot the word cloud
  dark2 <- brewer.pal(6, "Dark2")   
  wordcloud(df$word, df$freq, scale=c(6,.2),rot.per = .15,  max.words =30 , colors=dark2, random.order=FALSE)   
  
  
  #   # #subset the top 20 word bigram
  #   topfreq <- min(head(df$freq,20))
  #   df <- subset(df, freq>topfreq)
  #   
  #   g <- ggplot(df,aes(x=word,y=freq,fill=freq))
  #   g <- g +  geom_bar(stat="identity") 
  #   g <- g + labs(title="Most Frequent used word")
  #   g <- g + theme(axis.text.x=element_text(angle=45, hjust=1))   
  return(df)
}
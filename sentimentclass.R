#INSIGHT.AI sentimental score which return a dataframe with sentimental score & class column


sentimentclass <- function(data) {

require(ggplot2)
require(plotly)
require(lubridate)


data$createdMsia<-ymd_hms(data$createdMsia)
data$scores <- NA
data$class <- NA

#apply score sentiment function return sentimental score 


##sentiment score function===========================================

score.sentiment = function(sentences, pos.words, neg.words)
  # score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
    
{
  require(plyr)
  require(stringr)
  
  # we got a vector of sentences. plyr will handle a list
  # or a vector as an "l" for us
  # we want a simple array of scores back, so we use
  # "l" + "a" + "ply" = "laply":
  scores = laply(sentences, function(sentence, pos.words, neg.words){ 
  
    
    # clean up sentences with R's regex-driven global substitute, gsub():
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    # and convert to lower case:
    sentence = tolower(sentence)
    
    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+')
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)
    return(score)
    
  } ,pos.words,neg.words)

}

## calculate the sentiment score & class==========================================

data$scores <- score.sentiment(data$text, pos.words, 
                              neg.words)

data$class <- ifelse(data$score > 0, 'positive', ifelse(data$score < 0, 'negative', 'neutral'))
# 
# g <- ggplot(data,aes(x=createdMsia,y=scores, color=class)) 
# g <- g + geom_point() +geom_line()
# g <- g + scale_x_datetime() 
# g <- g + labs(title="MAXIS sentiment count")

df <- data

return(df)
##stacked barplot on sentiment (+ve,neutral,-ve) versus date========================

data$date <- as.Date(data$createdMsia)
data <- as.data.frame(table(data$date, data$class))
colnames(data) <- c("date", "class","count")
data$date <- as.Date(data$date)

# g <- ggplot(data,aes(x=date,y=count,fill=class)) + geom_bar(position='dodge',stat="identity")
# g <- g + scale_x_date(date_labels = "%b %d") 
# # g <- g + labs(title="MAXIS sentiment count")
# g

# return(data)

}

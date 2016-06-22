

#INSIGHT.AI user demographic


usergroup <- function(data) {
  
  set.seed(12345)
  
  
  require(ggplot2)
  require(plotly)
  
  
  data$gender <- NA
  data$age <- NA
  
  data$age <- runif(nrow(data),16,60)
  data$gender <- sample(0:1)
  data$gender <- ifelse(data$gender== 0, 'male', "female")
  
  data$agegroup<-cut(data$age, c(14,24,34,44,54,64))
  
  
  ## calculate the sentiment score & class==========================================
  
  
  ##stacked barplot on sentiment (+ve,neutral,-ve) versus date========================
  
  data2 <- as.data.frame(table(data$agegroup, data$gender))
  colnames(data2) <- c("agegroup", "gender","count")
  
  g <- ggplot(data2,aes(x=agegroup,y=count,fill=gender)) +theme(legend.position ="bottom",legend.direction = "horizontal")
  g <- g + geom_bar(position='dodge',stat="identity")
  g <- g + labs(title="MAXIS user demographic")

  
  return(g)
  
}

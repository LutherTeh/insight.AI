

#INSIGHT.AI sentimental plot 
##input: data frame with sentimentclass output: "scores" & "class"


sentimentplot <- function(data) {
  
  require(ggplot2)
  require(plotly)
  require(lubridate)
  
 
  data$date <- as.Date(data$createdMsia)
  data <- as.data.frame(table(data$date, data$class))
  colnames(data) <- c("date", "class","count")
  data$date <- as.Date(data$date)
  
  g <- ggplot(data,aes(x=date,y=count,colour=class)) + geom_line()
    # ggplot(data,aes(x=date,y=count,fill=class)) +geom_bar(position='dodge',stat="identity")
  g <- g + scale_x_date(date_labels = "%b %d") 
  g
  
  return (g)
}

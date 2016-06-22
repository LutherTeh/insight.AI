



library(shiny)
library(ggplot2)
library(plotly)
library(plyr)
library(wordcloud)
library(leaflet)
library(DT)
library(shinythemes)
library(ggthemes)
library(shinydashboard)

source("plot_tweet.R")
source("trigram.R")
source("usergroup.R")
source("userlocation.R")
source("sentimentclass.R")
source("sentimentplot.R")


data1 <- read.csv("maxistweet_all.csv",encoding = 'UTF-8')
dat2 <-read.csv("userlocation.csv", header =TRUE)

# data2 <- read.csv("maxistweet2.csv",encoding = 'UTF-8')
# write.csv(data1, "maxistweet_all.csv", fileEncoding = "UTF-8", row.names = FALSE)


neg.words = scan('negative-words.txt', what='character', comment.char=';')
pos.words = scan('positive-words.txt', what='character', comment.char=';')

#trigram wordcloud preprocessing
w <- data1$text
dt<- trigram(w)

#sentimental score &class preprocessing
s <-sentimentclass(data1)

##dashboard header setup
header  <-dashboardHeader(
            title= "insight.AI")
              
              # tags$a(href='', tags$img(src='logo.png')))
            
#             dropdownMenu(type = "notifications",
#                          notificationItem(
#                            text = "5 new users today",
#                            icon("users")
#                          ),
#                          notificationItem(
#                            text = "10 potential sales lead",
#                            icon("area-chart")
#                          ),
#                          notificationItem(
#                            text = "13 user enquiries & support case",
#                            icon = icon("support")
#                          )
#             ))

#dashboard sidebar setup
sidebar <-dashboardSidebar(disable = TRUE)
#   sidebarMenu(
#     menuItem("Brand Monitoring", tabName = "module1", icon = icon("dashboard")),
#     menuItem("Engagement", tabName = "module2", icon=icon("th"))

#dashboard body setup
body<- dashboardBody(
          
#           tabItems(
#             tabItem(tabName = "module1",
                  fluidRow(
                      
                
                      
                      infoBox("Product/Service",width=2, "MAXIS plan", icon = icon("list")),
                      infoBox("Channel",width=2, "Twitter,FB", icon = icon("globe")),
                      infoBox("Time period",width=2, "May 2016 Jun 2016", icon = icon("calendar")),
                    
                      valueBox(33,width=3, color = "blue", "Overall Positive sentiment count", icon("smile-o")),
                      valueBox(111,width=3, color = "red","Overall Negative sentiment count", icon("frown-o")),
                  
                       box(title = "Brand mention", status = "primary", solidHeader = TRUE,
                           collapsible = TRUE, plotlyOutput("plot1")),
                       box(title = "User sentiment", status = "primary", solidHeader = TRUE,
                           collapsible = TRUE, plotlyOutput("plot3")),
                       box(title = "User demographic", status = "primary", solidHeader = TRUE,
                           collapsible = TRUE, plotlyOutput("plot2")),
                       box(title = "User location", status = "primary", solidHeader = TRUE,
                           collapsible = TRUE, leafletOutput("plot7")),
                       box(title = "Most frequent used word", status = "primary", solidHeader = TRUE,
                           collapsible = TRUE,plotOutput("plot5")),
                       box(title = "Most used Word & frequency count", status = "primary", solidHeader = TRUE,
                           collapsible = TRUE, dataTableOutput("plot6"))
#                        box(width=12, title = "User negative statement", status = "primary", solidHeader = TRUE,
#                            collapsible = TRUE, dataTableOutput("plot4"))
                           
                       ))
            
#             tabItem(tabName = "module2",    
#                     fluidRow(


ui <-dashboardPage(header,sidebar,body, skin = "black")


##server.R setup
server <- function(input, output) {
  
  output$plot1<- renderPlotly({
    p <- plot_tweet(data1)
    ggplotly(p)
    
  })
  
  
  output$plot2<- renderPlotly({
    a<- usergroup(data1)
    ggplotly(a)
    
  })
  
  output$plot3<- renderPlotly({
    g <- sentimentplot(s)
    ggplotly(g)
    
  })
  
  
#   output$plot4<- renderDataTable({
#     
#     s2 <- subset(s, s$class=="negative")
#     datatable(s2[,c("text","screenName","createdMsia","scores","class")], rownames = F,
#               options = list(autoWidth = T, pageLength = 6))
#     
#   })
  
  output$plot5<- renderPlot({
    
    trigram(w)
    
  })
  #   
  #   
  output$plot6<- renderDataTable({
    
    datatable(dt, rownames = F,
              options = list(autoWidth = T, pageLength = 6))
    
  })
  
  
  
  output$plot7<- renderLeaflet({
    
    
    leaflet(data=dat2) %>% addTiles() %>% 
      addMarkers(~lng,~lat, popup= paste("state", dat2$state),
                 clusterOptions = markerClusterOptions())
    
  })
  
} ##end of server.R

shinyApp(ui, server)

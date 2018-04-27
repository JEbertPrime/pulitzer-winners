#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

library(readr)
library(dplyr)
library(stringr)
library(lubridate)
library(tidyr)
library(tidytext)
library(quanteda)
library(ggplot2)
library(DT)
library(ggthemes)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Pulitzer Winning Features"),
   
   # Sidebar with a slider input for number of bins 
   fluidRow(
      column(8,
         wellPanel(
             selectInput("year",
                     "Year:",
                     choices = c("1993","1994","1995","1996","1997","1998","1999","2000","2001","2002","2003","2005","2006","2007","2008","2009","2010","2011","2012","2013","2015","2016","2017","2018"))
      ))),
   fluidRow(
      
      # Show a plot of the generated distribution
      column(5,
             textOutput("labels")),
      column(8,
             verbatimTextOutput("headline"))
   ),
   fluidRow(
     column(8,
            textOutput("author"))
   ),
      
   fluidRow(
     column(8,
            plotOutput("cloudPlot")
     )
   ),
   fluidRow(
     column(8,
            plotOutput("graph1"))
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  data <- read_csv("data/feature_winners_2007_2017.csv")
  data$graf <- sapply(data$graf,function(row) iconv(row, "latin1", "ASCII", sub=""))
  data$lower_graf <- tolower(data$graf)
  data <- data %>%
    mutate(year = year(date),
           syllables = nsyllable(lower_graf),
           sentences = nsentence(graf),
           words = ntoken(graf, remove_punct = TRUE),
           fk_ease = 206.835 - 1.105*(words/sentences) - 84.6*(syllables/words),
           fk_grade = 0.39*(words/sentences) + 11.8*(syllables/words) - 15.59) %>%
    arrange(date)
  summary_data <- data.frame(data$headline, data$syllables, data$sentences, data$words, data$year_won)
  summary_data <- aggregate(summary_data[2:4], list(summary_data$data.headline), FUN=sum)
  summary_data <- summary_data %>%
    mutate(fk_ease = 206.835 - 1.105*(data.words/data.sentences) - 84.6*(data.syllables/data.words),
           fk_grade = 0.39*(data.words/data.sentences) + 11.8*(data.syllables/data.words) - 15.59) 
  names(summary_data)[names(summary_data) == 'Group.1'] <- 'headline'
  summary_data <- merge(x=summary_data, data[ ,c('headline','date','year_won')], by='headline', all.x=TRUE)
  summary_data <- unique(summary_data)
  
   output$cloudPlot <- renderPlot(res=50,width = 'auto', height = 'auto',{
     text <- readLines(paste("data/txt-files/", input$year, ".txt", sep=""))
     
     docs <- Corpus(VectorSource(text))
     #Clean the text and remove stop words, super common words
     toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
     docs <- tm_map(docs, toSpace, "/")
     docs <- tm_map(docs, toSpace, "@")
     docs <- tm_map(docs, toSpace, "\\|")
     docs <- tm_map(docs, toSpace, "said")
     docs <- tm_map(docs, toSpace, "says")
     docs <- tm_map(docs, toSpace, "like")
     docs <- tm_map(docs, toSpace, "miss")
     docs <- tm_map(docs, content_transformer(tolower))
     docs <- tm_map(docs, removeNumbers)
     docs <- tm_map(docs, removeWords, stopwords("english"))
     docs <- tm_map(docs, removeWords, c("one", "two","three","four","five","six","seven","eight","nine","ten")) 
     # Remove punctuations
     docs <- tm_map(docs, removePunctuation)
     # Eliminate extra white spaces
     docs <- tm_map(docs, stripWhitespace)
     dtm <- TermDocumentMatrix(docs)
     m <- as.matrix(dtm)
     v <- sort(rowSums(m),decreasing=TRUE)
     d <- data.frame(word = names(v),freq=v)
     set.seed(1234)
     wordcloud(words = d$word, freq = d$freq, min.freq = 3,
               max.words=100, random.order=FALSE, rot.per=0.35, 
               colors=brewer.pal(8, "Dark2"))
   }
   )
   output$headline <- renderText(paste(unique(data$headline[data$year_won == input$year]), "\n", sep=""))
   output$labels <- renderText("Headline(s)")
   output$author <- renderText(paste("By: ",unique(data$author[data$year_won == input$year]), "\n", sep=""))
   output$graph1 <- renderPlot(res = 50, width = 'auto', height = 'auto', {
     sub_data<-droplevels(summary_data[which(summary_data$year_won == input$year),])
     ggplot(data=sub_data, aes(x=headline, y=fk_grade), color=NA) +
       geom_bar(stat='identity',aes(color="auto", fill="auto")) +
       ylab("Reading Level") +
       xlab("") +
       ggtitle("Reading Level") +
       coord_flip() +
       scale_y_continuous(limits=c(0,15), breaks = c(0,2.5,5,7.5,10,12.5,15,17.5,20)) +
       theme_minimal(base_size=25, base_family="Garamond") +
       theme(axis.title.x=element_blank(),
             legend.position="none",
             axis.ticks.x=element_blank(),
             legend.text = element_text(color="#909090", size = 10))
   })
}

# Run the application 
shinyApp(ui = ui, server = server)


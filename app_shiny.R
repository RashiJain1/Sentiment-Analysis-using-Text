#---------------------------------------------------------------------#
#               TABA App                               #
#---------------------------------------------------------------------#

library(shiny)
library(tm)
#library(RWeka)
library(quanteda)
library(shiny)
library(text2vec)
library(tm)
library(tokenizers)
library(wordcloud)
library(slam)
library(stringi)
library(magrittr)
library(tidytext)
library(dplyr)
library(tidyr)
library(ggplot2)
library(tm)
library(tau)
library(plyr)
library(readr)
library(plotly)


# Define ui function
ui <- shinyUI(
  fluidPage(
    
    titlePanel("Text Analysis of Reviews"),
    
    # Input in sidepanel:
    sidebarPanel(
      
      fileInput("file", "Upload text file of Reviews"),
      
      textInput("keywords", ("Enter Keywords separated by comma(,)"), value = "jeep,car"),
      
      
      submitButton(text = "Analyse Again", icon("refresh"))
      
    ),
    
    # Main Panel:
    mainPanel( 
      tabsetPanel(type = "tabs",
                  #
                  tabPanel("Analyse Your Reviews",h4(p("Instructions to use this App")),
                           
                           p("YOU cas use this app to get keyword frequencies of your text review corpus. As a input you can upload a text file and a list of comma seperated keywords.
                           The app will display the relative frequencies of keywords in corpus.", align = "justify"),
                           p("To analyse multiple corpus, modify input file in side-bar panel, click on Analyse Again. Results will get refreshed", align = "Justify"),
                  )
                  ,
                  tabPanel("Keyword Frequency for Reviews",
                           br(),
                           br(),
                           h4("Keyword Frequency"),
                           br(),
                           br(),
                           plotOutput("wordcloud")
                  )
      )
    ))  # end if fluidPage
) # end of UI



# Define Server function
server <- shinyServer(function(input, output) {
  
  output$plot1 = renderPlot({
    
    # Set Minimum and Maximum Word Frequency
    a <- 1
    b <- 100
    
    # Remove Stop Words - Yes/No
    stop_words<-F
    
    # Downlod and Read Text File
    data <- tm::PlainTextDocument(readr::read_lines(file = input$file, 
                                                    progress = interactive()), heading = "KJB", id = basename(tempfile()), 
                                  language = "en", description = "Report File")
    
    # Remove Stop Words and Tokenize Text
    data <- tau::textcnt(
      
      if(stop_words==T) {tm::removeWords(tm::scan_tokenizer(data), tm::stopwords("SMART"))}
      
      else {
        
        tm::scan_tokenizer(data)
      }
      
      , method = "string", n = 1L, lower = 1L)
    
    # Change List to Data Frame
    data <- plyr::ldply(data, data.frame) 
    
    # Using dplyr Filter
    Results<-dplyr::filter(data, data[,2]>a & data[,2]<b)
    
    colnames(Results)<-c("word", "frequency")
    
    ggplot2::ggplot(Results, aes(x=word, y=frequency, fill=word)) + geom_bar(width = 0.75,  stat = "identity", colour = "black", size = 1) + coord_polar(theta = "x") + xlab("") + ylab("") + ggtitle("Word Frequency") + theme(legend.position = "none") + labs(x = NULL, y = NULL)
    
    plotly::ggplotly(ggplot2::ggplot(Results, aes(x=word, y=frequency, fill=word)) + geom_bar(width = 0.75, stat = "identity", colour = "black", size = 1) + 
                       xlab("") + ylab("") + ggtitle("Word Frequency") + theme(legend.position = "none") + labs(x = NULL, y = NULL) + theme(plot.subtitle = element_text(vjust = 1), plot.caption = element_text(vjust = 1), axis.text.x = element_text(angle = 90)) + theme(panel.background = element_rect(fill = "honeydew1"), plot.backgrond = element_rect(fill = "antiquewhite")))%>% config(displaylogo = F) %>% config(showLink = F)
    
    
    
    
    
    
  })
  
})

shinyApp(ui = ui, server = server)
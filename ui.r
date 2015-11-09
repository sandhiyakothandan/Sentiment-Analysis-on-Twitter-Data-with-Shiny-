library(shiny) 

#fluid page that creates two tabes on the output ,
#with emotion and polarity in the drop down box 
#wordcloud as a seperate tab

shinyUI(fluidPage( 
  titlePanel("Twitter Sentiment Analysis"), 
  sidebarLayout( 
    sidebarPanel( 
      textInput("searchTerm", label = "Enter twitter search keywords below", value = "#VirginAmerica"),
      selectInput('plot_opt', 'Plot options', c('emotion', 'polarity'), selectize=TRUE), 
      submitButton("View") 
    ), 
    
    mainPanel(
      tabsetPanel(
        tabPanel("Sentiment analysis for the given word ",plotOutput("plot_emotion")),
        tabPanel("Wordcloud", plotOutput("wordcloudform"))
        
      )
      )
      
      ) 
  ) 
)

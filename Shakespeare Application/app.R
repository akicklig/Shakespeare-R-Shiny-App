library(shiny)
library(tidyverse)
library(wordcloud)
library(ggplot2)
library(shinythemes)
library(RColorBrewer)
library(tidytext)

books <- list("A Mid Summer Night's Dream" = "summer",
              "The Merchant of Venice" = "merchant",
              "Romeo and Juliet" = "romeo")

# task4: add in getFreq function for pre-processing

getFreq <- function(book, stopwords = TRUE) {
  # check that only one of three books is selected
  if (!(book %in% books))
    stop("Unknown book")
  
  text <-  tibble(text = readLines(sprintf("./data/%s.txt", book), encoding="UTF-8"))
  
  # could also pass column of text/character instead
  text <- text %>%
    unnest_tokens(word, text) %>%
    count(word, sort = TRUE) 
  
  if(stopwords){
    text <- text %>%
      anti_join(stop_words)
  }
  
  return(text)
}



ui <- fluidPage(
  
  # task6: add in shinythemes function
  
  theme = shinytheme("cosmo"),
  
  titlePanel("Shakespeare's Plays Word Frequencies"), # Application title
  
  # task1: add in the sidebarLayout with sidebarPanel and mainPanel
  sidebarLayout(
    
    sidebarPanel(
  
  # task2: add in the inputs in the sidebarPanel
      
      selectInput(inputId = "book_input", label = "Choose a Book",
                  choices = books,
                  selected = "summer"),
      
      checkboxInput(inputId = "stop_words", label = "Exclude Stop Words?",
                    value = TRUE),
      
      actionButton(inputId = "run_app", label = "Run App"),
      
      hr(),
      
      h3("Word Cloud Settings"),
      
      sliderInput(inputId = "max_words", label = "Max # of Words",
                  min = 10, max = 200, value = 100, step = 10),
      
      sliderInput(inputId = "largest_word", "Largest Word Size",
                  min = 1, max = 8, value = 4),
      
      sliderInput(inputId = "smallest_word", "Smallest Word Size",
                  min = 0.1, max = 4, value = 0.5),
      
      hr(),
      
      h3("Word Count Settings"),
      
      sliderInput(inputId = "min_words", label = "Min Frequency of Word Count",
                  min = 10, max = 100, value = 25),
      
      sliderInput(inputId = "font_size", label = "Font Size",
                  min = 8, max = 30, value = 14)
      
                  ),
  # task1: within the mainPanel, create two tabs (Word Cloud and Frequency)
  # task3: add in the outputs in the sidebarPanel
  # task6: and modify your figure heights
    mainPanel(
      
      tabsetPanel(type = "tabs",
                  tabPanel(title = "Word Cloud",
                           plotOutput(outputId = "cloud", height = "600px")),
                  tabPanel(title = "Word Counts",
                           plotOutput(outputId = "freq", height = "600px")))
      
    )
  )
)

server <- function(input, output) {
  
  # task5: add in reactivity for getFreq function based on inputs
  
  reactive_freq <- eventReactive(
    input$run_app,
    {
      input$getFreq
      
      withProgress({
        setProgress(message = "Processing corpus...")
        getFreq(input$book_input, input$stop_words) # ... = replace with the two inputs from Task 2
      })
      
    }
  )
  
  output$cloud <- renderPlot({
    
    v <- reactive_freq()
    pal <- brewer.pal(8,"Dark2")
    
    v %>% 
      with(
        wordcloud(
          word, 
          n, 
          scale = c(input$largest_word, input$smallest_word),
          random.order = FALSE, 
          max.words = input$max_words, 
          colors=pal))
    
  })

  output$freq <- renderPlot({
    
    v <- reactive_freq()
    v %>%
      filter(n > input$min_words) %>% 
      ggplot(aes(x = reorder(word,n), y = n)) +
               geom_bar(stat = 'identity', fill = "sea green") +
      coord_flip() +
      theme(legend.position = "none") +
      theme_minimal() +
      theme(axis.title.x = element_blank()) +
      theme(axis.title.y = element_blank()) +
      theme(text = element_text( size = input$font_size))
  })

}

shinyApp(ui = ui, server = server)

library(shiny)

# Define UI for next word application
ui <- fluidPage( 
        tags$style("body {background-color: lightblue;}"),
        mainPanel(
           # Application title
           titlePanel("Find Next Word"),
           textInput("sentence", NULL, "Enter a sentence here (First time might take a few seconds)", "100%"),
           tags$style(type="text/css", "input.shiny-bound-input { font-size:20px; height:35px;}"),
           actionButton("go","Go", style='padding:10px; font-size:150%' ),
           hr(),
           h3(htmlOutput("result"))

        )
)
# Define server logic required to draw a histogram
server <- function(input, output) {
##        setwd("C:/Users/Christian/datasciencecoursera/Capstone")
        load(file = "model3.rda")
        library(tm)
        library(qdap)
        library(ngram)
        library(stringr)
        library(dplyr)

        
        my_predict <- function(text, models, n = 5){
                ## predicts next word based on ngrams and backoff method. returs n most probable values
                if(n < 1){ n <- 1}
                ## would need to check number of models in model list. Here we assue there are 3 (2,3 and 4-gram)
                result <- as.data.frame(matrix(ncol = 2, nrow = n))
                colnames(result) <- c('proposals','freq')
                ##       print(text)
                for (i in 3:1){ 
                        if(word_count(text) > i){ text <- word(text, start = -(i) , end = -1) }
                        stext <- concatenate ("^", text,"", collapse = "")
                        ##                print(stext)
                        ##                print(i)
                        ngrams <- subset(models[[i]], grepl(stext, models[[i]]$words))
                        ngrams <- subset(ngrams,!(word(ngrams$words,-2)%in% stopwords("english")))
                        ## remove no words
                        ngrams <- ngrams[order(-ngrams$freq), ]
                        if(!is.na(ngrams[1,1])){
                                for(j in 1:n){
                                        result[j,1] <- word(ngrams[j, ]$words, -2) 
                                        result[j,2] <- ngrams[j, ]$freq / sum(ngrams$freq)}
                                ##                       break
                        }
                } 
                if(is.na(result[1,1])){
                        result[1,1] <- 'No proposal could be found'
                        result[1,2] <- 0}
                return(result[1,1])
        }
        
        my_clean_input <- function(text){
                ctext <- Corpus(VectorSource(text))
                ctext <- tm_map(ctext, tolower)
                ctext <- tm_map(ctext, removePunctuation, preserve_intra_word_dashes = TRUE)
                ctext <- tm_map(ctext, removeNumbers)
                ##        ctext <- tm_map(ctext, removeWords, stopwords("english"))
                ctext <- tm_map(ctext, stripWhitespace)
                return(ctext$content)
        }
        
        true_sentence <- reactive({
                shiny::validate(need(input$sentence != " ", "Enter a sentence"))
                input$sentence})
        
        my_clean_data <- reactive({my_clean_input(true_sentence()) })
        my_new_word <- reactive({my_predict(my_clean_data(), my_model,1)})

        re <- eventReactive(input$go, {paste(true_sentence(),sep = " ", "<font color=\"#FF0000\"><b>", my_new_word(), "</b></font>")})
        output$result <- renderText({re()})
    
        
}

# Run the application 
shinyApp(ui = ui, server = server)


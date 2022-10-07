# shiny libraries
library(shiny)
library(shinycssloaders) # not on DTU Health Tech server
# library(wordcloud) # not on DTU Health Tech server
# library(rsconnect)
library(markdown)

# clssify_articles_main libraries
library(parallel)
library(doParallel)
library(doMC)
library(easyPubMed) # not on DTU Health Tech server
library(dplyr)
library(plyr)
library(tidyr) # not on DTU Health Tech server
library(stringr)
library(tidymodels) # not on DTU Health Tech server
library(textrecipes) # not on DTU Health Tech server
library(discrim) # not on DTU Health Tech server
library(rules) # not on DTU Health Tech server
library(baguette) # not on DTU Health Tech server

# pca_plot libraries
library(stopwords) # not on DTU Health Tech server

# word_cloud libraries
library(wordcloud) # not on DTU Health Tech server
library(RColorBrewer)

# Source custom functions
source("classify_articles_functions.R")
source("word_cloud.R")
source("pca_plot.R")
source("helpers.R")

options(spinner.color="#0275D8", 
        spinner.color.background="#ffffff", 
        spinner.size=1.5)

ui <- (fluidPage(
  
  # Title panel with DTU logo
  titlePanel(
    fluidRow(
      column(9, h1("BioReader - 2.0"), 
             h4("Biomedical Research Article Distiller")),
      column(3, tags$img(height = 100, width = 65,
                         src = "img/DTU_Logo_Corporate_Red_RGB.png")),
      )
    ),
  
  mainPanel(
    tabsetPanel(
      
      # Primary panel where PubMed IDs can be submitted
      tabPanel("Submission",
               helpText("Do you have a large number of research articles to go 
                        through, but do not know where to start? BioReader can 
                        help you distill your reading list by ranking articles 
                        by relevance.  Simply collect the PubMed IDs of a number 
                        of articles you found relevant and a similar
                        number of articles not relevant to you (we recommend at 
                        least 20 in each category - see Tips and Tricks in 
                        instruction for successful classification). These two 
                        sets of PubMed IDs represent your positive and negative 
                        text mining training corpora to be pasted below. Then, 
                        either paste the PubMed IDs of up to 1000 articles that 
                        you would like to have ranked according to you content 
                        of interest, or enter a PubMed search term, and 
                        BioReader will provide you with a ranked reading list to 
                        limit the time wasted on reading irrelevant literature."),
               
               # Submission boxes for the PubMed IDs
               column(width = 5, offset = 0, style='padding:0px;', 
                      wellPanel(
                        h4("Positive category"),
                        p("Paste PubMed IDs for papers containing information 
                          relevant to you."), 
                        p("For example", 
                          a("Click here.",
                            href = paste0("https://services.healthtech.dtu.dk/",
                                         "services/BioReader-1.2/ex_set1"), 
                            target="_blank")),
                        textAreaInput(inputId = "pmidPositive", 
                                      label = NULL, width = "400px", 
                                      height = "180px", resize = "none"),
                        
                        h4("Negative category"),
                        p("Paste PubMed IDs for papers similar to the positive 
                          category, but not containing information relevant to 
                          you."), 
                        p("For example", 
                          a("Click here.", 
                            href = paste0("https://services.healthtech.dtu.dk/",
                                          "services/BioReader-1.2/ex_set2"), 
                            target="_blank")),
                        textAreaInput(inputId = "pmidNegative", 
                                      label = NULL, width = "400px", 
                                      height = "180px", resize = "none"),
                     
                        h4("Documents to classify"),
                        p("Paste PubMed IDs (max 1000) for articles that you 
                          would like to have classified as either relevant or 
                          irrelevant."), 
                        p("For example", 
                          a("Click here.", 
                            href = paste0("https://services.healthtech.dtu.dk/",
                                          "services/BioReader-1.2/ex_testset"), 
                            target="_blank")),
                        textAreaInput(inputId = "pmidTBD", label = NULL, 
                                      width = "400px", height = "180px", 
                                      resize = "none"),
                     
                     actionButton(inputId = "submitPMID", label = "Submit Job"),
                     br()
                     )),
               
               # Column with output of text classification
               column(width = 7, 
                      tabsetPanel(
                        
                        # Results with ranked articles
                        tabPanel("Table", 
                                 withSpinner(DT::dataTableOutput(outputId = 
                                                                   "table"), 
                                             image = "spinner_2.gif", 
                                             image.height = 300, 
                                             image.width = 300)),
                        
                        # Word cloud with the top positive and negative terms
                        tabPanel("Word cloud", 
                                 fluidRow(
                                   column(width = 12,
                                          withSpinner(
                                          plotOutput(outputId = "word_cloud_1"),
                                          image = "spinner_2.gif", 
                                          image.height = 300, 
                                          image.width = 300)),
                                   
                                   column(width = 12,
                                          withSpinner(
                                            plotOutput(outputId = "word_cloud_2"),
                                            image = "spinner_2.gif", 
                                            image.height = 300, 
                                            image.width = 300)))),
                        
                        # PCA plot of the document-term matrix
                        tabPanel("PCA plot", 
                                 withSpinner(
                                   plotOutput(outputId = "pca_plot"),
                                   image = "spinner_2.gif", 
                                   image.height = 300, 
                                   image.width = 300)))),
               ),
      
      # Panel with instructions
      tabPanel("Instructions",
               fluidRow(column(12, includeMarkdown("instructions.md")))
         ),
      
      # Panel with citation instructions
      tabPanel("Citation",
               fluidRow(column(12, includeMarkdown("citation.md"))),
         )
      ),
    style='width: 1200px; height: 1000px',
   )
)
)



server <- function(input, output, session){
  
  # Reactive elements for checking the PMIDs have been supplied by the user
  posPMIDs <- reactive({
    validate(
      need(input$pmidPositive != "", 
           "Please write the PubMed IDs for the articles in the positive category"),
    )
    input$pmidPositive
  })
  
  negPMIDs <- reactive({
    validate(
      need(input$pmidNegative != "", 
           "Please write the PubMed IDs for the articles in the negative category"),
    )
    input$pmidNegative
  })
  
  tbdPMIDs <- reactive({
    validate(
      need(input$pmidTBD != "", 
           "Please write the PubMed IDs of the articles you wish to classify"),
    )
    input$pmidTBD
  })
   
   # Collect pmid article data
   pmid_data <- eventReactive(
     input$submitPMID, {
      withProgress({
         setProgress(message = "Downloading articles from PubMed...",
                     value = 0)
      pubmed_articles(posPMIDs(), 
                      negPMIDs(), 
                      tbdPMIDs(),
                      shiny_input=TRUE,
                      progress=TRUE)
      })}
      )
   
   # Split data into training and testing data
   data_splitted <- reactive({
      withProgress({
         setProgress(message = "Splitting data...")
         split_data(pmid_data())
      })
   })
   
   
   # Generate data to be used for creating wordclouds
   word_cloud_data <- reactive({
      withProgress({
         setProgress(message = "Processing data for wordcloud...")
         word_cloud(data = data_splitted(), pmid_data = pmid_data())
      })
   })
   
   # Generate table with ranked articles
   output$table <- DT::renderDataTable({
     withProgress({
        setProgress(message = "Classifying articles...")
     DT::datatable(classify_articles(data_splitted(),
                                     metric="roc_auc", 
                                     verbose=TRUE,
                                     fold=5)$ranked_results,
                   escape = FALSE,
                   selection = "none",
                   rownames = FALSE)
     })
  })
   
   # Make wordcloud function reapeatable
   wordcloud_rep <- repeatable(wordcloud)

   # Setting colors
   cols  <- colorRampPalette(c("#530000", "#D0D0D0"))(25)

   # Creating the word cloud for the positive terms
   output$word_cloud_1 <- renderPlot({
      withProgress({
         setProgress(message = "Creating wordcloud for postive terms...")
      pos_av_freq <- word_cloud_data()$pos_av_freq
      wordcloud_rep(words = pos_av_freq$word, freq = pos_av_freq$freq,
                    min.freq = 0.05, max.words = 45, random.order = FALSE,
                    rot.per = 0, colors = cols, scale = c(3.75, 1))
      title(main="Top discriminating class I terms", family="Helvetica",
            cex.main=1.5, font.main=1)
      })
   })

   # Creating the word cloud for the negative terms
   output$word_cloud_2 <- renderPlot({
      withProgress({
         setProgress(message = "Creating wordcloud for negative terms...")
         neg_av_freq <- word_cloud_data()$neg_av_freq
         wordcloud_rep(words = neg_av_freq$word, freq = neg_av_freq$freq,
                       min.freq = 0.05, max.words = 45, random.order = FALSE,
                       rot.per=0, colors = cols, scale=c(3.75, 1))
         title(main="Top discriminating class II terms", family="Helvetica",
               cex.main=1.5, font.main=1)
      })
   })
   
   # Create PCA plot
   output$pca_plot <- renderPlot({
      withProgress({
         setProgress(message = "Creating PCA plot...")
         pca_plot(data_splitted())
      })
   })
}
   
shinyApp(ui = ui, server = server)

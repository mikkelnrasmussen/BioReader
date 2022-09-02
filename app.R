# shiny libraries
library(shiny)
library(shinycssloaders)
library(wordcloud)
library(rsconnect)
library(markdown)

# clssify_articles_main libraries
library(parallel)
library(doMC)
library(easyPubMed)
library(dplyr)
library(plyr)
library(tidyr)
# library(stringr)
library(tidymodels)
library(textrecipes)
library(discrim)
library(rules)
library(baguette)

# pca_plot libraries
library(stopwords)

# word_cloud libraries
library(wordcloud)
library(tidyr)
library(RColorBrewer)

# Source custom functions
source("classify_articles_functions.R")
source("word_cloud.R")
source("pca_plot.R")
source("helpers.R")

options(spinner.color="#0275D8", spinner.color.background="#ffffff", spinner.size=1.5)

ui <- (fluidPage(
  
   titlePanel(
      fluidRow(
        column(9, h1("BioReader - 1.2"), h4("Biomedical Research Article Distiller")),
         column(3, tags$img(height = 100, width = 65,
                            src = "img/DTU_Logo_Corporate_Red_RGB.png")),
      )
   ),
   mainPanel(
      tabsetPanel(
         tabPanel("Submission",
                  helpText("Do you have a large number of research articles to go through, but do not know where to start?
                                             BioReader can help you distill your reading list by ranking articles by relevance. 
                                             Simply collect the PubMed IDs of a number of articles you found relevant and a similar
                                             number of articles not relevant to you (we recommend at least 20 in each category
                                             - see Tips and Tricks in instruction for successful classification). These two sets of PubMed
                                             IDs represent your positive and negative text mining training corpora to be pasted below.
                                             Then, either paste the PubMed IDs of up to 1000 articles that you would like to have ranked
                                             according to you content of interest, or enter a PubMed search term, and BioReader will
                                             provide you with a ranked reading list to limit the time wasted on reading irrelevant
                                             literature."),
                  
                  column(width = 5, offset = 0, style='padding:0px;', wellPanel(
                     h4("Positive category"),
                     p("Paste PubMed IDs for papers containing information relevant to you."), 
                     p("For example", a("Click here.", href = "https://services.healthtech.dtu.dk/services/BioReader-1.2/ex_set1", target="_blank")),
                     textAreaInput(inputId = "pmidPositive", label = NULL, width = "400px", height = "180px", resize = "none"),
                     
                     h4("Negative category"),
                     p("Paste PubMed IDs for papers similar to the positive category, but not containing information relevant to you."), 
                     p("For example", a("Click here.", href = "https://services.healthtech.dtu.dk/services/BioReader-1.2/ex_set2", target="_blank")),
                     textAreaInput(inputId = "pmidNegative", label = NULL, width = "400px", height = "180px", resize = "none"),
                     
                     h4("Documents to classify"),
                     p("Paste PubMed IDs (max 1000) for articles that you would like to have classified as either relevant or irrelevant."), 
                     p("For example", a("Click here.", href = "https://services.healthtech.dtu.dk/services/BioReader-1.2/ex_testset", target="_blank")),
                     textAreaInput(inputId = "pmidTBD", label = NULL, width = "400px", height = "180px", resize = "none"),
                     
                     actionButton(inputId = "submitPMID", label = "Submit Job"),
                     br()
                     
                  )),
                  
                  column(width = 7, 
                         tabsetPanel(
                            tabPanel("Table", withSpinner(DT::dataTableOutput(outputId = "table"), 
                                                          image = "spinner_2.gif", image.height = 300, image.width = 300)),
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
                            
                            tabPanel("PCA plot", 
                                     withSpinner(plotOutput(outputId = "pca_plot"),
                                                 image = "spinner_2.gif", 
                                                 image.height = 300, 
                                                 image.width = 300)))),
                  
         ),
         tabPanel("Instructions",
                  fluidRow(
                     column(12, includeMarkdown("instructions.md")))
         ),
         tabPanel("Citation",
                  fluidRow(
                     column(12, includeMarkdown("citation.md"))
                  ),
         )
      ),style='width: 1200px; height: 1000px',
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
     
     input$submitPMID,
    
      withProgress({
         setProgress(message = "Downloading articles from PubMed...",
                     value = 0)
         
      pubmed_articles(posPMIDs(), 
                      negPMIDs(), 
                      tbdPMIDs(),
                      shiny_input=TRUE,
                      progress=TRUE)
      })
      )
   
   # Split data into training and testing data
   data_splitted <- reactive({
      
      req(pmid_data())
      
      withProgress({
         setProgress(message = "Splitting data...")
         pm_data <- pmid_data()
         split_data(pm_data)
      })
   })
   
   
   word_cloud_data <- reactive({

      req(data_splitted())

      withProgress({
         setProgress(message = "Processing data for wordcloud...")
         data_split <- data_splitted()
         pm_data <- pmid_data()
         word_cloud(data = data_split, pmid_data = pm_data)

      })
   })

   output$table <- DT::renderDataTable({
      
     # pm_data <- pmid_data()
     data_split <- data_splitted()
      
     withProgress({
        setProgress(message = "Classifying articles...")
   
     DT::datatable(classify_articles(data_split)$ranked_results,
                   escape = FALSE,
                   selection = "none",
                   rownames = FALSE)
     })
  })

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
   # Plot PCA
   output$pca_plot <- renderPlot({
      
      req(data_splitted())
      
      withProgress({
         setProgress(message = "Creating PCA plot...")
         data_split <- data_splitted()
         pca_plot(data_split)
      })
   })
}
   
shinyApp(ui = ui, server = server)

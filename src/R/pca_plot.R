## Call libraries
# suppressMessages(library(easyPubMed))
# suppressMessages(library(parallel))
# suppressMessages(library(foreach))
# suppressMessages(library(doParallel))
suppressMessages(library(dplyr))
suppressMessages(library(workflows))
suppressMessages(library(tune))
suppressMessages(library(stopwords))
suppressMessages(library(tidymodels))
suppressMessages(library(tidytext))
suppressMessages(library(textrecipes))
suppressMessages(library(discrim))
suppressMessages(library(keras))

pca_plot <- function(data){
   
   train_data <- data$train_data
   test_data <- data$test_data
   
   # Creating recipe and specifying outcome and predictors
   train_rec <-
      recipe(class ~ ., data = train_data) %>% 
      update_role(pmid, new_role = "id") %>% 
      step_tokenize(abstract) %>% 
      step_stopwords(abstract) %>%
      step_stem(abstract) %>%
      step_tokenfilter(abstract, max_tokens = 500) %>% 
      step_tfidf(abstract)
   
   dtm_tfidf <- train_rec %>%
     prep() %>% 
     juice() %>% 
     dplyr::select(-c(class, year, title))
   
   colnames(dtm_tfidf) <- gsub(pattern = "tfidf_abstract_", "", colnames(dtm_tfidf))
   dtm_tfidf <- dtm_tfidf[,grepl(pattern = "[a-z]", colnames(dtm_tfidf), ignore.case=TRUE, perl=TRUE)]
   
   pca_rec <- recipe(~., data = dtm_tfidf) %>%
      update_role(pmid, new_role = "id") %>%
      step_normalize(all_predictors()) %>%
      step_pca(all_predictors())
   
   pca_prep <- pca_rec %>% 
         prep()
   
   tidied_pca <- tidy(pca_prep, 2)
   
   tidied_pca %>%
      filter(component %in% paste0("PC", 1:5)) %>%
      mutate(component = fct_inorder(component)) %>%
      ggplot(aes(value, terms, fill = terms)) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~component, nrow = 1) +
      labs(y = NULL)
   
   tidied_pca %>%
      filter(component %in% paste0("PC", 1:4)) %>%
      group_by(component) %>%
      top_n(8, abs(value)) %>%
      ungroup() %>%
      mutate(terms = reorder_within(terms, abs(value), component)) %>%
      ggplot(aes(abs(value), terms, fill = value > 0)) +
      geom_col() +
      facet_wrap(~component, scales = "free_y") +
      scale_y_reordered() +
      labs(
         x = "Absolute value of contribution",
         y = NULL, fill = "Positive?"
      )
   
   juice(pca_prep) %>%
     inner_join(., train_data %>% dplyr::select(pmid, class),
                by = "pmid") %>% 
     ggplot(aes(PC1, PC2)) +
     geom_point(aes(color = class), alpha = 0.7, size = 2) + 
     ggtitle("PCA plot of the positive and negative articles") +
     theme(plot.title = element_text(size = 15, family = "Helvetica",
                                     hjust = 0.5, 
                                     margin = margin(t = 20, r = 0, b = 20, l = 0)))
}

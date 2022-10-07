setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## Call libraries
library(parallel)
library(doMC)
library(easyPubMed)
library(dplyr)
library(plyr)
library(tidyr)
library(stringr)
library(tidymodels)
library(textrecipes)
library(discrim)
library(plsmod)
library(rules)
library(baguette)
library(doParallel)
library(data.table)
library(httr)
library(xml2)

source("classify_articles_functions.R")

# Whether to train, save or load models
train_model <- FALSE
save_model <- FALSE
load_model <- TRUE

# Whether to test the pubmed_articles function or not 
download_articles <- FALSE


if(download_articles){
  # Load the datasets
  all_pos <- read.table(file.path("data", "allergy_positive.txt"))
  all_neg <- read.table(file.path("data", "allergy_negative.txt"))
  
  # Split the positive and negative cases to make a pseudo-TBD group
  tbd_pmids <- all_pos[1:100,] %>% append(all_neg[1:100,])
  pos_pmids <- all_pos[501:1000,]
  neg_pmids <- all_neg[501:1000,]
  
  # Provide the PMIDs positive and negative for the information and the PMIDs for 
  # the articles that needs to be determined. The input right now is strings with 
  # the PMIDs, since we are working with input through a Shiny app.
  pmid_data <- retrive_articles(pmidPositive = pos_pmids,
                                pmidNegative = neg_pmids,
                                pmidTBD = tbd_pmids,
                                verbose = TRUE,
                                progress=FALSE,
                                shiny_input = FALSE)
  
  sum(pmid_data$class == 0)
  sum(pmid_data$class == 1)
  sum(pmid_data$class == 2)
  
  test_class <- pmid_data[pmid_data$class == 2, ]
  test_class$class <- ifelse(test_class$pmid %in% all_pos[1:100,],
                             "Positive", "Negative")
  test_class$class <- factor(test_class$class, levels = c("Positive", "Negative"))
  true_classes <- test_class[, c('pmid', 'class')]
  
} else {
  
  # Load the already downloaded PMID abstracts
  column_names <- c('pmid', 'year', 'title', 'abstract', 'class')
  train_pos <- read.csv(file.path("data", "curatable_training_set.csv"),
                        col.names = column_names) %>% filter(year < 2020)
  train_neg <- read.csv(file.path("data", "uncuratable_training_set.csv"),
                        col.names = column_names) %>% filter(year < 2020)
  test_pos <- read.csv(file.path("data", "curatable_test_set.csv"),
                       col.names = column_names)
  test_neg <- read.csv(file.path("data", "uncuratable_test_set.csv"),
                       col.names = column_names)
  
  # Create a dataframe with training and test data
  set.seed(1353)
  #training_data <- rbind(train_pos, train_neg[sample(1:dim(train_pos)[1]), ])
  training_data <- rbind(train_pos[1:100, ], train_neg[1:100, ])
  training_split <- initial_split(training_data, strata = class, prop = 0.8)
  orig_train_data <- training(training_split)
  orig_test_data <- testing(training_split)

  orig_train_data$pmid <- as.character(orig_train_data$pmid)
  orig_test_data$pmid <- as.character(orig_test_data$pmid)

  # Check that classes are balanced in train and test data
  sum(orig_train_data$class == 'yes')
  sum(orig_train_data$class == 'no')
  sum(orig_test_data$class == 'yes')
  sum(orig_test_data$class == 'no')

  # Change class label on test data
  tbd_data <- orig_test_data %>%
     mutate(class = 2)

  # Change class label on train data and merge with test data
  pmid_data <- orig_train_data %>%
     mutate(class = ifelse(class == 'yes', 1, 0)) %>%
     rbind(tbd_data)
  
  # Create dataframe with true class values
  test_class <- orig_test_data %>%
     mutate(class = ifelse(class == 'yes', 'Positive', 'Negative'))
  #test_class <- test_class[test_class$pmid %in% testing_data$pmid, ]
  test_class$class <- factor(test_class$class, levels = c("Positive", "Negative"))
  true_classes <- test_class[, c('pmid', 'class')]
  
}

# Splitting the PMIDs into training and testing data
data_separated <- split_data(data=pmid_data)
training_data <- tibble(data_separated$train_data)
testing_data <- tibble(data_separated$test_data)

sum(training_data$class == 'Positive')
sum(training_data$class == 'Negative')
dim(testing_data)

if(train_model){
  # Training the classifiers and select the best classifier based on specified 
  # metric
  training_results <- train_classifiers(train_data = training_data, 
                                        eval_metric="roc_auc", 
                                        verbose=TRUE, fit_all=TRUE)
  
  # Collect model(s), metrics and predictions
  metrics <- training_results$model_metrics
  pred_train <- training_results$model_predictions
  final_model <- training_results$best_model
  fitted_models <- training_results$fitted_models}


# Save final model
if(save_model){
  saveRDS(training_results, "full_model_object.rds")
  saveRDS(final_model, "rf_pred_model_full.rds")
  
}

if(load_model){
  final_model_list <- readRDS("rf_pred_model.rds")
  metrics <- final_model_list$model_metrics
  pred_train <- final_model_list$model_predictions
  final_model <- final_model_list$best_model
  fitted_models <- final_model_list$fitted_models
}

# Predict classes for the test data and extract results
prediction_results <- classifier_predict(final_model_fit=final_model, 
                                         test_data=testing_data)
ranked_results <- prediction_results$ranked_results
pred_test <- prediction_results$pred

# Evaluate the models on the training and test data
evaluation_plots <- evaluate_models(test_data=testing_data, pred_train = pred_train,
                                    fitted_models=fitted_models, 
                                    metrics=metrics, classes=true_classes)


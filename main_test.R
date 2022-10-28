setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## Call libraries
library(httr) 
library(dplyr)
library(xml2)
library(purrr)
library(stringr)
library(rentrez) # not on DTU server
library(tidyr)
library(textrecipes) # not on DTU server
library(tidymodels) # not on DTU server
library(discrim) # not on DTU server
library(plsmod) # not on DTU Heath Tech server
library(plyr)

source("classify_articles_functions.R")

# Whether to train, save or load models
train_model <- TRUE
save_model <- FALSE
load_model <- FALSE

# Whether to test the pubmed_articles function or not 
download_articles <- TRUE

# Load the datasets
all_pos <- read.table(file.path("data", "allergy_positive.txt"))
all_neg <- read.table(file.path("data", "allergy_negative.txt"))
N <- dim(all_pos)[1]
df_time <- data.frame()

for(i in seq(1, N, by=50)){
start.time <- Sys.time()
if(download_articles){
  
  start_index <- 1
  stop_index <- i + 100 - 1
  if(stop_index > N){
    stop_index <- N
  }
  
  current_pos_pmids <- all_pos[start_index:stop_index,]
  current_neg_pmids <- all_neg[start_index:stop_index,]
  
  # Split the positive and negative cases to make a pseudo-TBD group
  train_prct <- 0.8
  train_index <- start_index:as.numeric(stop_index*train_prct)
  test_index <- as.numeric((stop_index*train_prct)+1):stop_index
  pos_pmids <- current_pos_pmids[train_index]
  neg_pmids <- current_neg_pmids[train_index]
  tbd_pmids <- c(current_pos_pmids[test_index], 
                 current_neg_pmids[test_index])

  # Provide the PMIDs positive and negative for the information and the PMIDs for 
  # the articles that needs to be determined. The input right now is strings with 
  # the PMIDs, since we are working with input through a Shiny app.
  pmid_data <- retrive_articles(pmidPositive = pos_pmids,
                                pmidNegative = neg_pmids,
                                pmidTBD = tbd_pmids,
                                verbose = TRUE,
                                progress=FALSE,
                                shiny_input = FALSE)
  
  sum(pmid_data$class == 1)
  sum(pmid_data$class == 0)
  sum(pmid_data$class == 2)
  
  test_class <- pmid_data[pmid_data$class == 2, ]
  test_class$class <- ifelse(test_class$pmid %in% current_pos_pmids,
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
                                        seed_num=123,
                                        verbose=TRUE, 
                                        fit_all=TRUE,
                                        model_names=c("bart", "xgboost", "ldm", "logit",
                                                      "mr", "nb","knn", "rf", "pls", 
                                                      "svm_linear")
                                        )
  
  # Collect model(s), metrics and predictions
  metrics <- training_results$model_metrics
  pred_train <- training_results$model_predictions
  final_model <- training_results$best_model
  fitted_models <- training_results$fitted_models
  }


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

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
data_amount <- length(current_pos_pmids) + length(current_neg_pmids)
tmp <- data.frame(data=data_amount, time=time.taken)
df_time <- rbind(df_time, tmp)

}

saveRDS(df_time, "bioreader_time.rds")

library(ggplot2)
library(ggthemes)
df_time %>% 
  ggplot(aes(x=data, y=as.numeric(time))) +
  geom_point(size=3, color = "#0277bd") +
  #geom_line(color = "#0277bd", linetype = "dotted", size = .5) + 
  geom_smooth(formula = 'y ~ x', method = "lm", color = "#0277bd", 
              linetype = "dotted", size = .5) + 
  labs(title="Expected runtime of BioReader-2.0",
       y="Time (min)", 
       x="Number of abstracts",
       caption = "The runtime may vary due to server load") +
  theme(axis.title.x = element_text(margin = margin(t = 10), size = 12,
                                    family = "Times New Roman"),
        axis.title.y = element_text(margin = margin(r = 10), size = 12,
                                    family = "Times New Roman",),
        axis.text = element_text(size = 12, family = "Times New Roman",),
        plot.title = element_text(face = "bold",
                                  family = "Times New Roman",
                                  margin = margin(10, 0, 10, 0),
                                  size = 16),
        plot.caption = element_text(hjust = 0, family = "Times New Roman"),
        panel.background = element_rect(fill = NA),
        plot.background = element_rect(fill = "#DEDEDE",
                                       color = "#DEDEDE", size = 2)) + 
  scale_x_continuous(breaks = round(seq(min(df_time$data), max(df_time$data), 
                                        by = 100), 1))#+ theme_economist()
ggsave("BioReader_runtime.png", width = 10, height = 7)

# Evaluate the models on the training and test data
evaluation_plots <- evaluate_models(test_data=testing_data, pred_train = pred_train,
                                    fitted_models=fitted_models, 
                                    metrics=metrics, classes=true_classes)


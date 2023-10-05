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
library(baguette)
library(rules)
library(readxl)
library(themis)
library(doFuture)
library(parallel)


# Load the data
file_names <- dir("data/training_data", full.names = TRUE)
df_all_classes <- do.call(rbind, lapply(file_names, read.csv))
df_class_label <- read_excel("data/All_Updated_Categories_2019.xlsx")

# QC: Check if all the cateogries are present in both the metadata file
# and the data files
df_all_classes_only <- df_all_classes %>%
  filter(!(SubType %in% df_class_label$Abbreviation)) %>% 
  select(SubType) %>% 
  pull() %>% 
  unique()

df_class_label_only <- df_class_label %>%
  filter(!(Abbreviation %in% df_all_classes$SubType)) %>% 
  select(Abbreviation) %>% 
  pull() %>% 
  unique()

# Perform inner join to only keep the categories that are in common
df_merged <- df_all_classes %>%
  inner_join(., df_class_label,
            by=c("SubType" = "Abbreviation"))

# QC: Check which columns contain NAs 
df_merged %>%
  is.na() %>%
  colSums()

# Create dataframe with all the classes
df_main_classes <- df_merged %>%
  select(PubMed_ID, Title, Abstract, Class) %>%
  dplyr::rename(pmid = PubMed_ID) %>%
  dplyr::rename_with(tolower)

# QC: Check if there are any NAs
df_main_classes %>%
  is.na() %>%
  colSums()

# Create dataframe with all categories
df_all_classes <- df_merged %>%
  select(PubMed_ID, Title, Abstract, SubType) %>%
  dplyr::rename(pmid = PubMed_ID) %>%
  dplyr::rename(class = SubType) %>%
  dplyr::rename_with(tolower)

# QC: Check if there are any NAs
df_all_classes %>%
  is.na() %>%
  colSums()

# Create training and test set
set.seed(123)
split <- initial_split(df_all_classes, strata = class, prop = 0.90)
training_data <- training(split)
testing_data <- testing(split)
dim(training_data)
dim(testing_data)

training_data <- training_data[, c('pmid', 'abstract', 'class')]
train_rec <-
  recipe(class ~ ., data = training_data) %>% 
  update_role(pmid, new_role = "id") %>% 
  step_tokenize(abstract) %>% 
  step_stopwords(abstract) %>%
  step_stem(abstract) %>%
  step_tokenfilter(abstract, max_tokens = 500) %>%
  step_tfidf(abstract) %>%
  step_downsample(class)

#train_prep <- prep(train_rec)
#train_prep

# We will save a data frame from the PREP to use later with another algo
#train_baked <- bake(train_prep, new_data = NULL)
#write.csv(train_baked, "data/abstracts_baked.csv", row.names = FALSE)
#dim(train_baked)

## Cross Validation Split of Training Data
set.seed(888)
train_folds <- vfold_cv(data = training_data, v = 10)
train_folds

# Lasso
lasso_spec <- multinom_reg(
  penalty = tune(), 
  mixture = 1
) %>%
  set_engine("glmnet")

# Support Vector Machine - polynomial degree = 1
svmlinear_spec <- svm_poly(degree=1, cost = tune()) %>%
  set_engine("kernlab") %>%
  set_mode("classification")

# Support Vector Machine - radial basis function
svmrbf_spec <- svm_rbf(cost = tune(), rbf_sigma = tune()) %>%
  set_mode("classification") %>%
  set_engine("kernlab")

# Random Forest
randomf_spec <- rand_forest(
    mtry = tune(),
    trees = tune(),
    min_n = tune()
    ) %>%
  set_mode("classification") %>%
  set_engine("ranger")

# XGBoost
xgboost_spec <- boost_tree(
  trees = tune(),
  mtry = tune(),
  tree_depth = tune(),
  learn_rate = .01
  ) %>%
  set_mode("classification") %>% 
  set_engine("xgboost")

# Neural network
nnet_spec <- mlp(epochs = 30,
                   hidden_units = tune(),
                   dropout = tune()) %>%
  set_mode("classification") %>%
  set_engine("keras", verbose = 2)


# Set up workflow set
workflow_sets <-workflow_set(
  preproc = list(train_rec),
  models = list(
    lasso_spec,
    svmrbf_spec,
    xgboost_spec,
    randomf_spec
    ),
  cross = TRUE
  )
workflow_sets


RUN = FALSE
if (RUN) {
    control <- control_resamples(save_pred = TRUE, verbose = TRUE,
                                   allow_par=TRUE,
                                   parallel_over = "resamples")
      
    library(doFuture)
    library(parallel)
    
    fit_workflows <- workflow_sets %>%
        workflow_map(
            seed = 888,  
            fn = "tune_grid",
            grid = 20,
            resamples = train_folds,
            verbose = TRUE,
            control = control
            )
    
    numCores <- as.numeric(Sys.getenv('LSB_DJOB_NUMPROC'))
    
    registerDoFuture()
    cl <- makeCluster(numCores)
    cl
    plan(cluster, workers=cl)
    #Train all the models by mapping the fit_resamples function to every
    #training workflow
    start.time <- Sys.time()
    train_results <- train_models %>%
      workflow_map("fit_resamples", resamples = train_folds,
                    metrics = metrics,
                    verbose = TRUE,
                    control = control)
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    print(time.taken)
}

if (RUN) {
    saved_abstract_modelset <- fit_workflows
    saveRDS(saved_abstract_modelset, "saved_abstract_modelset_all_data.rds")
    }

if (!RUN) {
    # fit_workflows <- readRDS("saved_imdb_modelset_50K.rds")
    fit_workflows <- readRDS("saved_abstract_modelset_all_data.rds")
    #fit_workflows <- readRDS("saved_imdb_modelset_SVM_50K.rds")
}

autoplot(fit_workflows)
ggsave("model_parformance_auc_and_accuracy.png")


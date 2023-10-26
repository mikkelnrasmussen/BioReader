## Call libraries
library(tidyverse)
library(httr)
library(xml2)
library(rentrez) # not on DTU server
library(textrecipes) # not on DTU server
library(tidymodels) # not on DTU server
library(discrim) # not on DTU server
library(plsmod) # not on DTU Heath Tech server
library(plyr)
library(baguette)
library(rules)
library(readxl)
library(themis)
library(optparse)

option_list <- list(
  make_option(
    c("-t", "--target"),
    type = "character",
    default = "class",
    help = "The target variable to train a model for [default: %default]"
  ),
  make_option(
    c("--test"),
    action = "store_true",
    default = FALSE,
    help = "Whether to subsample the data for testing [default: %default]"
  )
)

opt_parser <- OptionParser(option_list = option_list)
opt <- parse_args(opt_parser)

# Access named arguments
cat(paste("Target:", opt$target), fill = TRUE)
cat(paste("Test:", opt$test), fill = TRUE)

###################################################################
######################### Load Data ###############################
###################################################################

file_names <- dir("data/training_data", full.names = TRUE)
df_all_classes <- file_names |>
  map(\(x) read_csv(x, show_col_types = FALSE)) |>
  bind_rows()
df_class_label <- read_excel(
  "data/All_Updated_Categories_2019.xlsx"
)

# If the category label is missing, then add the class
# label instead
df_class_label <- df_class_label |>
  mutate(
    category = if_else(is.na(category), class, category)
  )

# If the subcategory label is missing add the one found in the
# Subcatgory column
df_class_label <- df_class_label |>
  mutate(
    subcategory = if_else(
      is.na(subcategory),
      Abbreviation,
      subcategory
    )
  )

###################################################################
###################### Quality Control ############################
###################################################################

# Check if all the cateogries are present in both the metadata file
# and the data files
df_all_classes_only <- df_all_classes |>
  filter(!(SubType %in% df_class_label$subcategory)) |>
  select(SubType) |>
  pull() |>
  unique()

df_class_label_only <- df_class_label |>
  filter(!(subcategory %in% df_all_classes$SubType)) |>
  select(subcategory) |>
  pull() |>
  unique()

# Perform inner join to only keep the categories that are in common
df_merged <- df_all_classes |>
  left_join(
    df_class_label,
    by = c("SubType" = "subcategory")
  ) |>
  filter(!(SubType %in% df_all_classes_only))

# QC: Check which columns contain NAs
df_merged |>
  is.na() |>
  colSums()

# Let's fist look at the rows with missing titles
df_merged |>
  filter(is.na(Title))

# There are some abstracts that look weird and starts with
# "[Data extracted from this article was imported from"
# Let's remove those
weird_abstract <- "\\[Data extracted from this article was imported from"
df_merged <- df_merged |>
  filter(!str_detect(Abstract, weird_abstract))

# Let's check again which columns contain NAs
df_merged |>
  is.na() |>
  colSums()

# Create dataframe with all the classes, category or subcategory
df_main <- df_merged |>
  select(PubMed_ID, Abstract, opt$target)
colnames(df_main) <- c("pmid", "abstract", "target")
df_main <- df_main |>
  mutate(target = as.factor(target))

# QC: Check if there are any NAs
df_main |>
  is.na() |>
  colSums()

# Check the distribution of the different classes
df_main |>
  group_by(target) |>
  dplyr::summarise(n = n())

# Sample a subset of the data for testing
if (opt$test) {
  df_main <- df_main |>
    group_by(target) |>
    sample_n(min(n(), 200)) |>
    ungroup()
}

# Create training and test set
set.seed(123)
split <- initial_split(df_main, strata = target, prop = 0.90)
training_data <- training(split)
testing_data <- testing(split)

# Setup the preprocessing steps
train_rec <- training_data |>
  recipe(target ~ ., data = _) |>
  update_role(pmid, new_role = "id") |>
  step_tokenize(abstract) |>
  step_stopwords(abstract) |>
  step_stem(abstract) |>
  step_tokenfilter(abstract, max_tokens = 500) |>
  step_tfidf(abstract) |>
  step_downsample(target)

train_prep <- prep(train_rec)
train_prep

# We will save a data frame from the PREP to use later with another algo
train_baked <- bake(train_prep, new_data = NULL)
# write.csv(train_baked, "data/abstracts_baked.csv", row.names = FALSE)
dim(train_baked)

## Cross Validation Split of Training Data
set.seed(888)
train_folds <- vfold_cv(data = training_data, v = 10)
train_folds

# Lasso
lasso_spec <- multinom_reg(
  penalty = tune(),
  mixture = 1
) |>
  set_mode("classification") |>
  set_engine("nnet")

# Support Vector Machine - polynomial degree = 1
svmlinear_spec <- svm_poly(
  degree = 1,
  cost = tune()
) |>
  set_mode("classification") |>
  set_engine("kernlab")

# Support Vector Machine - radial basis function
svmrbf_spec <- svm_rbf(
  cost = tune(),
  rbf_sigma = tune()
) |>
  set_mode("classification") |>
  set_engine("kernlab")

# Random Forest
randomf_spec <- rand_forest(
  mtry = tune(),
  trees = tune(),
  min_n = tune()
) |>
  set_mode("classification") |>
  set_engine("ranger")

# XGBoost
xgboost_spec <- boost_tree(
  trees = tune(),
  mtry = tune(),
  tree_depth = tune(),
  learn_rate = .01
) |>
  set_mode("classification") |>
  set_engine("xgboost")

# Neural network
nnet_spec <- mlp(
  epochs = 30,
  hidden_units = tune(),
  dropout = tune()
) |>
  set_mode("classification") |>
  set_engine("keras", verbose = 2)


# Set up workflow set
workflow_sets <- workflow_set(
  preproc = list(train_rec),
  models = list(
    # lasso_spec,
    svmrbf_spec,
    xgboost_spec,
    randomf_spec,
    nnet_spec
  ),
  cross = TRUE
)
workflow_sets

num_cores <- parallel::detectCores() - 1
RUN <- TRUE
if (RUN) {
  doParallel::registerDoParallel(cores = num_cores)
  start_time <- Sys.time()
  start_time
  fit_workflows <- workflow_sets |>
    workflow_map(
      seed = 888,
      fn = "tune_grid",
      grid = 5,
      resamples = train_folds,
      verbose = TRUE
    )
  end_time <- Sys.time()
  time_taken <- end_time - start_time
  time_taken
  doParallel::stopImplicitCluster()
}
fit_workflows
if (RUN) {
  saved_abstract_modelset <- fit_workflows
  saveRDS(saved_abstract_modelset, "saved_abstract_modelset_1000_docs.rds")
}

if (!RUN) {
  fit_workflows <- readRDS("saved_abstract_modelset_1000_docs.rds")
}

# autoplot(fit_workflows)

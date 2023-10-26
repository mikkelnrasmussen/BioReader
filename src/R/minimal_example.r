## Call libraries
library("optparse")
library("tidyverse")
library("textrecipes") # not on DTU server
library("tidymodels") # not on DTU server
library("discrim") # not on DTU server
library("plyr")
library("baguette")
library("rules")
library("readxl")
library("themis")
library("parallel")
library("doFuture")

option_list <- list(
  make_option(
    c("-a", "--target"),
    type = "character",
    default = "class",
    help = "The target variable to train a model for [default: %default]"
  ),
  make_option(
    c("-t", "--test"),
    action = "store_true",
    default = FALSE,
    help = "Whether to subsample the data for testing [default: %default]"
  ),
  make_option(
    c("-c", "--hpc"),
    action = "store_true",
    default = FALSE,
    help = "If running on a HPC system like gbar [default: %default]"
  )
)

opt_parser <- OptionParser(option_list = option_list)
opt <- parse_args(opt_parser)

# Access named arguments
cat(paste("Target:", opt$target), fill = TRUE)
cat(paste("Test:", opt$test), fill = TRUE)
cat(paste("HPC:", opt$hpc), fill = TRUE)

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

###################################################################
######################## Modelling ################################
###################################################################

# Create training and test set
set.seed(123)
data_split <- initial_split(df_main, strata = target, prop = 0.80)
training_data <- training(data_split)
testing_data <- testing(data_split)

# Setup the preprocessing steps
train_rec <- recipe(target ~ ., data = training_data) |>
  update_role(pmid, new_role = "id") |>
  step_tokenize(abstract) |>
  step_stopwords(abstract) |>
  step_stem(abstract) |>
  step_tokenfilter(abstract, max_tokens = 500) |>
  step_tfidf(abstract)


train_prep <- prep(train_rec)
train_baked <- bake(train_prep, new_data = NULL)

# Random Forest
rf_spec <- rand_forest(
  mtry = tune(),
  trees = tune(),
  min_n = tune()
) |>
  set_mode("classification") |>
  set_engine("ranger")

# Create a workflow
workflow <- workflow() %>%
  add_model(rf_spec) %>%
  add_recipe(train_rec)

# Create a grid of tuning parameters
grid_ctrl <-
  control_grid(
    save_pred = TRUE,
    parallel_over = "everything",
    save_workflow = TRUE
  )

# Create a grid of tuning parameters
grid <- grid_regular(
  mtry(range = c(1, floor(sqrt(ncol(train_baked) - 1)))),
  min_n(range = c(1, 15)),
  trees(range = c(1000, 2000)),
  levels = 5 # This creates a 5x5 grid, with 5 levels for each parameter
)

train_folds <- vfold_cv(data = training_data, v = 5)
if (opt$hpc) {
  num_cores <- as.numeric(Sys.getenv("LSB_DJOB_NUMPROC"))
  doParallel::registerDoParallel(cores = num_cores)
  registerDoFuture()
  cl <- makeCluster(numCores)
  cl
  plan(cluster, workers = cl)
} else {
  num_cores <- detectCores()
  doParallel::registerDoParallel(cores = num_cores)
}


start_time <- Sys.time()
start_time

# Tune the model paramters
tune_results <- tune_grid(
  workflow,
  resamples = train_folds,
  grid = grid,
  control = grid_ctrl,
  metrics = metric_set(accuracy, roc_auc)
)
end_time <- Sys.time()
time_taken <- end_time - start_time
time_taken

if (opt$hpc) {
  parallel::stopCluster(cl)
} else {
  doParallel::stopImplicitCluster()
}

tune_results %>%
  collect_metrics() %>%
  filter(.metric == "accuracy") %>%
  select(mean, min_n, trees, mtry) %>%
  pivot_longer(min_n:mtry,
    values_to = "value",
    names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "accuracy")

tune_results %>%
  collect_predictions() |>
  colnames()
inner_join(final_param) %>%
  group_by(id) %>%
  conf_mat(truth = target, estimate = .pred_class) %>%
  mutate(tidied = map(conf_mat, tidy)) %>%
  unnest(tidied)

saved_abstract_modelset <- tune_results
saveRDS(saved_abstract_modelset, "results/saved_abstract_modelset_minimal_example.rds")

# Select the best model based on accuracy
best_model <- tune_results |>
  select_best("accuracy")

# Create the final workflow with the paramters found via CV
final_wf <- workflow |>
  finalize_workflow(best_model)

# Fit the best model on the training data and evaluate on the test data
final_fit <- final_wf %>%
  last_fit(data_split)

# Collect the predictions from the final model fit
predictions <- final_fit %>%
  collect_predictions()

# Create confusion matrix
conf_matrix <- predictions %>%
  conf_mat(truth = target, estimate = .pred_class)

# Creat a plot of the confusion matrix
plot_conf <- conf_matrix |>
  autoplot(type = "heatmap") +
  scale_fill_gradient(
    low = "white",
    high = "lightblue",
    name = "Count"
  ) +
  labs(title = "Confusio  n Matrix", x = "Predicted labels", y = "True labels") +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major = element_line(colour = "grey85", linetype = "solid"),
    panel.grid.minor = element_line(colour = "grey85", linetype = "dotted"),
    panel.background = element_rect(fill = "white"),
    legend.position = "right",
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 12)
  )

# Save the plot
ggsave(
  filename = "results/rf_fine_tune_class_confusion_matrix.png",
  plot = plot_conf
)

# Create a performance metrics matrix
metrics_mat <- confusionMatrix(
  predictions$.pred_class,
  predictions$target
)
conf_table <- metrics_mat$table |>
  as.table() |>
  as.data.frame()

metrics_by_class <- metrics_mat$byClass |>
  as.table() |>
  as.data.frame()

overall_stats <- metrics_mat$overall |>
  as.table() |>
  as.data.frame()

# Save results
write_csv(conf_table, file = "results/rf_confusion_matrix_table.csv")
write_csv(metrics_by_class, file = "results/rf_metric_by_class.csv")
write_csv(overall_stats, file = "results/rf_overall_stats.csv")

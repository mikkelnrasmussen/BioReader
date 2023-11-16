library("tidyverse")
library("tidymodels")
library("here")

####################################################################
############## Class classification - 1000 samples #################
####################################################################

# Read in overall stat reaults
overall_stats_1 <- read_csv(here("results/rf_overall_stats_1000.csv"))
View(overall_stats_1)

# Read in by class reaults
metrics_by_class_1 <- read_csv(here("results/rf_metric_by_class_1000.csv"))
View(metrics_by_class_1)

####################################################################
############## Class classification - all data #####################
####################################################################

# Read in overall stat reaults
overall_stats_2 <- read_csv(here("results/rf_overall_stats_test1.csv"))
View(overall_stats_2)

# Read in by class reaults
metrics_by_class_2 <- read_csv(here("results/rf_metric_by_class_test1.csv"))
View(metrics_by_class_2)

tune_results2 <- read_rds("results/saved_abstract_modelset_minimal_example_test1.rds")

collect_metrics(tune_results2) |>
  filter(.metric == "accuracy") |>
  arrange(desc(mean)) |>
  view()

####################################################################
###### Class classification - 1000 samples - tune max_tokens #######
####################################################################

# Read in overall stat reaults
overall_stats_3 <- read_csv(here("results/rf_overall_stats_class_1000.csv"))
View(overall_stats_3)

# Read in by class reaults
metrics_by_class_3 <- read_csv(here("results/rf_metric_by_target_class_1000.csv"))
View(metrics_by_class_3)

tune_results3 <- read_rds("results/saved_abstract_modelset_minimal_example.rds")

collect_metrics(tune_results3) |>
  filter(.metric == "accuracy") |>
  arrange(desc(mean)) |>
  view()

# Select the best model based on accuracy
best_model <- tune_results3 |>
  select_best("accuracy")

# Create the final workflow with the paramters found via CV
final_wf <- workflow |>
  finalize_workflow(best_model)

# Fit the best model on the training data and evaluate on the test data
final_fit <- final_wf %>%
  last_fit(data_split)

collect_metrics(tune_results3) |>
  filter(.metric == "accuracy") |>
  arrange(desc(mean))


####################################################################
###### Allergen classification - all data #######
####################################################################

# Cross-validatoin results
tune_results_allergen <- read_rds(
  "results/tuning_results_Allergen_Allergen.rds"
)
collect_metrics(tune_results_allergen) |>
  filter(.metric == "accuracy") |>
  arrange(desc(mean))

# Read in overall stat reaults
overall_stats_allergen <- read_csv(here(
  "results/rf_overall_stats_Allergen.csv"
))
View(overall_stats_allergen)

# Read in by class reaults
metrics_by_class_allergen <- read_csv(here(
  "results/rf_metric_by_target_Allergen.csv"
))
View(metrics_by_class_allergen)

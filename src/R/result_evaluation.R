library("tidyverse")
library("tidymodels")
library("here")

####################################################################
################# Class classification - all data ##################
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
################## Allergen classification - all data ##############
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


####################################################################
############### Autoimmune classification - all data ###############
####################################################################

# Cross-validatoin results
tune_results_autoimmune <- read_rds(
  "results/tuning_results_Autoimm.rds"
)
collect_metrics(tune_results_autoimmune) |>
  filter(.metric == "accuracy") |>
  arrange(desc(mean))

# Read in overall stat reaults
overall_stats_allergen <- read_csv(here(
  "results/rf_overall_stats_Autoimm.csv"
))
View(overall_stats_allergen)

# Read in by class reaults
metrics_by_class_allergen <- read_csv(here(
  "results/rf_metric_by_target_Allergen.csv"
))
View(metrics_by_class_allergen)

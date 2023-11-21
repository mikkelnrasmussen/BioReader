library("tidymodels")
library("readr")
library("textrecipes")


# Load the saved model
model_path <- "results/model_fitted_on_entire_data_class_micro_test.rds"
fitted_model <- readRDS(model_path)

# Load new data for prediction
new_data <- read_csv("data/training_data/ABIOT.csv") |>
  select(PubMed_ID, Abstract) |>
  rename(pmid = PubMed_ID, abstract = Abstract)

# Predict using the fitted model
predictions <- predict(fitted_model, new_data, type)

# The predictions object will contain the predicted classes or probabilities,
# depending on the model
predicted_classes <- predictions$.pred_class

# You can then save or return these predictions as needed
write_csv(predicted_classes, "predicted_classes.csv") # Save predicted classes
write_csv(predicted_probabilities, "predicted_probabilities.csv") # Save predicted probabilities

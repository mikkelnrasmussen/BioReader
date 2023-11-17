## Call libraries
library("tidyverse")
library("readxl")


###################################################################
######################### Load Data ###############################
###################################################################

# First we will need to handle the HIV cateogory, as this data is
# in a different format than the rest of the training data
hiv_data <- read_csv("data/_raw/HIV_curatable_table4.csv")
hiv_data <- hiv_data |>
  select(PubMed_ID, Title, Abstract) |>
  mutate(SubType = "HIV")
write_csv(hiv_data, "data/_raw/training_data/HIV.csv")

# Read in all of the data
file_names <- dir("data/_raw/training_data", full.names = TRUE)
df_all_classes <- file_names |>
  map(\(x) read_csv(x, show_col_types = FALSE)) |>
  bind_rows()
df_class_label <- read_excel(
  "data/_raw/All_Updated_Categories_2019.xlsx",
  na = "NA"
)

# Add the OTV label to other virus
df_class_label <- df_class_label |>
  mutate(
    category = if_else(
      subcategory == "Other_Virus",
      subcategory,
      category
    ),
    subcategory = if_else(
      subcategory == "Other_Virus",
      "OTV",
      subcategory
    )
  )

# If the category label is missing, then add the class
# label instead
df_class_label <- df_class_label |>
  mutate(
    category = if_else(
      is.na(category) & class == "Other",
      str_replace_all(Category, "\\s", "_"),
      if_else(
        subcategory %in% c("MYA", "BETAAM"),
        subcategory,
        category
      )
    )
  )

# If the subcategory label is missing add the one found in the
# Subcatgory column
df_class_label <- df_class_label |>
  mutate(
    subcategory = if_else(
      is.na(subcategory),
      Abbreviation,
      subcategory
    ),
    category = if_else(
      is.na(category),
      class,
      category
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
print(df_all_classes_only) # Only 1-2 papers in these categories, will be ignored

df_class_label_only <- df_class_label |>
  filter(!(subcategory %in% df_all_classes$SubType)) |>
  select(subcategory) |>
  pull() |>
  unique()
print(df_class_label_only)

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

# Save merged and QC'ed dataset
write_csv(df_merged, "data/all_categories_merged.csv")

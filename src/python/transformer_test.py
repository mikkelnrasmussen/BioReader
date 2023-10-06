## Load libraries
# Data wrangling
import pandas as pd
import os
from sklearn.model_selection import train_test_split
from pyprojroot.here import here

# Model training
import torch
from sklearn.preprocessing import LabelEncoder, label_binarize
from transformers import BertTokenizer, BertForSequenceClassification, AutoModelForSequenceClassification, AdamW, get_scheduler
from torch.utils.data import DataLoader, TensorDataset, Dataset
from tqdm.auto import tqdm
from datasets import load_metric

# Model evaluation
from sklearn.metrics import confusion_matrix, classification_report, roc_curve, auc, precision_recall_curve
import seaborn as sns
import matplotlib.pyplot as plt
import numpy as np
from sklearn.metrics import roc_auc_score, average_precision_score


## Preprocessing
# Load the training data and combine
data_path = here("data/training_data")
all_files = [os.path.join(data_path, f) for f in os.listdir(data_path) if f.endswith(".csv")]
df_list = [pd.read_csv(file) for file in all_files]
combined_df = pd.concat(df_list, ignore_index=True)

# Load the excel file containing category information
category_info = pd.read_excel(here("data/All_Updated_Categories_2019_edited_by_mikkel.xlsx"))

# Merge the dataframes
merged_df = pd.merge(
  combined_df, 
  category_info, 
  left_on = "SubType", 
  right_on = "subcategory",
  how = "left"
)

# Check the number of documents for each class, category, and subcategory
class_counts = merged_df['class'].value_counts()
subcategory_counts = merged_df['subcategory'].value_counts()

print("Class Counts:\n", class_counts)
print("\nSubcategory Counts:\n", subcategory_counts)

missing_values = merged_df.isnull().sum()
print("Missing Values:\n", missing_values)


# Create a boolean mask for rows where the abstract starts with the specified string
mask = merged_df['Abstract'].str.startswith("[Data extracted from this article was imported")

# Filter out these rows
filtered_df = merged_df[~mask]

# Check the number of rows before and after filtering
print(f"Number of rows before filtering: {len(merged_df)}")
print(f"Number of rows after filtering: {len(filtered_df)}")
print(f"Difference: {len(merged_df) - len(filtered_df)}")

missing_values = filtered_df.isnull().sum()
print("Missing Values:\n", missing_values)

### Split the Data for the class variable
target = "class"

# Drop rows where 'class' is NA
filtered_df = filtered_df.dropna(subset=[target])

# Check the number of rows after dropping
print(f"Number of rows after dropping NAs in target: {len(filtered_df)}")

# Split the data into train and a temporary dataset (70% train, 30% temp)
train_df, temp_df = train_test_split(filtered_df, test_size=0.3, random_state=42, stratify=filtered_df[target])

# Split the temporary dataset into validation and test datasets (50% validation, 50% test from the temp dataset)
eval_df, test_df = train_test_split(temp_df, test_size=0.5, random_state=42, stratify=temp_df[target])

# Initialize the label encoder
label_encoder = LabelEncoder()

# Transform class label from string to integers
# Fit the encoder on the 'class' column and transform it
train_df['target_int'] = label_encoder.fit_transform(train_df[target])
eval_df['target_int'] = label_encoder.transform(eval_df[target])
test_df['target_int'] = label_encoder.transform(test_df[target])


# Check the number of rows in each dataset
print(f"Number of rows in train dataset: {len(train_df)}")
print(f"Number of rows in validation dataset: {len(eval_df)}")
print(f"Number of rows in test dataset: {len(test_df)}")

# Check the number of documents from each class in the train dataset
train_class_counts = train_df[target].value_counts()

# Check the number of documents from each class in the validation dataset
valid_class_counts = eval_df[target].value_counts()

# Check the number of documents from each class in the test dataset
test_class_counts = test_df[target].value_counts()

print("Number of documents from each class in train dataset:\n", train_class_counts)
print("\nNumber of documents from each class in validation dataset:\n", valid_class_counts)
print("\nNumber of documents from each class in test dataset:\n", test_class_counts)

# Load BioBERT and tokenizer
model_name = "dmis-lab/biobert-base-cased-v1.2"
tokenizer = BertTokenizer.from_pretrained(model_name)
num_labels = len(train_df['target_int'].unique())
model = BertForSequenceClassification.from_pretrained(model_name, num_labels=num_labels)

# 1. Tokenization
data_size = 1000
train_abstract = train_df['Abstract'].tolist()[1:data_size]
eval_abstract = eval_df['Abstract'].tolist()[1:data_size]
tokenized_train = tokenizer(train_abstract, padding=True, truncation=True, return_tensors="pt", max_length=512)
tokenized_eval = tokenizer(eval_abstract, padding=True, truncation=True, return_tensors="pt", max_length=512)

class CustomDataset(Dataset):
    def __init__(self, encodings, labels):
        self.encodings = encodings
        self.labels = labels

    def __getitem__(self, idx):
        item = {key: val[idx].clone().detach() for key, val in self.encodings.items()}
        item['labels'] = torch.tensor(self.labels[idx])
        return item

    def __len__(self):
        return len(self.labels)


# Convert tokenized data to PyTorch dataset
train_labels = train_df['target_int'].tolist()[1:data_size]
eval_labels = eval_df["target_int"].tolist()[1:data_size]
train_dataset = CustomDataset(encodings = tokenized_train, labels = train_labels)
eval_dataset = CustomDataset(encodings = tokenized_eval, labels = eval_labels)

# Create DataLoader
train_dataloader = DataLoader(train_dataset, shuffle=True, batch_size=8)
eval_dataloader = DataLoader(eval_dataset, batch_size=8)


# 2. Training Loop
optimizer = torch.optim.AdamW(model.parameters(), lr=5e-5)
num_epochs = 1
num_training_steps = num_epochs * len(train_dataloader)
lr_scheduler = get_scheduler(
    "linear", 
    optimizer=optimizer, 
    num_warmup_steps=0, 
    num_training_steps=num_training_steps
)

device = torch.device("cuda") if torch.cuda.is_available() else torch.device("cpu")
model.to(device)

progress_bar = tqdm(range(num_training_steps))

model.train()

# For accumulating loss over the epoch
total_loss = 0.0

for epoch in range(num_epochs):
    for batch in train_dataloader:
        batch = {k: v.to(device) for k, v in batch.items()}
        outputs = model(**batch)
        loss = outputs.loss
        loss.backward()

        optimizer.step()
        lr_scheduler.step()
        optimizer.zero_grad()
        
        # Add the batch loss to the total loss
        total_loss += loss.item()

        progress_bar.update(1)
        
        # Print loss for the current batch
        print(f"Batch Loss: {loss.item():.4f}")
    
    # Print average loss for the epoch
    avg_loss = total_loss / len(train_dataloader)
    print(f"Epoch {epoch+1} Average Loss: {avg_loss:.4f}")
    
    # Reset total_loss for the next epoch
    total_loss = 0.0


# 3. Evaluation
metric = load_metric("accuracy")

# Store probabilities, predictions and true labels
all_scores = []
all_predictions = []
all_true_labels = []

model.eval()
for batch in eval_dataloader:
    batch = {k: v.to(device) for k, v in batch.items()}
    with torch.no_grad():
        outputs = model(**batch)
    logits = outputs.logits

    # Get probabilities and predictions
    scores = torch.nn.functional.softmax(logits, dim=1).cpu().numpy()
    predictions = torch.argmax(logits, dim=-1).cpu().numpy()

    # For computing the metric
    metric.add_batch(predictions=predictions, references=batch["labels"])

    # Save probabilites, predictions and labels
    all_scores.extend(scores)
    all_predictions.extend(predictions)
    all_true_labels.extend(batch["labels"].cpu().numpy())

print(metric.compute())

# Generate the confusion matrix
cm = confusion_matrix(all_true_labels, all_predictions)

# Plot the confusion matrix
plt.figure(figsize=(10, 10))
sns.heatmap(cm, annot=True, fmt='g', cmap='Blues', 
            xticklabels=label_encoder.classes_, 
            yticklabels=label_encoder.classes_)
plt.xlabel('Predicted labels')
plt.ylabel('True labels')
plt.title('Confusion Matrix')
plt.savefig(here("results/biobert_fine_tune_1000_class_confusion_matrix.png"))

# Save classification report results
metrics = classification_report(all_true_labels, all_predictions, target_names=label_encoder.classes_)
with open(here("results/biobert_classification_report_1000.csv"), "w") as text_file:
    text_file.write(metrics)

roc_aucs = []
pr_aucs = []

# Binarize the labels for multi-class AUC calculation
true_labels_bin = label_binarize(all_true_labels, classes=np.arange(len(label_encoder.classes_)))

# Calculate AUC for each class
for i in range(len(label_encoder.classes_)):
    roc_auc = roc_auc_score(true_labels_bin[:, i], np.array(all_scores)[:, i])
    pr_auc = average_precision_score(true_labels_bin[:, i], np.array(all_scores)[:, i])
    
    roc_aucs.append(roc_auc)
    pr_aucs.append(pr_auc)


# Create a DataFrame
df = pd.DataFrame({
    'Class': label_encoder.classes_,
    'ROC AUC': roc_aucs,
    'PR AUC': pr_aucs
})

# Display the table
df.to_csv(here("results/biobert_roc_pr_auc_1000.csv"))


# Binarize the labels for multi-class ROC
true_labels_bin = label_binarize(all_true_labels, classes=np.arange(len(label_encoder.classes_)))

plt.figure(figsize=(10, 8))

# Compute ROC curve and ROC area for each class
for i in range(len(label_encoder.classes_)):
    fpr, tpr, _ = roc_curve(true_labels_bin[:, i], np.array(all_scores)[:, i])
    roc_auc = auc(fpr, tpr)
    plt.plot(fpr, tpr, label=f'Class {label_encoder.classes_[i]} (area = {roc_auc:.2f})')

plt.plot([0, 1], [0, 1], 'k--')
plt.xlim([0.0, 1.0])
plt.ylim([0.0, 1.05])
plt.xlabel('False Positive Rate')
plt.ylabel('True Positive Rate')
plt.title('ROC Curve')
plt.legend(loc="lower right")
plt.savefig(here("results/biobert_fine_tune_1000_class_roc_curve.png"))

plt.figure(figsize=(10, 8))

# Compute Precision-Recall and plot curve for each class
for i in range(len(label_encoder.classes_)):
    precision, recall, _ = precision_recall_curve(true_labels_bin[:, i], np.array(all_scores)[:, i])
    avg_precision = average_precision_score(true_labels_bin[:, i], np.array(all_scores)[:, i])
    plt.plot(recall, precision, label=f'Class {label_encoder.classes_[i]} (avg. precision = {avg_precision:.2f})')

plt.xlabel('Recall')
plt.ylabel('Precision')
plt.title('Precision-Recall Curve')
plt.legend(loc="upper right")
plt.grid(True)
plt.savefig(here("results/biobert_fine_tune_1000_class_pr_curve.png"))

# Convert predictions and true labels to a numpy array for easier indexing
predicted_classes = np.array(all_predictions)
true_classes = np.array(all_true_labels)

# Number of classes
num_classes = len(label_encoder.classes_)

all_scores_np = np.array(all_scores)

# Plot histograms for confidence scores of each class separately
fig, axes = plt.subplots(num_classes, 1, figsize=(10, 5 * num_classes))

for i in range(num_classes):
    # Correct predictions for this class
    correct_mask = (predicted_classes == i) & (true_classes == i)
    incorrect_mask = (predicted_classes != i) & (true_classes == i)
    
    axes[i].hist(all_scores_np[correct_mask, i], bins=50, color='green', alpha=0.7, label='Correct Predictions')
    axes[i].hist(all_scores_np[incorrect_mask, i], bins=50, color='red', alpha=0.7, label='Incorrect Predictions')
    
    axes[i].set_title(f'Confidence Scores for Class {label_encoder.classes_[i]}')
    axes[i].set_xlabel('Confidence Score')
    axes[i].set_ylabel('Number of Samples')
    axes[i].legend()
    axes[i].grid(True)

plt.tight_layout()
plt.show()





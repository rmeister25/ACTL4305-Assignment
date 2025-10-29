import pandas as pd
import numpy as np
import torch
import torch.nn as nn
import torch.nn.functional as F
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import StandardScaler
from sklearn.metrics import roc_auc_score
import matplotlib.pyplot as plt
import seaborn as sns
from sklearn.metrics import roc_curve, auc
from sklearn.metrics import f1_score, precision_score, recall_score

# Your model class
class Model(nn.Module):
    def __init__(self, in_features=44, h1=64, h2=32, out_features=1):
        super(Model, self).__init__()
        self.fc1 = nn.Linear(in_features, h1)
        self.dropout1 = nn.Dropout(0.3)  # Add dropout
        self.fc2 = nn.Linear(h1, h2)
        self.dropout2 = nn.Dropout(0.2)  # Add dropout
        self.out = nn.Linear(h2, out_features)

    def forward(self, x):
        x = F.relu(self.fc1(x))
        x = self.dropout1(x)  # Apply dropout
        x = F.relu(self.fc2(x))
        x = self.dropout2(x)  # Apply dropout
        x = torch.sigmoid(self.out(x))
        return x

# Load and prepare training data
df_train = pd.read_csv('C:/Users/Leon Chai/OneDrive - UNSW/Documents/GitHub/ACTL4305-Assignment/training_SMOTE_dataset.csv')
df_train = df_train.drop(df_train.columns[0], axis=1)

X = df_train.drop('convert', axis=1)
X = df_train.drop('convert', axis=1)
# X = X.drop('continent_vector', axis=1)
# X = X.drop('boost_coverage_vector', axis=1)
X = X.astype(float)
y = df_train['convert']

# Get feature names BEFORE converting to numpy
feature_names = X.columns.tolist()
print(f"Features: {feature_names}")

# Convert to numpy arrays
X = X.values
y = y.values

X_train = X
y_train = y

# Load and prepare test data
df_test = pd.read_csv('C:/Users/Leon Chai/OneDrive - UNSW/Documents/GitHub/ACTL4305-Assignment/test_scaled_dataset.csv')
df_test = df_test.drop(df_test.columns[0], axis=1)

X_test = df_test.drop('convert', axis=1)
X_test = X_test.astype(float)
y_test = df_test['convert']

feature_names = X_test.columns.tolist()
print(f"Features: {feature_names}")

# Convert to numpy arrays
X_test = X_test.values
y_test = y_test.values

# # Split and scale data
# X_train, X_test, y_train, y_test = train_test_split(
#     X, y, test_size=0.2, random_state=123, stratify=y
# )

scaler = StandardScaler()
X_train_scaled = scaler.fit_transform(X_train)
X_test_scaled = scaler.transform(X_test)

# Convert to tensors
X_train_tensor = torch.FloatTensor(X_train_scaled)
X_test_tensor = torch.FloatTensor(X_test_scaled)
y_train_tensor = torch.FloatTensor(y_train).view(-1, 1)
y_test_tensor = torch.FloatTensor(y_test).view(-1, 1)

# Train model
torch.manual_seed(123)
model = Model(in_features=X_train_scaled.shape[1])

criterion = nn.BCELoss()
optimizer = torch.optim.Adam(model.parameters(), lr=0.001, weight_decay=1e-3)


epochs = 90
train_losses = []
val_losses = []

for i in range(epochs):
    # --- Training phase ---
    model.train()
    y_pred = model(X_train_tensor)
    loss = criterion(y_pred, y_train_tensor)
    train_losses.append(loss.detach().numpy())

    optimizer.zero_grad()
    loss.backward()
    optimizer.step()

    # --- Validation phase ---
    model.eval()
    with torch.no_grad():
        y_val_pred = model(X_test_tensor)  # using your test set as validation
        val_loss = criterion(y_val_pred, y_test_tensor)
        val_losses.append(val_loss.detach().numpy())

    if i % 10 == 0:
        print(f"Epoch {i:03d} | Train Loss: {loss:.4f} | Val Loss: {val_loss:.4f}")

# --- Plot both losses ---
plt.figure(figsize=(10, 6))
plt.plot(train_losses, 'b-', linewidth=2, label='Training Loss')
plt.plot(val_losses, 'orange', linewidth=2, label='Validation Loss')
plt.title('Training vs. Validation Loss')
plt.xlabel('Epoch')
plt.ylabel('Loss (BCE)')
plt.grid(True, alpha=0.3)
plt.legend()
plt.show()



# Predictions for TEST data
with torch.no_grad():
    test_pred = model(X_test_tensor)
    test_pred_proba = test_pred.numpy()
    test_pred_binary = (test_pred_proba > 0.5).astype(int)

# CALCULATE TEST LOSS
test_loss = criterion(test_pred, y_test_tensor)

print("\n" + "="*60)
print("LOSS ANALYSIS")
print("="*60)
print(f"Final Training Loss: {train_losses[-1]:.4f}")
print(f"Test Loss: {test_loss.item():.4f}")
print(f"Loss Difference: {test_loss.item() - train_losses[-1]:.4f}")


# Permutation Importance Function
def permutation_importance(model, X_test, y_test, feature_names, n_repeats=5, random_state=42):
    """
    Calculate permutation importance for neural network
    """
    np.random.seed(random_state)
    model.eval()
    
    # Get baseline score
    with torch.no_grad():
        X_tensor = torch.FloatTensor(X_test)
        y_pred = model(X_tensor).numpy()
        baseline_score = roc_auc_score(y_test, y_pred)
    
    print(f"Baseline AUC-ROC: {baseline_score:.4f}")
    
    n_features = X_test.shape[1]
    importances = np.zeros(n_features)
    stds = np.zeros(n_features)
    
    for feature_idx in range(n_features):
        print(f"Processing {feature_idx+1}/{n_features}: {feature_names[feature_idx]}")
        feature_scores = []
        
        for repeat in range(n_repeats):
            # Create shuffled copy
            X_shuffled = X_test.copy()
            X_shuffled[:, feature_idx] = np.random.permutation(X_shuffled[:, feature_idx])
            
            # Calculate score with shuffled feature
            with torch.no_grad():
                X_tensor = torch.FloatTensor(X_shuffled)
                y_pred_shuffled = model(X_tensor).numpy()
                shuffled_score = roc_auc_score(y_test, y_pred_shuffled)
            
            importance_score = baseline_score - shuffled_score
            feature_scores.append(importance_score)
        
        importances[feature_idx] = np.mean(feature_scores)
        stds[feature_idx] = np.std(feature_scores)
    
    results_df = pd.DataFrame({
        'feature': feature_names,
        'importance': importances,
        'std': stds,
        'importance_abs': np.abs(importances)
    }).sort_values('importance_abs', ascending=False)
    
    return results_df

# RUN PERMUTATION IMPORTANCE
print("\n" + "="*50)
print("CALCULATING PERMUTATION IMPORTANCE")
print("="*50)

# Use n_repeats=5 for faster results, increase to 10-20 for more accuracy
importance_results = permutation_importance(
    model=model,
    X_test=X_test_scaled,
    y_test=y_test,
    feature_names=feature_names,
    n_repeats=5,
    random_state=42
)

# DISPLAY RESULTS
print("\n" + "="*60)
print("TOP 15 MOST IMPORTANT FEATURES FOR CONVERSION")
print("="*60)
print("\nPositive importance = feature helps prediction")
print("Negative importance = feature hurts prediction when shuffled\n")

for i, row in importance_results.head(15).iterrows():
    print(f"{i+1:2d}. {row['feature']:30s} : {row['importance']:7.4f} ± {row['std']:.4f}")

# VISUALIZE RESULTS
plt.figure(figsize=(12, 10))

# Plot 1: Top 15 features
plt.subplot(2, 1, 1)
top_features = importance_results.head(15)
colors = ['red' if x < 0 else 'blue' for x in top_features['importance']]

plt.barh(range(len(top_features)), top_features['importance'], 
         color=colors, alpha=0.7, xerr=top_features['std'])
plt.yticks(range(len(top_features)), top_features['feature'])
plt.xlabel('Permutation Importance (AUC Drop)')
plt.title('Top 15 Features Affecting Conversion Rate\n(Blue = Positive Impact, Red = Negative Impact)')
plt.grid(axis='x', alpha=0.3)
plt.gca().invert_yaxis()

# Plot 2: Feature importance distribution
plt.subplot(2, 1, 2)
plt.hist(importance_results['importance'], bins=20, alpha=0.7, color='green', edgecolor='black')
plt.xlabel('Permutation Importance')
plt.ylabel('Number of Features')
plt.title('Distribution of Feature Importances')
plt.axvline(x=0, color='red', linestyle='--', alpha=0.8)
plt.grid(alpha=0.3)

plt.tight_layout()
plt.show()

# ADDITIONAL ANALYSIS
print("\n" + "="*50)
print("ADDITIONAL INSIGHTS")
print("="*50)

# Features with significant positive impact
positive_impact = importance_results[importance_results['importance'] > 0.001]
print(f"\nFeatures with STRONG positive impact (> 0.001): {len(positive_impact)}")
print("Top 5:")
for i, row in positive_impact.head(5).iterrows():
    print(f"  - {row['feature']}: {row['importance']:.4f}")

# Features with negative impact
negative_impact = importance_results[importance_results['importance'] < -0.001]
print(f"\nFeatures with NEGATIVE impact (< -0.001): {len(negative_impact)}")
if len(negative_impact) > 0:
    print("These might need investigation:")
    for i, row in negative_impact.iterrows():
        print(f"  - {row['feature']}: {row['importance']:.4f}")

# Save results
importance_results.to_csv('conversion_feature_importance.csv', index=False)
print(f"\nResults saved to 'conversion_feature_importance.csv'")




from sklearn.metrics import confusion_matrix, classification_report
import matplotlib.pyplot as plt
import seaborn as sns
import numpy as np

# Set model to evaluation mode
model.eval()

def plot_confusion_matrix(y_true, y_pred, title):
    """Plot confusion matrix"""
    cm = confusion_matrix(y_true, y_pred)
    
    plt.figure(figsize=(6, 5))
    sns.heatmap(cm, annot=True, fmt='d', cmap='Blues', 
                xticklabels=['Predicted 0', 'Predicted 1'],
                yticklabels=['Actual 0', 'Actual 1'])
    plt.title(title)
    plt.ylabel('True Label')
    plt.xlabel('Predicted Label')
    plt.show()
    
    return cm

# Get predictions for TRAINING data
with torch.no_grad():
    # Training data predictions
    train_pred = model(X_train_tensor)
    train_pred_proba = train_pred.numpy()
    train_pred_binary = (train_pred_proba > 0.5).astype(int)
    
    # Test data predictions  
    test_pred = model(X_test_tensor)
    test_pred_proba = test_pred.numpy()
    test_pred_binary = (test_pred_proba > 0.5).astype(int)

# Convert y_train and y_test to numpy for sklearn
y_train_np = y_train_tensor.numpy().flatten()
y_test_np = y_test_tensor.numpy().flatten()

print("="*60)
print("TRAINING DATA RESULTS")
print("="*60)

# Training data confusion matrix
train_cm = plot_confusion_matrix(y_train_np, train_pred_binary, 
                                'After SMOTE Confusion Matrix - Training Data')

# Training data metrics
train_accuracy = (train_cm[0,0] + train_cm[1,1]) / train_cm.sum()
train_precision = train_cm[1,1] / (train_cm[1,1] + train_cm[0,1]) if (train_cm[1,1] + train_cm[0,1]) > 0 else 0
train_recall = train_cm[1,1] / (train_cm[1,1] + train_cm[1,0]) if (train_cm[1,1] + train_cm[1,0]) > 0 else 0
train_f1 = 2 * (train_precision * train_recall) / (train_precision + train_recall) if (train_precision + train_recall) > 0 else 0

print(f"Training Accuracy: {train_accuracy:.3f}")
print(f"Training Precision: {train_precision:.3f}")
print(f"Training Recall: {train_recall:.3f}")
print(f"Training F1-Score: {train_f1:.3f}")
print(f"Training AUC: {roc_auc_score(y_train_np, train_pred_proba):.3f}")

print("\n" + "="*60)
print("TEST DATA RESULTS")
print("="*60)

# Test data confusion matrix
test_cm = plot_confusion_matrix(y_test_np, test_pred_binary, 
                               'After SMOTE Confusion Matrix - Test Data')

# Test data metrics
test_accuracy = (test_cm[0,0] + test_cm[1,1]) / test_cm.sum()
test_precision = test_cm[1,1] / (test_cm[1,1] + test_cm[0,1]) if (test_cm[1,1] + test_cm[0,1]) > 0 else 0
test_recall = test_cm[1,1] / (test_cm[1,1] + test_cm[1,0]) if (test_cm[1,1] + test_cm[1,0]) > 0 else 0
test_f1 = 2 * (test_precision * test_recall) / (test_precision + test_recall) if (test_precision + test_recall) > 0 else 0

print(f"Test Accuracy: {test_accuracy:.3f}")
print(f"Test Precision: {test_precision:.3f}")
print(f"Test Recall: {test_recall:.3f}")
print(f"Test F1-Score: {test_f1:.3f}")
print(f"Test AUC: {roc_auc_score(y_test_np, test_pred_proba):.3f}")

print("\n" + "="*60)
print("COMPARISON SUMMARY")
print("="*60)

print(f"{'Metric':<12} {'Training':<10} {'Test':<10}")
print(f"{'-'*12} {'-'*10} {'-'*10}")
print(f"{'Accuracy':<12} {train_accuracy:.3f}     {test_accuracy:.3f}")
print(f"{'Precision':<12} {train_precision:.3f}     {test_precision:.3f}")
print(f"{'Recall':<12} {train_recall:.3f}     {test_recall:.3f}")
print(f"{'F1-Score':<12} {train_f1:.3f}     {test_f1:.3f}")
print(f"{'AUC':<12} {roc_auc_score(y_train_np, train_pred_proba):.3f}     {roc_auc_score(y_test_np, test_pred_proba):.3f}")

# Detailed classification reports
print("\n" + "="*60)
print("DETAILED CLASSIFICATION REPORT - TRAINING DATA")
print("="*60)
print(classification_report(y_train_np, train_pred_binary, target_names=['Class 0', 'Class 1']))

print("\n" + "="*60)
print("DETAILED CLASSIFICATION REPORT - TEST DATA")
print("="*60)
print(classification_report(y_test_np, test_pred_binary, target_names=['Class 0', 'Class 1']))


model.eval()
with torch.no_grad():
    y_train_pred_prob = model(X_train_tensor).squeeze().numpy()
    y_test_pred_prob  = model(X_test_tensor).squeeze().numpy()

# Compute ROC curve and AUC for train
fpr_train, tpr_train, _ = roc_curve(y_train, y_train_pred_prob)
auc_train = auc(fpr_train, tpr_train)

# Compute ROC curve and AUC for test
fpr_test, tpr_test, _ = roc_curve(y_test, y_test_pred_prob)
auc_test = auc(fpr_test, tpr_test)

# Plot ROC curves
plt.figure(figsize=(8,6))
plt.plot(fpr_train, tpr_train, label=f'Train ROC (AUC = {auc_train:.4f})', color='blue')
plt.plot(fpr_test, tpr_test, label=f'Test ROC (AUC = {auc_test:.4f})', color='red')
plt.plot([0,1], [0,1], 'k--')  # diagonal line
plt.xlabel('False Positive Rate')
plt.ylabel('True Positive Rate')
plt.title('ROC Curve')
plt.legend()
plt.grid(True)
plt.show()



model.eval()
with torch.no_grad():
    # Predictions as probabilities
    y_train_pred_prob = model(X_train_tensor).squeeze().numpy()
    y_test_pred_prob  = model(X_test_tensor).squeeze().numpy()

# Ensure labels are numpy arrays
y_train_np = y_train.numpy() if torch.is_tensor(y_train) else y_train
y_test_np  = y_test.numpy()  if torch.is_tensor(y_test) else y_test

# Choose a threshold (0.5 default)
threshold = 0.5
y_train_pred = (y_train_pred_prob >= threshold).astype(int)
y_test_pred  = (y_test_pred_prob  >= threshold).astype(int)

# --- Compute metrics ---
train_f1 = f1_score(y_train_np, y_train_pred)
train_precision = precision_score(y_train_np, y_train_pred)
train_recall = recall_score(y_train_np, y_train_pred)

test_f1 = f1_score(y_test_np, y_test_pred)
test_precision = precision_score(y_test_np, y_test_pred)
test_recall = recall_score(y_test_np, y_test_pred)

# --- Print results ---
print(f"Train -> F1: {train_f1:.4f}, Precision: {train_precision:.4f}, Recall: {train_recall:.4f}")
print(f"Test  -> F1: {test_f1:.4f}, Precision: {test_precision:.4f}, Recall: {test_recall:.4f}")
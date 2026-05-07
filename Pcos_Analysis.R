# ============================================================
# PCOS ANALYSIS AND RISK PREDICTION
# Data Source: Kaggle - PCOS Dataset
# Analyst: Rebecca Agbolade
# Date: May 2026
# ============================================================
# PROJECT AIM:
# To analyse PCOS patient data, identify key risk factors,
# predict PCOS likelihood, identify PCOS subtypes, and
# build an interactive tool to help women understand
# their risk profile and potential PCOS type.
# ============================================================

# ============================================================
# 1. LOAD LIBRARIES
# ============================================================
install.packages("rlang")
install.packages(c("ggcorrplot", "caret", "randomForest", 
                   "pROC", "cluster", "factoextra", 
                   "NbClust", "shinyWidgets", "plotly", "DT"))

library(tidyverse)    # Data cleaning and visualisation
library(ggcorrplot)   # Correlation heatmaps
library(caret)        # Machine learning framework
library(randomForest) # Random forest classification
library(pROC)         # ROC curve evaluation
library(cluster)      # Clustering algorithms
library(factoextra)   # Cluster visualisations
library(shiny)        # Interactive app
library(shinythemes)  # App styling
library(shinyWidgets) # Enhanced UI controls
library(plotly)       # Interactive charts
library(DT)           # Interactive tables

# ============================================================
# 2. LOAD DATA
# ============================================================

# PCOS patient dataset from Kaggle
# 541 patients | 44 variables
# Includes hormones, symptoms, lifestyle and diagnosis
pcos <- read.csv("PCOS_data.csv")

# ============================================================
# 3. INITIAL DATA EXPLORATION
# ============================================================

# --- 3.1 First look at the data ---
head(pcos)
str(pcos)
dim(pcos)

# ============================================================
# 4. DATA CLEANING
# ============================================================
# --- 4.1 Rename columns to clean readable names ---
pcos <- pcos %>%
  rename(
    Patient_ID       = Sl..No,
    File_No          = Patient.File.No.,
    PCOS             = PCOS..Y.N.,
    Age              = Age..yrs.,
    Weight_kg        = Weight..Kg.,
    Height_cm        = Height.Cm.,
    BMI              = BMI,
    Blood_Group      = Blood.Group,
    Pulse_rate       = Pulse.rate.bpm.,
    RR               = RR..breaths.min.,
    Hb               = Hb.g.dl.,
    Cycle            = Cycle.R.I.,
    Cycle_length     = Cycle.length.days.,
    Marriage_years   = Marraige.Status..Yrs.,
    Pregnant         = Pregnant.Y.N.,
    Abortions        = No..of.abortions,
    beta_HCG_1       = I...beta.HCG.mIU.mL.,
    beta_HCG_2       = II....beta.HCG.mIU.mL.,
    FSH              = FSH.mIU.mL.,
    LH               = LH.mIU.mL.,
    FSH_LH           = FSH.LH,
    Hip              = Hip.inch.,
    Waist            = Waist.inch.,
    Waist_Hip_Ratio  = Waist.Hip.Ratio,
    TSH              = TSH..mIU.L.,
    AMH              = AMH.ng.mL.,
    PRL              = PRL.ng.mL.,
    Vit_D3           = Vit.D3..ng.mL.,
    Progesterone     = PRG.ng.mL.,
    RBS              = RBS.mg.dl.,
    Weight_gain      = Weight.gain.Y.N.,
    Hair_growth      = hair.growth.Y.N.,
    Skin_darkening   = Skin.darkening..Y.N.,
    Hair_loss        = Hair.loss.Y.N.,
    Pimples          = Pimples.Y.N.,
    Fast_food        = Fast.food..Y.N.,
    Exercise         = Reg.Exercise.Y.N.,
    BP_Systolic      = BP._Systolic..mmHg.,
    BP_Diastolic     = BP._Diastolic..mmHg.,
    Follicle_L       = Follicle.No...L.,
    Follicle_R       = Follicle.No...R.,
    Follicle_size_L  = Avg..F.size..L...mm.,
    Follicle_size_R  = Avg..F.size..R...mm.,
    Endometrium      = Endometrium..mm.
  )

# Verify renaming worked
names(pcos)

# --- 4.2 Remove empty X column ---
pcos <- pcos %>%
  select(-X)

# --- 4.3 Convert character columns to numeric ---
pcos <- pcos %>%
  mutate(
    beta_HCG_2 = as.numeric(beta_HCG_2),
    AMH        = as.numeric(AMH)
  )

# --- 4.4 Convert PCOS to factor ---
pcos$PCOS <- as.factor(pcos$PCOS)

# --- 4.5 Convert Cycle to readable labels ---
pcos$Cycle <- ifelse(pcos$Cycle == 2, "Regular", "Irregular")
pcos$Cycle <- as.factor(pcos$Cycle)

# Verify changes
str(pcos %>% select(PCOS, Cycle, beta_HCG_2, AMH))

# --- 4.6 Check for missing values ---
colSums(is.na(pcos))

# --- 4.7 Check PCOS diagnosis split ---
table(pcos$PCOS)
prop.table(table(pcos$PCOS))

colSums(is.na(pcos))

# --- 4.8 Handle missing values ---
# Only 4 missing values across entire dataset
# Replace each with the column median to preserve data distribution

pcos <- pcos %>%
  mutate(
    Marriage_years = ifelse(is.na(Marriage_years), 
                            median(Marriage_years, na.rm = TRUE), 
                            Marriage_years),
    beta_HCG_2     = ifelse(is.na(beta_HCG_2), 
                            median(beta_HCG_2, na.rm = TRUE), 
                            beta_HCG_2),
    AMH            = ifelse(is.na(AMH), 
                            median(AMH, na.rm = TRUE), 
                            AMH),
    Fast_food      = ifelse(is.na(Fast_food), 
                            median(Fast_food, na.rm = TRUE), 
                            Fast_food)
  )

# Verify no missing values remain
sum(is.na(pcos))

# ============================================================
# 5. EXPLORATORY DATA ANALYSIS
# ============================================================

# --- 5.1 Compare key variables between PCOS and non PCOS ---
# Compare average hormone levels by PCOS diagnosis
hormone_summary <- pcos %>%
  group_by(PCOS) %>%
  summarise(
    Avg_AMH    = round(mean(AMH), 2),
    Avg_FSH    = round(mean(FSH), 2),
    Avg_LH     = round(mean(LH), 2),
    Avg_TSH    = round(mean(TSH), 2),
    Avg_PRL    = round(mean(PRL), 2),
    Avg_BMI    = round(mean(BMI), 2),
    Avg_RBS    = round(mean(RBS), 2),
    Avg_Vit_D3 = round(mean(Vit_D3), 2)
  )

# Preview the summary
print(hormone_summary)

# --- 5.2 Visualise hormone differences ---

# Reshape data for plotting
hormone_plot <- pcos %>%
  select(PCOS, AMH, FSH, LH, TSH, PRL) %>%
  pivot_longer(cols = -PCOS, 
               names_to = "Hormone", 
               values_to = "Value") %>%
  group_by(PCOS, Hormone) %>%
  summarise(Mean_Value = round(mean(Value), 2), .groups = "drop")

# Plot
ggplot(hormone_plot, aes(x = Hormone, y = Mean_Value, fill = PCOS)) +
  geom_col(position = "dodge", width = 0.6) +
  scale_fill_manual(values = c("0" = "#1a6fc4", "1" = "#e74c3c"),
                    labels = c("0" = "No PCOS", "1" = "PCOS")) +
  labs(
    title = "Average Hormone Levels: PCOS vs No PCOS",
    subtitle = "LH is nearly 6x higher in PCOS patients",
    x = "Hormone",
    y = "Average Level",
    fill = "Diagnosis"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", colour = "#1a3a5c"))

# Save
ggsave("hormone_comparison.png", width = 10, height = 6, dpi = 300)

# --- 5.3 Symptom comparison ---
symptom_summary <- pcos %>%
  group_by(PCOS) %>%
  summarise(
    Weight_gain    = round(mean(Weight_gain) * 100, 1),
    Hair_growth    = round(mean(Hair_growth) * 100, 1),
    Skin_darkening = round(mean(Skin_darkening) * 100, 1),
    Hair_loss      = round(mean(Hair_loss) * 100, 1),
    Pimples        = round(mean(Pimples) * 100, 1),
    Fast_food      = round(mean(Fast_food) * 100, 1),
    Exercise       = round(mean(Exercise) * 100, 1)
  )

print(symptom_summary)

# --- 5.4 Visualise symptom differences ---
symptom_plot <- symptom_summary %>%
  pivot_longer(cols = -PCOS,
               names_to = "Symptom",
               values_to = "Percentage")

ggplot(symptom_plot, aes(x = reorder(Symptom, Percentage), 
                         y = Percentage, 
                         fill = PCOS)) +
  geom_col(position = "dodge", width = 0.6) +
  coord_flip() +
  scale_fill_manual(values = c("0" = "#1a6fc4", "1" = "#e74c3c"),
                    labels = c("0" = "No PCOS", "1" = "PCOS")) +
  labs(
    title = "Symptom Prevalence: PCOS vs No PCOS",
    subtitle = "Fast food consumption and weight gain show biggest gaps",
    x = "Symptom",
    y = "Percentage of Patients (%)",
    fill = "Diagnosis"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", colour = "#1a3a5c"))

# Save
ggsave("symptom_comparison.png", width = 10, height = 6, dpi = 300)

# --- 5.5 Correlation heatmap --- improved version
numeric_cols <- pcos %>%
  select(Age, BMI, FSH, LH, AMH, TSH, PRL,
         Waist_Hip_Ratio, RBS, Vit_D3,
         Follicle_L, Follicle_R) %>%
  cor()

ggcorrplot(numeric_cols,
           method = "square",
           type = "full",
           lab = TRUE,
           lab_size = 3,
           colors = c("#1a6fc4", "white", "#e74c3c"),
           outline.color = "white",
           tl.cex = 10,
           title = "Correlation Between Key PCOS Variables",
           ggtheme = theme_minimal()) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    axis.text.y = element_text(size = 9),
    plot.title = element_text(face = "bold", colour = "#1a3a5c", size = 13)
  )

# Save
ggsave("correlation_heatmap.png", width = 12, height = 10, dpi = 300)

# --- 5.6 BMI distribution by PCOS diagnosis ---
ggplot(pcos, aes(x = PCOS, y = BMI, fill = PCOS)) +
  geom_boxplot(width = 0.5, outlier.colour = "#e74c3c", 
               outlier.size = 2) +
  geom_jitter(width = 0.15, alpha = 0.3, size = 1) +
  scale_fill_manual(values = c("0" = "#1a6fc4", "1" = "#e74c3c"),
                    labels = c("0" = "No PCOS", "1" = "PCOS")) +
  scale_x_discrete(labels = c("0" = "No PCOS", "1" = "PCOS")) +
  labs(
    title = "BMI Distribution: PCOS vs No PCOS",
    subtitle = "PCOS patients tend to have higher BMI",
    x = "Diagnosis",
    y = "BMI",
    fill = "Diagnosis"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", colour = "#1a3a5c"),
    legend.position = "none"
  )

# Save
ggsave("bmi_distribution.png", width = 8, height = 6, dpi = 300)

# ============================================================
# 6. PCOS PREDICTION MODEL
# ============================================================

# --- 6.1 Prepare data for machine learning ---
# Select features for the model
# Removing ID columns that have no predictive value
model_data <- pcos %>%
  select(-Patient_ID, -File_No)

# Check the data is ready
dim(model_data)
head(model_data)
# --- 6.2 Split data into training and testing sets ---

# Set seed for reproducibility
# This means anyone who runs this code gets the same split
set.seed(123)

# Create 80/20 split
train_index <- createDataPartition(model_data$PCOS, 
                                   p = 0.8, 
                                   list = FALSE)

train_data <- model_data[train_index, ]
test_data  <- model_data[-train_index, ]

# Check the split
cat("Training set:", nrow(train_data), "patients\n")
cat("Testing set:", nrow(test_data), "patients\n")

# --- 6.3 Train the Random Forest model ---

# Train the model
set.seed(123)
rf_model <- randomForest(
  PCOS ~ .,
  data = train_data,
  ntree = 500,
  importance = TRUE
)

# Print model summary
print(rf_model)

# --- 6.4 Test the model on unseen data ---

# Make predictions on test data
predictions <- predict(rf_model, test_data)

# Confusion matrix on test data
conf_matrix <- confusionMatrix(predictions, test_data$PCOS)
print(conf_matrix)

# --- 6.5 Feature importance ---
importance_df <- as.data.frame(importance(rf_model)) %>%
  rownames_to_column("Feature") %>%
  arrange(desc(MeanDecreaseGini))

# Plot top 15 most important features
ggplot(importance_df[1:15,], 
       aes(x = reorder(Feature, MeanDecreaseGini),
           y = MeanDecreaseGini,
           fill = MeanDecreaseGini)) +
  geom_col(width = 0.6) +
  coord_flip() +
  scale_fill_gradient(low = "#a8c8e8", high = "#1a3a5c") +
  labs(
    title = "Top 15 Most Important Features for PCOS Prediction",
    subtitle = "Variables that contribute most to the model's accuracy",
    x = "Feature",
    y = "Importance Score",
    fill = "Importance"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", colour = "#1a3a5c"),
    legend.position = "none"
  )

# Save
ggsave("feature_importance.png", width = 10, height = 8, dpi = 300)

# --- 6.6 ROC Curve ---
predictions_prob <- predict(rf_model, test_data, type = "prob")[,2]

roc_curve <- roc(as.numeric(test_data$PCOS) - 1, 
                 predictions_prob)

# Plot ROC curve
ggroc(roc_curve, colour = "#1a6fc4", size = 1.2) +
  geom_abline(slope = 1, intercept = 1, 
              linetype = "dashed", colour = "grey") +
  labs(
    title = paste0("ROC Curve - AUC: ", 
                   round(auc(roc_curve), 3)),
    subtitle = "Closer to top left corner = better model",
    x = "Specificity",
    y = "Sensitivity"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", colour = "#1a3a5c"))

# Save
ggsave("roc_curve.png", width = 8, height = 6, dpi = 300)

# ============================================================
# 7. PCOS SUBTYPE CLUSTERING
# ============================================================

# --- 7.1 Overview ---
# We will use K-means clustering to identify natural groupings
# within PCOS patients only. These groupings will be mapped
# to the 4 known PCOS subtypes:
# 1. Insulin Resistant PCOS - high RBS, BMI, waist
# 2. Androgen Excess PCOS - high LH, AMH, hair growth, acne
# 3. Inflammatory PCOS - low Vit D3, high TSH, skin issues
# 4. Post Pill PCOS - cycle irregularity, hormone imbalance
# ============================================================
# --- 7.2 Filter to PCOS patients only ---
# Clustering only makes sense within diagnosed PCOS patients
pcos_only <- pcos %>%
  filter(PCOS == 1) %>%
  select(
    # Insulin resistance markers
    RBS, BMI, Waist_Hip_Ratio, Weight_gain,
    # Androgen excess markers  
    LH, AMH, FSH_LH, Hair_growth, Pimples, Hair_loss,
    # Inflammatory markers
    Vit_D3, TSH, Skin_darkening,
    # Cycle markers
    Cycle, Cycle_length, FSH
  )

# Check how many PCOS patients we have
nrow(pcos_only)

# --- 7.3 Prepare data for clustering ---
# Clustering requires all numeric columns
# Convert Cycle factor to numeric first
pcos_cluster <- pcos_only %>%
  mutate(Cycle = ifelse(Cycle == "Regular", 0, 1))

# Scale all variables so no single one dominates
# This is critical for K-means clustering
pcos_scaled <- scale(pcos_cluster)

# Verify scaling worked
head(pcos_scaled)

# --- 7.4 Find the optimal number of clusters ---
# We need to decide how many subtypes to look for
# The elbow method helps us find the right number

set.seed(123)
fviz_nbclust(pcos_scaled, 
             kmeans, 
             method = "wss",
             k.max = 8) +
  labs(
    title = "Optimal Number of PCOS Clusters",
    subtitle = "Look for the elbow point where the curve flattens",
    x = "Number of Clusters",
    y = "Total Within Sum of Squares"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", colour = "#1a3a5c"))

# Save
ggsave("optimal_clusters.png", width = 8, height = 6, dpi = 300)

# --- 7.5 Build K-means clustering with 4 clusters ---
set.seed(123)
kmeans_result <- kmeans(pcos_scaled, 
                        centers = 4, 
                        nstart = 25)

# Add cluster labels back to original PCOS data
pcos_only$Cluster <- as.factor(kmeans_result$cluster)

# Check how many patients in each cluster
table(pcos_only$Cluster)

# Check the outlier patient in cluster 1
pcos_only %>% filter(Cluster == 1)


# Check all extreme values
summary(pcos_only$FSH_LH)
summary(pcos_only$FSH)
summary(pcos_only$LH)
summary(pcos_only$AMH)

# Find outliers using IQR method
Q1_fsh <- quantile(pcos_only$FSH_LH, 0.25)
Q3_fsh <- quantile(pcos_only$FSH_LH, 0.75)
IQR_fsh <- Q3_fsh - Q1_fsh

outliers <- pcos_only %>%
  filter(FSH_LH > Q3_fsh + 3 * IQR_fsh |
           FSH > quantile(FSH, 0.99) |
           LH > quantile(LH, 0.99))

nrow(outliers)
print(outliers %>% select(FSH_LH, FSH, LH, AMH))

# --- 7.6 Remove all extreme outliers using IQR method ---
pcos_clean_final <- pcos_only %>%
  select(-Cluster) %>%
  filter(
    FSH_LH < Q3_fsh + 3 * IQR_fsh &
      LH < 100 &
      FSH < 30 &
      AMH < 25
  )

# Check how many remain
nrow(pcos_clean_final)

# Rescale
pcos_scaled_final <- pcos_clean_final %>%
  mutate(Cycle = ifelse(Cycle == "Regular", 0, 1)) %>%
  scale()

# Rerun with 4 clusters
set.seed(123)
kmeans_final <- kmeans(pcos_scaled_final,
                       centers = 4,
                       nstart = 25)

# Add cluster labels
pcos_clean_final$Cluster <- as.factor(kmeans_final$cluster)


# Check cluster sizes
table(pcos_clean_final$Cluster)

# --- 7.7 Final clustering approach using stable variables only ---
# Use only binary symptoms and BMI which have no extreme outliers

pcos_stable <- pcos %>%
  filter(PCOS == 1) %>%
  select(BMI, Waist_Hip_Ratio, Weight_gain, 
         Hair_growth, Skin_darkening, Hair_loss, 
         Pimples, Fast_food, Exercise,
         Cycle_length)

# Scale
pcos_stable_scaled <- scale(pcos_stable)

# Rerun with 4 clusters
set.seed(123)
kmeans_stable <- kmeans(pcos_stable_scaled,
                        centers = 4,
                        nstart = 50)

# Add cluster labels
pcos_stable$Cluster <- as.factor(kmeans_stable$cluster)

# Check cluster sizes
table(pcos_stable$Cluster)

# --- 7.8 Profile each cluster ---
cluster_profiles <- pcos_stable %>%
  group_by(Cluster) %>%
  summarise(
    Avg_BMI            = round(mean(BMI), 1),
    Avg_Waist_Hip      = round(mean(Waist_Hip_Ratio), 2),
    Pct_Weight_gain    = round(mean(Weight_gain) * 100, 1),
    Pct_Hair_growth    = round(mean(Hair_growth) * 100, 1),
    Pct_Skin_darkening = round(mean(Skin_darkening) * 100, 1),
    Pct_Hair_loss      = round(mean(Hair_loss) * 100, 1),
    Pct_Pimples        = round(mean(Pimples) * 100, 1),
    Pct_Fast_food      = round(mean(Fast_food) * 100, 1),
    Pct_Exercise       = round(mean(Exercise) * 100, 1),
    Avg_Cycle_length   = round(mean(Cycle_length), 1),
    Count              = n()
  )

print(cluster_profiles)

# View full profile table
print(cluster_profiles, width = Inf)

# --- 7.9 Assign PCOS subtype labels ---
pcos_stable <- pcos_stable %>%
  mutate(PCOS_Type = case_when(
    Cluster == 1 ~ "Insulin Resistant PCOS",
    Cluster == 2 ~ "Inflammatory PCOS",
    Cluster == 3 ~ "Post Pill PCOS",
    Cluster == 4 ~ "Androgen Excess PCOS"
  ))

# Check the labelling
table(pcos_stable$PCOS_Type)

# --- 7.10 Visualise PCOS subtypes ---

# Pie chart of subtype distribution
subtype_counts <- pcos_stable %>%
  count(PCOS_Type) %>%
  mutate(Percentage = round(n / sum(n) * 100, 1),
         Label = paste0(PCOS_Type, "\n", Percentage, "%"))

ggplot(subtype_counts, aes(x = "", y = n, fill = PCOS_Type)) +
  geom_col(width = 1, colour = "white", linewidth = 0.5) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c(
    "Insulin Resistant PCOS" = "#e74c3c",
    "Inflammatory PCOS"      = "#1a6fc4",
    "Post Pill PCOS"         = "#2ecc71",
    "Androgen Excess PCOS"   = "#f39c12"
  )) +
  geom_text(aes(label = Label),
            position = position_stack(vjust = 0.5),
            colour = "white",
            fontface = "bold",
            size = 3.5) +
  labs(
    title = "PCOS Subtype Distribution",
    subtitle = "Based on K-means clustering of 177 PCOS patients",
    fill = "PCOS Type"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold", colour = "#1a3a5c", size = 14),
    plot.subtitle = element_text(colour = "#666666", size = 11),
    legend.position = "none"
  )

# Save
ggsave("pcos_subtypes.png", width = 8, height = 8, dpi = 300)

# --- 7.11 Symptom profile by PCOS subtype ---
subtype_profile <- pcos_stable %>%
  group_by(PCOS_Type) %>%
  summarise(
    Weight_gain    = round(mean(Weight_gain) * 100, 1),
    Hair_growth    = round(mean(Hair_growth) * 100, 1),
    Skin_darkening = round(mean(Skin_darkening) * 100, 1),
    Hair_loss      = round(mean(Hair_loss) * 100, 1),
    Pimples        = round(mean(Pimples) * 100, 1),
    Fast_food      = round(mean(Fast_food) * 100, 1),
    Exercise       = round(mean(Exercise) * 100, 1)
  ) %>%
  pivot_longer(cols = -PCOS_Type,
               names_to = "Symptom",
               values_to = "Percentage")

ggplot(subtype_profile, aes(x = Symptom, 
                            y = Percentage, 
                            fill = PCOS_Type)) +
  geom_col(position = "dodge", width = 0.7) +
  scale_fill_manual(values = c(
    "Insulin Resistant PCOS" = "#e74c3c",
    "Inflammatory PCOS"      = "#1a6fc4",
    "Post Pill PCOS"         = "#2ecc71",
    "Androgen Excess PCOS"   = "#f39c12"
  )) +
  labs(
    title = "Symptom Profile by PCOS Subtype",
    subtitle = "Each subtype has a distinct symptom pattern",
    x = "Symptom",
    y = "Percentage of Patients (%)",
    fill = "PCOS Type"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", colour = "#1a3a5c"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Save
ggsave("subtype_symptoms.png", width = 12, height = 7, dpi = 300)
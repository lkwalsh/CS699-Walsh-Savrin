# Updated preprocessing script
setwd("C:\\Users\\linds\\OneDrive\\Documents\\GitHub\\CS699-Walsh-Savrin")
system("git status")
system("git add preprocessing_code.R")

install.packages()
library(dplyr)
library(tidyr)

# Load dataset
file <- read.csv("C:/Users/linds/Downloads/project_data.csv")
data <- file
dim(data)

# Check for missing values
sum(is.na(data)) # Number of missing values in the dataset
sapply(data, function(x) sum(is.na(x))) # Missing values per column

# Remove columns where more than 50% of rows are NA
threshold <- 0.5 * nrow(data)
df_col_trim <- data[, colSums(is.na(data)) <= threshold]
dim(df_col_trim)
sapply(df_col_trim, function(x) sum(is.na(x)))

# At this point MARHYP has 1542 missing out of 4318
# We have to decide whether to remove rows based on MARHYP or POWPUMA:
# If we remove rows based on MARHYP first --> 1,100 missing values remain in POWPUMA
# If we remove rows based on POWPUMA first --> 918 missing values remain in MARHYP
# Removing POWPUMA first keeps more non missing data so we can go in that direction

df_row_trim <- df_col_trim %>% drop_na(POWPUMA)
dim(df_row_trim)
sapply(df_row_trim, function(x) sum(is.na(x)))

# More trimming by removing MARHYP missing values
df_row_trim_2 <- df_row_trim %>% drop_na(MARHYP)
dim(df_row_trim_2)
sapply(df_row_trim_2, function(x) sum(is.na(x)))

# Replace remaining NA values with the median (imputation)
df_imputation <- df_row_trim_2 %>%
  mutate(across(where(is.numeric), 
                ~ replace(., is.na(.), median(., na.rm = TRUE))))
sapply(df_imputation, function(x) sum(is.na(x)))

# Check for duplicates
df_unique <- unique(df_imputation)
dim(df_unique)

# Convert class to a binary numeric (1 = Yes, 0 = No)
df_imputation$Class <- ifelse(df_imputation$Class == "Yes", 1, 0)

# Remove non numeric columns
df_numeric <- df_imputation %>% select(-RT, -SERIALNO)  # Keep class as it is our classifier
dim(df_numeric)

# Check for low variance columns
variances <- apply(df_numeric, 2, var, na.rm = TRUE)
threshold <- 0.01  # Set threshold for low variance
df_numeric_no_var <- df_numeric[, variances > threshold]
dim(df_numeric_no_var)

# Make sure zero variance columns are removed too
zero_variance_cols <- names(variances)[variances == 0]
df_numeric_no_var <- df_numeric[, variances > 0]
dim(df_numeric_no_var)

# At first we questioned whether both variance filtering steps were needed but confirmed it is good to double check:
# (1) First filters out low-variance columns
# (2) Second ensures zero-variance columns are removed

# Check for highly correlated columns
cor_matrix <- cor(df_numeric_no_var)
high_cor_matrix <- cor_matrix > 0.5

# Identify correlated pairs
high_cor_pairs <- which(high_cor_matrix, arr.ind = TRUE)
high_cor_pairs <- high_cor_pairs[high_cor_pairs[,1] != high_cor_pairs[,2], ] # Remove diagonal

# Review highly correlated columns
high_cor_df <- data.frame(
  var1 = rownames(cor_matrix)[high_cor_pairs[, 1]],
  var2 = colnames(cor_matrix)[high_cor_pairs[, 2]],
  correlation = cor_matrix[high_cor_pairs]
)

high_cor_df

# Remove highly correlated columns
df_numeric_no_cor <- df_numeric_no_var %>% select(-RAC1P, -RAC2P, -RACNUM, -RACSOR, -CIT, -NATIVITY, -MSP, -WAOB, -WAGP, -PERNP)
dim(df_numeric_no_cor)

# Boxplot for outlier detection in SCHL
boxplot(df_numeric_no_cor$SCHL, main = "Boxplot of SCHL")

# Identify outliers using IQR method from the boxplot
outliers <- boxplot(df_numeric_no_cor$SCHL, plot = FALSE)$out
outliers


install.packages()
library(dplyr)
library(tidyr)

file<-read.csv("C:/Users/lsav1/Downloads/project_data.csv")
dim(file)
data<-file
dim(data)
sum(is.na(data)) # number of missing values in the whole dataset
sapply(data, function(x) sum(is.na(x))) # missing values in individual columns

threshold <- 0.5 * nrow(data)
df_col_trim <- data[, colSums(is.na(data)) <= threshold] #cut columns where more than 50% of rows are NA
dim(df_col_trim)
sapply(df_col_trim, function(x) sum(is.na(x)))

# at this point 1542 N/A values out of 4318 for the MARHYP columns. do we remove those or based on the POWPUMA They remove
#different lines POWPUMA after MARHYP leaves 1100 NA. The reverse leaves 918 NA. Which is the better choice? Are either good?
#they remove different lines.... should we prioritize what returns more zeros over all or keeps more data involved to clean the rest
#I think we should go in the direction of removing rows based on POWPUMA since it retains more “non-missing data” (so less data loss)

df_row_trim <-df_col_trim %>% drop_na(POWPUMA)
dim(df_row_trim)
sapply(df_row_trim, function(x) sum(is.na(x)))

#checking for further trimming
df_row_trim_2 <- df_row_trim %>% drop_na(MARHYP)
dim(df_row_trim_2)
sapply(df_row_trim_2, function (x) sum(is.na(x)))
df_row_trim_2

#replace Na's with median
df_imputation <- df_row_trim_2 %>%
  mutate(across(where(is.numeric), 
                ~ replace(., is.na(.), median(., na.rm = TRUE))))
sapply(df_imputation, function(x) sum(is.na(x)))
sapply(df_imputation, class) #determines the type for each column
dim(df_imputation)

df_imputation

#checking for duplicates
df_unique <- unique(df_imputation)
dim(df_unique)

#converting class to a binary numeric code. 1=Yes, 0=No
df_imputation$Class <- ifelse(df_imputation$Class == "Yes", 1, 0)

#remove non-numeric columns
df_numeric <- df_imputation %>% select(-Class, -RT, -SERIALNO) ##since we made class a binary do we need to remove it. We don't want to lose that as the clasifier...
dim(df_numeric)

#check for low variance columns
variances <- apply(df_numeric, 2, var, na.rm = TRUE)
threshold <- 0.01  # Set the threshold for low variance
variances
df_no_var <- df_imputation[, variances > threshold]
dim(df_no_var)
df_numeric_no_var <- df_no_var %>% select(-Class, -SERIALNO)

variances <- apply(df_numeric, 2, var, na.rm = TRUE)
zero_variance_cols <- names(variances)[variances == 0]
zero_variance_cols
df_numeric_no_var <- df_numeric[, variances > 0]
dim(df_numeric_no_var)
#Do we need both of the above? Apparently yes

#check for highly correlated columns
cor_matrix <- cor(df_numeric_no_var)
cor_matrix

high_cor_matrix <- cor_matrix > 0.5
high_cor_values <- cor_matrix[high_cor_matrix] #do we need this? seems like not...
high_cor_matrix

# Get the row and column names for high correlations
high_cor_pairs <- which(high_cor_matrix, arr.ind = TRUE)
high_cor_pairs <- high_cor_pairs[high_cor_pairs[,1] != high_cor_pairs[,2], ]#Remove diagonal
high_cor_pairs

#review highly correlated columns
high_cor_df <- data.frame(
  var1 = rownames(cor_matrix)[high_cor_pairs[, 1]],
  var2 = colnames(cor_matrix)[high_cor_pairs[, 2]],
  correlation = cor_matrix[high_cor_pairs]
)

high_cor_df
df_numeric_no_cor<- df_numeric_no_var %>% select(-RAC1P, -RAC2P, -RACNUM, -RACSOR,-CIT,-NATIVITY,-MSP,-WAOB, -WAGP, -PERNP)
high_cor_df2 <- high_cor_df[!(high_cor_df$var1 %in% c("RAC1P", "RAC2P", "RACNUM", "RACSOR","CIT","NATIVITY","MSP","WAOB", "WAGP", "PERNP")),]  # Keep rows where 'class' is not "No"

high_cor_df2
dim(df_numeric_no_cor)

# Create a correlation matrix
cor_matrix <- cor(df_numeric2)  # where df is your dataset

colnames(df_numeric_no_cor)

boxplot(df_numeric_no_cor$SCHL, main = "Boxplot of column_name")

# Identify outliers using the IQR method from the boxplot
outliers <- boxplot(df_numeric_no_cor$SCHL, plot = FALSE)$out
outliers

system('git commit -m "Updated preprocessing_code.R with latest changes"')
system("git push origin main")


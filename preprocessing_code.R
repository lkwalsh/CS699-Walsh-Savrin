setwd("C:\\Users\\lsav1\\OneDrive\\Documents\\GitHub\\CS699-Walsh-Savrin")
system("git status")
system("git add preprocessing_code.R")

install.packages()
library(dplyr)
library(tidyr)
library(caret)

# Load dataset
file<-read.csv("C:/Users/lsav1/Downloads/project_data.csv")
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

# Replace remaining NA values with the median (imputation)
df_imputation <- df_col_trim %>%
  mutate(across(where(is.numeric), 
                ~ replace(., is.na(.), median(., na.rm = TRUE))))
sapply(df_imputation, function(x) sum(is.na(x)))
dim (df_imputation)
# Check for duplicates
df_unique <- unique(df_imputation)
dim(df_unique)

# Convert class to a binary numeric (1 = Yes, 0 = No)
df_imputation$Class <- ifelse(df_imputation$Class == "Yes", 1, 0)

# Remove non numeric columns
df_numeric <- df_imputation %>% dplyr::select(-RT, -SERIALNO)
dim(df_numeric)
colnames(df_numeric)

# Make sure zero variance columns are removed
variances <- apply(df_numeric, 2, var, na.rm = TRUE)
zero_variance_cols <- names(variances)[variances == 0]
zero_variance_cols
df_numeric_no_zero <- df_numeric[, variances > 0]
dim(df_numeric_no_zero)

# Check for low variance columns and remove those as well
low_variances <- apply(df_numeric_no_zero, 2, var, na.rm = TRUE)
threshold <- 0.01  # Set threshold for low variance
low_var_cols <- names(low_variances)[low_variances < threshold]
low_var_cols
df_numeric_no_var <- df_numeric_no_zero[, low_variances > threshold]
dim(df_numeric_no_var)
colnames(df_numeric_no_var)

# At first we questioned whether both variance filtering steps were needed but confirmed it is good to check both:
# (1) First ensures zero-variance columns are removed
# (2) Second filters out low-variance columns

# Check for highly correlated columns
cor_matrix <- cor(df_numeric_no_var)
print(cor_matrix)
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
df_numeric_no_cor <- df_numeric_no_var %>% dplyr::select(-RAC1P, -RAC2P, -RACNUM, -RACSOR, -CIT, -NATIVITY, -MSP, -WAOB, -WAGP, -PERNP, -OC, -WRK, -NWLA, -NWAB, -NWLK)
dim(df_numeric_no_cor)
print(colnames(df_numeric_no_cor))

#Identify outliers the boxplot

summary(df_numeric_no_cor$SPORDER)
table(boxplot(df_numeric_no_cor$SPORDER, plot = FALSE)$out)
#everything 4 and over...keep them all

summary(df_numeric_no_cor$PUMA)
table(boxplot(df_numeric_no_cor$PUMA, plot = FALSE)$out) 
#none

summary(df_numeric_no_cor$PWGTP)
table(boxplot(df_numeric_no_cor$PWGTP, plot = FALSE)$out)
sum(table(boxplot(df_numeric_no_cor$PWGTP, plot = FALSE)$out))
#evertyhing over 188. total off 422 outliers. removal may be too much? unclear if there is overlap with other outliers

summary(df_numeric_no_cor$COW)
table(boxplot(df_numeric_no_cor$COW, plot = FALSE)$out)
sum(table(boxplot(df_numeric_no_cor$COW, plot = FALSE)$out))
#anything over 3. Total of 519 outliers

summary(df_numeric_no_cor$INTP)
table(boxplot(df_numeric_no_cor$INTP, plot = FALSE)$out)
sum(table(boxplot(df_numeric_no_cor$INTP, plot = FALSE)$out))
#643 outliers. Drop the two ends? 363000, -2100

summary(df_numeric_no_cor$JWTRNS)
table(boxplot(df_numeric_no_cor$JWTRNS, plot = FALSE)$out)
sum(table(boxplot(df_numeric_no_cor$JWTRNS, plot = FALSE)$out))
#everything that isn't 1. Total of 820. 1 is car, truck or van. therefore the largest group by default. keep all

summary(df_numeric_no_cor$MAR)
table(boxplot(df_numeric_no_cor$MAR, plot = FALSE)$out)
#none

summary(df_numeric_no_cor$MARHYP)
table(boxplot(df_numeric_no_cor$MARHYP, plot = FALSE)$out)
sum(table(boxplot(df_numeric_no_cor$MARHYP, plot = FALSE)$out))
#323 outliers, anything between 1947 - 1971. this is based on 2023 data so people married in these years are older and fewer

summary(df_numeric_no_cor$MIL)
table(boxplot(df_numeric_no_cor$MIL, plot = FALSE)$out)
#2 had 173 but everything but 4 is an outlier

summary(df_numeric_no_cor$NWAV)
table(boxplot(df_numeric_no_cor$NWAV, plot = FALSE)$out)
#1,2 and 3 are all outliers. total of 261

summary(df_numeric_no_cor$OIP)
table(boxplot(df_numeric_no_cor$OIP, plot = FALSE)$out)
sum(table(boxplot(df_numeric_no_cor$OIP, plot = FALSE)$out))
#164 outliers but scattered. no more than 7 on any value. anything above 149

summary(df_numeric_no_cor$PAP)
table(boxplot(df_numeric_no_cor$PAP, plot = FALSE)$out)
#predominantly zero. Keep them all.

summary(df_numeric_no_cor$RETP)
table(boxplot(df_numeric_no_cor$RETP, plot = FALSE)$out)
#predominantly zero. Keep them all. lots of outliers but that is essentially anything but zero

summary(df_numeric_no_cor$SCHL)
table(boxplot(df_numeric_no_cor$SCHL, plot = FALSE)$out)
#keep all, lower levels are uncommon but still relevant.

summary(df_numeric_no_cor$SEMP)
table(boxplot(df_numeric_no_cor$SEMP, plot = FALSE)$out)
sum(table(boxplot(df_numeric_no_cor$SEMP, plot = FALSE)$out))
#272 outliers. over 10 are 20k and 423k. also predominantly zero. keep it all

summary(df_numeric_no_cor$SSIP)
table(boxplot(df_numeric_no_cor$SSIP, plot = FALSE)$out)
sum(table(boxplot(df_numeric_no_cor$SSIP, plot = FALSE)$out))
#predominantly zero. Keep them all. most in 26800 with 10 but all were above 1300. 138 total

summary(df_numeric_no_cor$SSP)
table(boxplot(df_numeric_no_cor$SSP, plot = FALSE)$out)
sum(table(boxplot(df_numeric_no_cor$SSP, plot = FALSE)$out))
#predominantly zero. 992 outliers... keep them all

table(df_numeric_no_cor$WKHP)
table(boxplot(df_numeric_no_cor$WKHP, plot = FALSE)$out)
sum(table(boxplot(df_numeric_no_cor$WKHP, plot = FALSE)$out))
#anything outside a 40 hour work week is an outlier. 1604 outliers

summary(df_numeric_no_cor$WKWN)
table(boxplot(df_numeric_no_cor$WKWN, plot = FALSE)$out)
sum(table(boxplot(df_numeric_no_cor$WKWN, plot = FALSE)$out))
#anything outside a 52 weeks is an outlier. 697 outliers

summary(df_numeric_no_cor$ANC)
table(boxplot(df_numeric_no_cor$ANC, plot = FALSE)$out)
#outliers is 4 with 799

summary(df_numeric_no_cor$ANC1P)
table(boxplot(df_numeric_no_cor$ANC1P, plot = FALSE)$out)
#none

summary(df_numeric_no_cor$ANC2P)
table(boxplot(df_numeric_no_cor$ANC2P, plot = FALSE)$out)
#none

summary(df_numeric_no_cor$ESR)
table(boxplot(df_numeric_no_cor$ESR, plot = FALSE)$out)
#none

summary(df_numeric_no_cor$HISP)
table(boxplot(df_numeric_no_cor$HISP, plot = FALSE)$out)
sum(table(boxplot(df_numeric_no_cor$HISP, plot = FALSE)$out))
# ttoal: 374. everything but one is an outlier

summary(df_numeric_no_cor$INDP)
table(boxplot(df_numeric_no_cor$INDP, plot = FALSE)$out)
sum(table(boxplot(df_numeric_no_cor$INDP, plot = FALSE)$out))
#is there any real value in this. anything below 5581 and above 9290. 974 total

summary(df_numeric_no_cor$OCCP)
table(boxplot(df_numeric_no_cor$OCCP, plot = FALSE)$out)
sum(table(boxplot(df_numeric_no_cor$OCCP, plot = FALSE)$out))
#anything above 8450 is an outlier. total of 225

summary(df_numeric_no_cor$PINCP)
table(boxplot(df_numeric_no_cor$PINCP, plot = FALSE)$out)
sum(table(boxplot(df_numeric_no_cor$PINCP, plot = FALSE)$out))
#anything above 195k with rounded increments have higher volumes. 260 total

summary(df_numeric_no_cor$POBP)
table(boxplot(df_numeric_no_cor$POBP, plot = FALSE)$out)
sum(table(boxplot(df_numeric_no_cor$POBP, plot = FALSE)$out))
#878 total. anything over 70 (is that outside the US)

summary(df_numeric_no_cor$POVPIP)
table(boxplot(df_numeric_no_cor$POVPIP, plot = FALSE)$out)
#none

summary(df_numeric_no_cor$POWPUMA)
table(boxplot(df_numeric_no_cor$POWPUMA, plot = FALSE)$out)
sum(table(boxplot(df_numeric_no_cor$POWPUMA, plot = FALSE)$out))
#total of 327. below 300 and above 1290. is this locationally like area codes becuase that could influence the range

summary(df_numeric_no_cor$POWSP)
table(boxplot(df_numeric_no_cor$POWSP, plot = FALSE)$out)
sum(table(boxplot(df_numeric_no_cor$POWSP, plot = FALSE)$out))
#93 outliers . anything that was not 25 was an outlier possibility

summary(df_numeric_no_cor$QTRBIR)
table(boxplot(df_numeric_no_cor$QTRBIR, plot = FALSE)$out)
#none

summary(df_numeric_no_cor$RAC3P)
table(boxplot(df_numeric_no_cor$RAC3P, plot = FALSE)$out)
sum(table(boxplot(df_numeric_no_cor$RAC3P, plot = FALSE)$out))
#850 total. looks like anything 4 or above


#Handling outliers
df_outliers<-df_numeric_no_cor
#SPORDER
df_outliers$SPORDER <- log(df_outliers$SPORDER)

summary(df_outliers$SPORDER)
table(boxplot(df_outliers$SPORDER, plot = FALSE)$out)

#PWGTP
df_outliers$PWGTP <- log(df_outliers$PWGTP)

summary(df_outliers$PWGTP)
table(boxplot(df_outliers$PWGTP, plot = FALSE)$out)

#OIP
df_outliers$OIP <- log(df_outliers$OIP+1)

summary(df_outliers$OIP)
sum(table(boxplot(df_outliers$OIP, plot = FALSE)$out))

#PAP
df_outliers$PAP <- log(df_outliers$PAP+1)

summary(df_outliers$PAP)
sum(table(boxplot(df_outliers$PAP, plot = FALSE)$out))

#RETP
df_outliers$RETP <- log(df_outliers$RETP+1)

summary(df_outliers$RETP)
sum(table(boxplot(df_outliers$RETP, plot = FALSE)$out))

#SEMP
df_outliers$SEMP <- log(df_outliers$SEMP+1)

summary(df_outliers$SEMP)
sum(table(boxplot(df_outliers$SEMP, plot = FALSE)$out))

problematic_rows <- df_numeric_no_cor[df_numeric_no_cor$SEMP < -1 | is.na(df_numeric_no_cor$SEMP) | is.infinite(df_numeric_no_cor$SEMP), ]
print(problematic_rows)


#SSIP
df_outliers$SSIP <- log(df_outliers$SSIP+1)

summary(df_outliers$SSIP)
sum(table(boxplot(df_outliers$SSIP, plot = FALSE)$out))

#SSP
df_outliers$SSP <- log(df_outliers$SSP+1)

summary(df_outliers$SSP)
sum(table(boxplot(df_outliers$SSP, plot = FALSE)$out))

##INSERT OUTLIER TABLES HERE

#Split? 

set.seed(31)
library(rsample)
split <- initial_split(data, prop = 0.60, strata = Class)
training <- training(split)
test <- testing(split)
dim(training)
dim(test)
table(training$Class)

system("git add preprocessing_code.R")
system('git commit -m "Updated preprocessing_code.R with latest changes"')
system("git push origin main")

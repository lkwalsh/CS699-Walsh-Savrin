getwd()
install.packages()
library(dplyr)
library(tidyr)
file<-read.csv("C:/Users/linds/Downloads/project_data.csv")
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
df_final <- df_row_trim %>% drop_na(MARHYP)
dim(df_final)
sapply(df_final, function (x) sum(is.na(x)))
df_final

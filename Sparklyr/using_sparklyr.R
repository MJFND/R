#===============================================================================================
#
# File:        using_sparklyr.R
# Author:      Junaid Effendi
# Description: This code provides the usage of some basic functions from sparklyr package in R.
#
#===============================================================================================

##===========================================================
# Installing and Loading Libraries
##===========================================================
install.packages(c("stringr", "plyr", "sparklyr", "devtools", "dplyr", "xlsx", "plyr", "tidyr"))
library("stringr")
library("plyr")
library("sparklyr")
library("dplyr")
library("devtools")
library("xlsx")
library("plyr")
library("tidyr")

# Getting updated sparklyr package
devtools::install_github("rstudio/sparklyr")

##===========================================================
# Spark installation and setup
##===========================================================
spark_install(version = "2.1.0")
sc <- spark_connect(master = "local") 

##===========================================================
# Reading the data from CSV
##===========================================================
# Reading csv into spark
data_temp <- spark_read_csv(sc, name = "spark_data",path = "data.csv", header = TRUE, delimiter = ",")

# Checking column names
colnames(data_temp)
head(data_temp)

##===========================================================
# Data Manipulation
##===========================================================
# Removing all -1 pdays records
data_temp <- data_temp %>%  filter(pdays != -1)
# Checking the number of rows
sdf_nrow(data_temp)
head(data_temp)


# Changing column name 'y' to 'outcome'
newnames<- colnames(data_temp)
newnames[newnames =="y"]<-"outcome"
data_temp <- data_temp %>% select_(.dots=setNames(colnames(data_temp), newnames))
sdf_read_column(data_temp, "outcome")


# Function to find out binary columns
is.binary <- function(v) {
  x <- unique(v)
  length(x) - sum(is.na(x)) == 2L
}

# Replacing yes and no with True and False, respectively
data_temp <- mutate_if(data_temp,is.binary, funs(ifelse(. == "yes", TRUE,FALSE)))
# Reading a column from Spark Table for checking
sdf_read_column(data_temp, "outcome")


# Separating out First and Last Name
data_temp <- ft_regex_tokenizer(data_temp, input.col = "name", output.col = "FName", pattern = ' .*$')
data_temp <- ft_regex_tokenizer(data_temp, input.col = "name", output.col = "SName", pattern = '^.* ')
#data_temp <- mutate_if(data_temp, is.list, funs(as.character(.)))
data_temp <- select(data_temp, -name)


# Replace date and month with single date column
data_temp_date <- transmute(data_temp, date = paste( day , month , "2017", sep = "/" ))
data_temp <- select(data_temp, -day, -month)
data_temp <- cbind(data_temp,data_temp_date)

# Using select, filter and arrange
data_temp %>% select(age) %>% filter (age > 30) %>%
  arrange(age)

##===========================================================
# Writing to CSV
##===========================================================
# Using xlsx package to write data into Excel workbook
write.xlsx(x = data, file = "sparklyr_task.xlsx",
           sheetName = "MySheet", row.names = FALSE)

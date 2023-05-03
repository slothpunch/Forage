# install.packages('readxl')

if (!require('readxl')) install.packages('readxl')
if (!require('skimr')) install.packages('skimr')
# if (!require('xlsx')) install.packages('xlsx')

Sys.setenv(LANG = "en")

library(readxl)
library(ggplot2)
# library(xlsx)
library(tidyverse)
library(skimr)

getwd()
setwd('D:\\0_Forage\\2. KPMG')
getwd()

excel_sheets('datasets-for-r\\KPMG_VI_New_raw_data_update_final.xlsx')
excel_path <- 'datasets-for-r\\KPMG_VI_New_raw_data_update_final.xlsx'

df_new_cust <- read_excel(excel_path, sheet = 'NewCustomerList')
df_cust_add <- read_excel(excel_path, sheet = 'CustomerAddress')
df_cust_demo <- read_excel(excel_path, sheet = 'CustomerDemographic')
df_trans <- read_excel(excel_path, sheet = 'Transactions')

rm(excel_path)

# Set the header
set_header <- function (df_name, df_text){
  # https://stackoverflow.com/questions/3969852/update-data-frame-via-function-doesnt-work
  
  # Set the first row to the header
  colnames(df_name) <- df_name[1,]
  # Delete the first row
  df_name <- df_name[-1,]
  
  # Drop the columns whose names are NA
  # df_name[!is.na(colnames(df_name))]
  # colnames(df_name[(colnames(df_name)) != 'NA'])
  df_name <- df_name[(colnames(df_name)) != 'NA']
  
  assign(df_text, df_name, envir = .GlobalEnv)
}

set_header(df_new_cust, 'df_new_cust')
set_header(df_cust_add, 'df_cust_add')
set_header(df_cust_demo, 'df_cust_demo')
set_header(df_trans, 'df_trans')



####################################
# Task 1 - Data Quality Assessment #
####################################

# --------------------- Transformation to the right data types


task1 <- function () {
  # ---------------------- Change data types for df_new_cust
  to_int <- c('past_3_years_bike_related_purchases', 'tenure', 'property_valuation', 'Rank', 'Value')
  to_factor <- c('tenure', 'gender', 'job_title', 'job_industry_category', 'wealth_segment', 'deceased_indicator', 'owns_car', 'postcode', 'state', 'country')
  
  # Convert to date type
  df_new_cust$DOB <- as.Date(df_new_cust$DOB)
  # as.Date(df_new_cust['DOB']) # Not working
  
  # typeof(df_new_cust$DOB) # char
  # typeof(df_new_cust['DOB']) # list
  
  # Convert to int 
  df_new_cust <- df_new_cust %>% mutate_at(to_int, as.numeric)
  
  # Convert to factor
  df_new_cust <- df_new_cust %>% mutate_at(to_factor, as.factor)
  
  
  # ---------------------- Change data types for df_cust_add 
  to_int <- c('customer_id')
  to_factor <- c('property_valuation', 'postcode', 'state', 'country')
  
  # Convert to date type
  df_cust_add$customer_id <- as.numeric(df_cust_add$customer_id)
  
  # Convert to factor
  df_cust_add <- df_cust_add %>% mutate_at(to_factor, as.factor)
  
  
  # ---------------------- Change data types for df_cust_demo 
  df_cust_demo$default <- NULL
  
  # https://stackoverflow.com/questions/51618600/convert-multiple-columns-of-numeric-data-to-dates-in-r
  df_cust_demo$DOB <- as.Date(as.numeric(df_cust_demo$DOB), origin = "1899-12-30")
  
  to_int <- c('customer_id', 'past_3_years_bike_related_purchases')
  to_factor <- c('tenure', 'gender', 'job_title', 'job_industry_category', 'wealth_segment', 'deceased_indicator', 'owns_car')
  
  # Convert to int
  df_cust_demo <- df_cust_demo %>% mutate_at(to_int, as.numeric)
  
  # Convert to factor
  df_cust_demo <- df_cust_demo %>% mutate_at(to_factor, as.factor)
  
  # length(df_cust_demo$job_title[df_cust_demo$job_title == 'n/a'])
  # sum(!complete.cases(df_cust_demo$job_title))
  
  
  # ---------------------- Change data types for df_cust_add 
  df_trans$transaction_date <- as.Date(as.numeric(df_trans$transaction_date), origin = '1899-12-30')
  df_trans$product_first_sold_date <- as.Date(as.numeric(df_trans$product_first_sold_date), origin = '1899-12-30')
  
  to_int <- c('transaction_id', 'list_price', 'standard_cost')
  to_factor <- c('product_id', 'customer_id', 'online_order', 'order_status', 'brand', 'product_line', 'product_class', 'product_size')
  
  # Convert to int
  df_trans <- df_trans %>% mutate_at(to_int, as.numeric)
  
  # Convert to factor
  df_trans <- df_trans %>% mutate_at(to_factor, as.factor)
  
  # write_csv(df_new_cust, "datasets-for-r\\df_new_cust.csv")
  # write_csv(df_cust_add, "path\\file_name.csv", row.names = FALSE)
  # write_csv(df_cust_demo, "path\\file_name.csv", row.names = FALSE)
  # write_csv(df_trans, "path\\file_name.csv", row.names = FALSE)

}

task1()

str(df_new_cust)
df_new_cust %>% skim()

str(df_cust_add)
df_cust_add %>% skim()

str(df_cust_demo)
df_cust_demo %>% skim()

str(df_trans)
df_trans %>% skim()

################
# Task 2 - EDA #
################

# df_cust_add
# df_cust_demo
# df_trans


# --------------------- Geographic analysis - can a customer commute to work by bike?
colnames(df_cust_add)


# --------------------- Correlation analysis

# LETTERS[1:20] # alphabets
# paste0("var", seq(1, 20)) # var1, ... , var20
# paste("var", seq(1, 20)) # var 1, ... , var 20

str(df_cust_add)
cor(as.numeric(df_cust_add$postcode), as.numeric(df_cust_add$property_valuation))

str(df_cust_demo)

# unique?
levels(df_cust_demo$gender) # "F"      "Femal"  "Female" "M"      "Male"   "U" 

# pandas value_count
table(df_cust_demo$gender)

# Change 


df_cust_demo$gender_num <- ifelse(df_cust_demo$gender == 'Male', 1, 0)

sapply()

# .apply(labmda x:) = sapply(df_col, function(x) ifelse(,, x))
table(sapply(df_cust_demo$gender, function(x) ifelse(tolower(x) == "f", "Female", x)))

View(df_cust_demo)
str(df_trans)


# --------------------- ML
# df_new_cust




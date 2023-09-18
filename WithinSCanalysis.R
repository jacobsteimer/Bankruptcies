# Install all needed packages
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(haven)) install.packages("haven", repos = "http://cran.us.r-project.org")
if(!require(ggrepel)) install.packages("ggrepel", repos = "http://cran.us.r-project.org")
if(!require(broom)) install.packages("broom", repos = "http://cran.us.r-project.org")
if(!require(readr)) install.packages("readr", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")

# Load all needed packages
library(tidyverse)
library(caret)
library(haven)
library(ggrepel)
library(broom)
library(readr)
library(lubridate)

#Timeout
options(timeout = 120)

# This code will automatically download and load the three necessary datasets
# This first one is all the 2022 bankruptcies from the federal judicial center
Brup_file_url <- "https://github.com/jacobsteimer/USBankruptcyZIP/blob/main/cpbankdec22.zip"
Brup_local_zip <- "cpbankdec22.zip"
download.file(Brup_file_url, Brup_local_zip)
unzip(Brup_local_zip, exdir = "data_directory")
Brup_data <- read_sas("data_directory/cpbankdec22.sas7bdat")
head(Brup_data)
# this second one is some census data I pulled together
Census_file_url <- "https://github.com/jacobsteimer/USBankruptcyZIP/raw/main/CensusData.csv"
census_data <- readr::read_csv(Census_file_url)

#Getting started, let's ensure there are no duplicate cases
Brup_No_Dup <- Brup_data %>% distinct(CASEKEY, .keep_all = TRUE)

#This dataset includes all cases that were either filed in 2022 or were still open then. Let's cut it down to just cases filed that year.
Brup_No_Dup_2022 <- Brup_No_Dup %>% filter((year(FILEDATE) == 2022))

#Then, let's clean up the ZIP column
Brup_No_Dup_2022$ZIP <- substr(Brup_No_Dup_2022$D1ZIP, 1, 5)


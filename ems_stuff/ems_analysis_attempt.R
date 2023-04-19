library("tidyr")
library("dplyr")
library("ggplot2")
library("DescTools")
library("GGally")
library("stringr")
library("stats")
library("RColorBrewer")
library("wesanderson")
library("viridis")
library("plotly")
library("knitr")

#Categorize as monolingual/bilingual, but keep data on early vs late acquisition for bilingual ? 
#What to do about "none of the above"?

demographics_full_data <- read.csv("behavioral_data_raw/STAG consent and Demographics_April 14, 2023_17.58.csv")
demographics_full_data <- demographics_full_data[-1,] #removing row w/ column names
demographics_full_data <- demographics_full_data[-1,] #removing row w/ import data

#fixing demographic errors (birthdays listed as the date)
col_index <- which(colnames(demographics_full_data) == "Date")
row_index <- which(demographics_full_data$Subject.. == 7521)

demographics_full_data[row_index, col_index] <- "4/11/23"

print(demographics_full_data$Date)

row_index <- which(demographics_full_data$Date == "6/17/2004")
sub_no <- demographics_full_data$Subject..[row_index]
#change the date to this 4/11/2023

candy_slim_full_data <- read.csv("behavioral_data_raw/slim/9856_Candy_responses 2.csv")

maps_slim_full_data <- read.csv("behavioral_data_raw/slim/9857_Maps_responses 2.csv")

#adding overall bilingual category to full demographics
#see under "lang.classification2"
demographics_full_data <- demographics_full_data %>%
  mutate(lang.classification2 = NA)

for (i in 1:nrow(demographics_full_data)){
  if (grepl("Early Bilingual", demographics_full_data$lang.classification[i])) {
    demographics_full_data$lang.classification2[i] <- "Bilingual"
  } else if (grepl("Late Bilingual", demographics_full_data$lang.classification[i])){
    demographics_full_data$lang.classification2[i] <- "Bilingual"
  } else if (grepl("Monolingual", demographics_full_data$lang.classification[i])){
    demographics_full_data$lang.classification2[i] <- "Monolingual"
  } else {
    demographics_full_data$lang.classification2[i] <- "None of the above"
  }
}

#pulling bilingual 
bi_demographics <- demographics_full_data %>%
  subset(lang.classification2 == "Bilingual") %>%
  bind_rows(subset(demographics_full_data, lang.classification == "Late Bilingual"))

#pulling monolingual
mono_demographics <- demographics_full_data %>%
  subset(lang.classification == "Monolingual")

#pulling "none of the above"
unknown_lang_demographics <- mono_demographics <- demographics_full_data %>%
  subset(lang.classification == "None of the above")

#removing Teddy
candy_slim_full_data <- candy_slim_full_data %>%
  filter(screen_name != "TeddyHaile")
maps_slim_full_data <- maps_slim_full_data %>%
  filter(screen_name != "TeddyHaile")

#listing sub no. per first/last version

#making lists for which subjects did what first 
candy_first_subject_ids <- list()

# for (i in seq(7500, ), by = 2)) {
#   candy_first_subject_ids[[length(candy_first_subject_ids) + 1]] <- i
# }
















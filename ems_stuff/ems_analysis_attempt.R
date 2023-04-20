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
#candy first and last, maps first and last
#language 
#facet ; box no -> candy and map box w x axis candy first or last, colors language, y accuracy 
#want accurate answers out of total and last alpha for each 

demographics_full_data <- read.csv("behavioral_data_raw/STAG consent and Demographics_April 14, 2023_17.58.csv") 
demographics_full_data <- demographics_full_data[-1,] #removing row w/ column names
demographics_full_data <- demographics_full_data[-1,] #removing row w/ import data
demographics_full_data <- demographics_full_data[order(demographics_full_data$Subject..,  
                                                       decreasing = T),]
  

#fixing demographic errors (birthdays listed as the date)
col_index <- which(colnames(demographics_full_data) == "Date")
row_index <- which(demographics_full_data$Subject.. == 7521)

demographics_full_data[row_index, col_index] <- "4/11/23"


candy_slim_full_data <- read.csv("behavioral_data_raw/slim/9856_Candy_responses 2.csv") 

maps_slim_full_data <- read.csv("behavioral_data_raw/slim/9857_Maps_responses 2.csv")

# #adding overall bilingual category to full demographics
# #see under "lang.classification2"
# demographics_full_data <- demographics_full_data %>%
#   mutate(lang.classification2 = NA)

# for (i in 1:nrow(demographics_full_data)){
#   if (grepl("Early Bilingual", demographics_full_data$lang.classification[i])) {
#     demographics_full_data$lang.classification2[i] <- "Bilingual"
#   } else if (grepl("Late Bilingual", demographics_full_data$lang.classification[i])){
#     demographics_full_data$lang.classification2[i] <- "Bilingual"
#   } else if (grepl("Monolingual", demographics_full_data$lang.classification[i])){
#     demographics_full_data$lang.classification2[i] <- "Monolingual"
#   } else {
#     demographics_full_data$lang.classification2[i] <- "None of the above"
#   }
# }

# #pulling bilingual 
# bi_demographics <- demographics_full_data %>%
#   subset(lang.classification2 == "Bilingual") %>%
#   bind_rows(subset(demographics_full_data, lang.classification == "Late Bilingual"))

# #pulling monolingual
# mono_demographics <- demographics_full_data %>%
#   subset(lang.classification == "Monolingual")
# 
# #pulling "none of the above"
# unknown_lang_demographics <- mono_demographics <- demographics_full_data %>%
#   subset(lang.classification == "None of the above")

#removing Teddy
candy_slim_full_data <- candy_slim_full_data %>%
  filter(screen_name != "TeddyHaile")
maps_slim_full_data <- maps_slim_full_data %>%
  filter(screen_name != "TeddyHaile")

#getting data for first/last version===========================================

#making lists for which subjects did what first 
maps_first_subject_ids <- list()
 for (i in seq(7500, demographics_full_data$Subject[1], by = 2)) {
   maps_first_subject_ids[[length(maps_first_subject_ids) + 1]] <- i
 }
maps_first_subject_ids <- tibble(maps_first_subject_ids)

candy_first_subject_ids <- list()
for (i in seq(7501, demographics_full_data$Subject..[1], by = 2)) {
  candy_first_subject_ids[[length(candy_first_subject_ids) + 1]] <- i
}
candy_first_subject_ids <- tibble(candy_first_subject_ids)

#putting everything together===============================================
#categorizing candy
candy_slim_full_data <- candy_slim_full_data %>%
  mutate(Type = "C", .before = "screen_name") %>%
  rename("Subject.." = "screen_name") %>%
  mutate(presented_first = NA, .before = "Subject..")
#marking if first
candy_slim_full_data$presented_first[candy_slim_full_data$Subject.. %in% 
                                       candy_first_subject_ids$candy_first_subject_ids] <- TRUE
candy_slim_full_data$presented_first[candy_slim_full_data$Subject.. %in% 
                                       maps_first_subject_ids$maps_first_subject_ids] <- FALSE
#marking if second
candy_slim_full_data <- candy_slim_full_data %>%
  mutate(presented_second = NA, .before = "Subject..")

candy_slim_full_data$presented_second[candy_slim_full_data$Subject.. %in% 
                                       candy_first_subject_ids$candy_first_subject_ids] <- FALSE
candy_slim_full_data$presented_second[candy_slim_full_data$Subject.. %in% 
                                       maps_first_subject_ids$maps_first_subject_ids] <- TRUE

#categorizing maps
maps_slim_full_data <- maps_slim_full_data %>%
  mutate(Type = "M", .before = "screen_name") %>%
  rename("Subject.." = "screen_name") %>%
  mutate(presented_first = NA, .before = "Subject..")
#marking if first
maps_slim_full_data$presented_first[maps_slim_full_data$Subject.. %in% 
                                       maps_first_subject_ids$maps_first_subject_ids] <- TRUE
maps_slim_full_data$presented_first[maps_slim_full_data$Subject.. %in% 
                                       candy_first_subject_ids$candy_first_subject_ids] <- FALSE
#marking if second
maps_slim_full_data <- maps_slim_full_data %>%
  mutate(presented_second = NA, .before = "Subject..")

maps_slim_full_data$presented_second[maps_slim_full_data$Subject.. %in% 
                                        maps_first_subject_ids$maps_first_subject_ids] <- FALSE
maps_slim_full_data$presented_second[maps_slim_full_data$Subject.. %in% 
                                        candy_first_subject_ids$candy_first_subject_ids] <- TRUE

master_slim <- rbind(maps_slim_full_data, candy_slim_full_data)
master_slim <- master_slim[order(master_slim$Subject..,  
                                                       decreasing = F),]

#Getting highest sequence reached per subject per type=======================
#maps
maps_subject_list <- tibble(maps_slim_full_data$Subject..) %>%
  distinct() %>%
  rename("Subject.." = "maps_slim_full_data$Subject..")

maps_last_sequence <- data_frame(matrix(nrow = nrow(maps_subject_list), ncol = 1))
for (i in 1:nrow(maps_last_sequence)){
maps_last_sequence[i, 1] <- max(subset(master_slim, Subject.. == maps_subject_list$Subject..[i] &
                           Type == "M")$sequence_number)
}
maps_last_sequence <- maps_last_sequence %>%
  mutate(Subject.. = maps_subject_list$Subject..) %>%
  rename("last_sequence_number" = "matrix(nrow = nrow(maps_subject_list), ncol = 1)")

#candy
candy_subject_list <- tibble(candy_slim_full_data$Subject..) %>%
  distinct() %>%
  rename("Subject.." = "candy_slim_full_data$Subject..")

candy_last_sequence <- data_frame(matrix(nrow = nrow(candy_subject_list), ncol = 1))
for (i in 1:nrow(candy_last_sequence)){
  candy_last_sequence[i, 1] <- max(subset(master_slim, Subject.. == candy_subject_list$Subject..[i] &
                                           Type == "C")$sequence_number)
}
candy_last_sequence <- candy_last_sequence %>%
  mutate(Subject.. = candy_subject_list$Subject..) %>%
  rename("last_sequence_number" = "matrix(nrow = nrow(candy_subject_list), ncol = 1)")

#figure out how to do this after combining the last sequences 
maps_test <- maps_last_sequence %>%
  left_join(master_slim, by = "Subject..") %>%
  subset(sequence_number == last_sequence_number)
















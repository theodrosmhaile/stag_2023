library("SlimStampeRData")
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
library("readr")
# SlimStampen
# uncomment "install.packages", "uncomment devtools" code
# install.packages("devtools") # Install SlimStampen packages. Instructions on https://github.com/VanRijnLab/SlimStampeRData
library("devtools")
#  devtools::install_github("VanRijnLab/SlimStampeRData",
#   build_vignettes = TRUE,
#  dependencies = TRUE,
# force = TRUE)
# The console will show a prompt asking whether all packages should be updated. Select option 1 (by typing 1 and then pressing enter), this will update all packages.
#vignette("SlimStampeRVignette")


# function to capitalize the first letter of a word
capitalize_word <- function(word) {
  paste0(toupper(substr(word, 1, 1)), substr(word, 2, nchar(word)))
}

#Categorize as monolingual/bilingual, but keep data on early vs late acquisition for bilingual ? 
#What to do about "none of the above"?
#candy first and last, maps first and last
#language 
#facet ; box no -> candy and map box w x axis candy first or last, colors language, y accuracy 

#lang classification df=================================================
langs <- read_dataset("behavioral_data_raw/STAG_lang.csv") %>%
  rename("screenName" = "screen_name") #for consistency 
langs <- langs[,-3] #removing gender column
langs <- subset(langs, !duplicated(langs)) #removing dups

#setting up candy data==================================================
candy_raw_data <- read_dataset("behavioral_data_raw/slim/9856_Candy_responses.csv") %>%
  rename("presentationStartTime" = "presentation_start_time")

# get the column names of the data
col_names <- colnames(candy_raw_data)

# split the column names into words
col_words <- strsplit(col_names, "_")

# capitalize the second word of each column name
col_words_modified <- lapply(col_words, function(x) {
  if(length(x) > 1) {
    x[2] <- capitalize_word(x[2])
  }
  return(x)
})

# paste the modified column names back together
new_col_names <- sapply(col_words_modified, function(x) {
  paste(x, collapse = "")
})

# set the new column names
colnames(candy_raw_data) <- new_col_names


#repeat for maps========================================================
maps_raw_data <- read_dataset("behavioral_data_raw/slim/9857_Maps_responses.csv") %>%
  rename("presentationStartTime" = "presentation_start_time")

# set the new column names
colnames(maps_raw_data) <- new_col_names


#removing Teddy==============================================
candy_raw_data <- candy_raw_data %>%
  filter(screenName != "TeddyHaile")
maps_raw_data <- maps_raw_data %>%
  filter(screenName != "TeddyHaile")

#make columns for lesson + repetition + time of day=============================
candy_raw_data <- candy_raw_data %>%
  mutate(lessonTitle = "C", .before = "screenName") %>%
  calculate_repetition()
maps_raw_data <- maps_raw_data %>%
  mutate(lessonTitle = "M", .before = "screenName") %>%
  calculate_repetition()

candy_raw_data <- candy_raw_data %>%
  mutate(candyBeforeNoon = createTime)
#fix time
candy_raw_data$candyBeforeNoon <- as.POSIXct(candy_raw_data$candyBeforeNoon) 
candy_raw_data$candyBeforeNoon <- as.POSIXct(as.numeric(candy_raw_data$candyBeforeNoon) - 2*60*60, 
                                       origin="1970-01-01") 
#pull time only
candy_raw_data$candyBeforeNoon <- format(as.POSIXct(candy_raw_data$candyBeforeNoon), format = "%H:%M:%S")

for (i in 1:nrow(candy_raw_data)){
  if(candy_raw_data$candyBeforeNoon[i] > 12){
    candy_raw_data$candyBeforeNoon[i] <- FALSE
  } else {
    candy_raw_data$candyBeforeNoon[i] <- TRUE
  }
}
#repeat for maps
maps_raw_data <- maps_raw_data %>%
  mutate(mapsBeforeNoon = createTime)
#fix time
maps_raw_data$mapsBeforeNoon <- as.POSIXct(maps_raw_data$mapsBeforeNoon) 
maps_raw_data$mapsBeforeNoon <- as.POSIXct(as.numeric(maps_raw_data$mapsBeforeNoon) - 2*60*60, 
                                        origin="1970-01-01") 
#pull time only
maps_raw_data$mapsBeforeNoon <- format(as.POSIXct(maps_raw_data$mapsBeforeNoon), format = "%H:%M:%S")

for (i in 1:nrow(maps_raw_data)){
  if(maps_raw_data$mapsBeforeNoon[i] > 12){
    maps_raw_data$mapsBeforeNoon[i] <- FALSE
  } else {
    maps_raw_data$mapsBeforeNoon[i] <- TRUE
  }
}

#getting data for first/last version===========================================
#making lists for which subjects did what first
langs <- langs %>%
  arrange(desc(screenName))
candy_first_subject_ids <- list()
for (i in seq(7501, langs$screenName[1], by = 2)) {
  candy_first_subject_ids[[length(candy_first_subject_ids) + 1]] <- i
}
candy_first_subject_ids <- tibble(candy_first_subject_ids)

maps_first_subject_ids <- list()
for (i in seq(7500, langs$screenName[1], by = 2)) {
  maps_first_subject_ids[[length(maps_first_subject_ids) + 1]] <- i
}
maps_first_subject_ids <- tibble(maps_first_subject_ids)

#candy marking if first
candy_raw_data <- candy_raw_data %>%
  mutate(maps_first = NA, .before = "screenName") %>%
  mutate(candy_first = NA, .before = "screenName")
candy_raw_data$maps_first[candy_raw_data$screenName %in%
                                       candy_first_subject_ids$candy_first_subject_ids] <- FALSE
candy_raw_data$maps_first[candy_raw_data$screenName %in%
                                       maps_first_subject_ids$maps_first_subject_ids] <- TRUE
candy_raw_data$candy_first[candy_raw_data$screenName %in%
                            candy_first_subject_ids$candy_first_subject_ids] <- TRUE
candy_raw_data$candy_first[candy_raw_data$screenName %in%
                            maps_first_subject_ids$maps_first_subject_ids] <- FALSE
#maps marking if first
maps_raw_data <- maps_raw_data %>%
  mutate(maps_first = NA, .before = "screenName") %>%
  mutate(candy_first = NA, .before = "screenName")
maps_raw_data$maps_first[maps_raw_data$screenName %in%
                                 maps_first_subject_ids$maps_first_subject_ids] <- TRUE
maps_raw_data$maps_first[maps_raw_data$screenName %in%
                                 candy_first_subject_ids$candy_first_subject_ids] <- FALSE
maps_raw_data$candy_first[maps_raw_data$screenName %in%
                             candy_first_subject_ids$candy_first_subject_ids] <- TRUE
maps_raw_data$candy_first[maps_raw_data$screenName %in%
                             maps_first_subject_ids$maps_first_subject_ids] <- FALSE

#adding languages and averages====================================================
langs$screenName <- as.character(langs$screenName)
#candy
candy_raw_data <- full_join(candy_raw_data, langs, by = "screenName")
candy_avg <- candy_raw_data %>%
  group_by(screenName, maps_first, candy_first, language) %>%
  summarize(candy_mean_rof = mean(alpha))
accuracy_candy <- candy_raw_data %>%
  group_by(screenName)%>%
  summarize(candy_mean_accuracy = mean(correct))
candy_avg <- full_join(candy_avg, accuracy_candy)
#maps
maps_raw_data <- full_join(maps_raw_data, langs, by = "screenName")
maps_avg <- maps_raw_data %>%
  group_by(screenName, maps_first, candy_first, language) %>%
  summarize(maps_mean_rof = mean(alpha))
accuracy_maps <- maps_raw_data %>%
  group_by(screenName)%>%
  summarize(maps_mean_accuracy = mean(correct))
maps_avg <- full_join(maps_avg, accuracy_maps)
#combining data=====================================
master_data <- full_join(maps_avg, candy_avg) %>%
  group_by(screenName, maps_first, language)
# master_data$maps_first <- ifelse(master_data$maps_first == FALSE, "Candy First",
#                                  master_data$maps_first)
# master_data$maps_first <- ifelse(master_data$maps_first == TRUE, "Maps First",
#                                  master_data$maps_first)

# master_averages <- master_data %>%
#   group_by()

#adding columns for time they *started* the task
map_time <- subset(maps_raw_data, !duplicated(screenName)) %>%
  select(screenName, mapsBeforeNoon)
map_time <- map_time[4:5]
candy_time <- subset(candy_raw_data, !duplicated(screenName)) %>%
  select(screenName, candyBeforeNoon)
candy_time <- candy_time[4:5]
master_data <- left_join(master_data, map_time)
master_data <- left_join(master_data, candy_time)

dups <- master_data[duplicated(master_data$screenName), ]
#what is this????????????

candy_avg_plot <- plot_ly(master_data, x = ~language, color = ~maps_first, hoverinfo = "none") %>%
  add_trace(y = ~mean(maps_mean_rof), type = "bar") %>%
  layout(barmode = "group")






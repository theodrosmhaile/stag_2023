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
library("htmltools")
# SlimStampen
# uncomment "install.packages", "uncomment devtools" code
# install.packages("devtools") # Install SlimStampen packages. Instructions on https://github.com/VanRijnLab/SlimStampeRData
#  library("devtools")
#  devtools::install_github("VanRijnLab/SlimStampeRData",
#   build_vignettes = F,
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
#take out 7541 (missing maps), 7532, 7519(weird collins)
candy_raw_data <- read_dataset("behavioral_data_raw/slim/9856_Candy_responses.csv") %>%
  rename("presentationStartTime" = "presentation_start_time")
candy_raw_data <- candy_raw_data %>%
  filter(screen_name != "7541") %>%
  filter(screen_name != "7532") %>%
  filter(screen_name != "7519")
  
  # subset(master_data$screenName != "7532") %>%
  # subset(master_data$screenName != "7519")

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
maps_raw_data <- maps_raw_data %>%
  filter(screen_name != "7541") %>%
  filter(screen_name != "7532") %>%
  filter(screen_name != "7519")

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
master_data <- maps_avg
master_data <- full_join(master_data, candy_avg) %>%
  group_by(screenName, maps_first, language)


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

# candy_avg_plot <- plot_ly(master_data, x = ~language, color = ~maps_first, hoverinfo = "none") %>%
#   add_trace(y = ~mean(maps_mean_rof), type = "bar") %>%
#   layout(barmode = "group")
# master_data %>% 
#   pivot_longer(cols = c(mapsBeforeNoon, candyBeforeNoon), names_to = 'category', values_to = 'BeforeNoon'
#   )%>%
#   pivot_longer(cols=c(candy_mean_accuracy, maps_mean_accuracy), names_to = 'cat2', values_to = 'accuracy')#%>%
#   lm(accuracy ~ category, Beforenoon) 

#accuracy comparision, category and time=================================================

acc <- master_data %>% ungroup() %>%  select(candy_mean_accuracy, maps_mean_accuracy) %>% 
  pivot_longer(cols=c(candy_mean_accuracy, maps_mean_accuracy), names_to = 'cat2', values_to = 'accuracy') %>% 
  separate(cat2, into = 'category', remove = T) %>%
  mutate(screenName = rep(master_data$screenName, each = 2), .before = "category")
  
befrNoon <- master_data %>% ungroup() %>% select(mapsBeforeNoon, candyBeforeNoon) %>% 
    pivot_longer(cols = c(mapsBeforeNoon, candyBeforeNoon), names_to = 'cat2', values_to = 'BeforeNoon') %>% 
   separate(cat2, into = 'category', remove = T, sep='B') %>%
  mutate(screenName = rep(master_data$screenName, each = 2), .before = "category")
    
acc_by_time_df <- inner_join(acc, befrNoon, by='screenName', "category") %>%
  subset(category.x == category.y) %>%
  select(category.x, accuracy, BeforeNoon) %>%
  rename("category" = "category.x") %>%
  na.omit()
  
   
 #  
 # acc_by_time = inner_join(acc, befrNoon, by='category') %>% 
 #    unique() %>% drop_na()
 #  
 #  acc_by_time %>% 
 #    lm( accuracy ~ category * BeforeNoon, data  = . ) %>% 
 #    anova()
 #  
 #  acc_by_time %>% 
 #    # filter(category=='Maps') %>% 
 #    group_by(category, BeforeNoon) %>% 
 #    summarize(m=mean(accuracy), n=n(), 
 #              se = sd(accuracy)/sqrt(n))
  
 
 acc_by_time <- lm(accuracy ~ category * BeforeNoon, data  = acc_by_time_df ) %>% 
   anova()
 
 mean_acc_time_lang <- acc_by_time_df %>%
   group_by(BeforeNoon, category) %>%
   reframe(mean_accuracy = mean(accuracy), se = sd(accuracy)/sqrt(nrow(.)), BeforeNoon, category) %>%
   unique() 
for (i in 1:nrow(mean_acc_time_lang)){
  if(mean_acc_time_lang$BeforeNoon[i] == F){
    mean_acc_time_lang$BeforeNoon[i] <- "After Noon"
  } else{
    mean_acc_time_lang$BeforeNoon[i] <- "Before Noon"
  }
}
 
acc_by_time_plot_lines <- plot_ly(mean_acc_time_lang, x = ~category, color = mean_acc_time_lang$BeforeNoon) %>%
   add_trace(y= ~mean_acc_time_lang$mean_accuracy, type = "scatter", mode = "lines+markers")%>%
   layout(yaxis = list(title = "Mean Accuracy"),
          xaxis = list(title = "Test Type"))
acc_by_time_plot_lines

# acc_by_time_plot_bar <- plot_ly(mean_acc_time_lang, x = ~category, color = mean_acc_time_lang$BeforeNoon) %>%
#   add_trace(y= ~mean_acc_time_lang$mean_accuracy, type = "bar")%>%
#   layout(yaxis = list(title = "Mean Accuracy"),
#          xaxis = list(title = "Test Type")) %>%
#   add_errorbars(y = ~se)
# acc_by_time_plot_bar

acc_by_time_plot_bar <- ggplot(mean_acc_time_lang, aes(x = category, y = mean_accuracy, fill = BeforeNoon)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = mean_accuracy - se, ymax = mean_accuracy + se), 
                width = 0.3, position = position_dodge(0.9)) +
  theme_minimal() +
  labs(fill = "Time of Day")
acc_by_time_plot_bar
  
#figure out what's happening w grouping 


#one column: m/c, one column: beforenoon 1/2 
#look only at first, label before noon or after noon
 
#only by first slim==============================================================
 cfirst_x <- subset(master_data, master_data$candy_first == T)
 cfirst <- cfirst_x %>% ungroup() %>%  
   select(candy_mean_accuracy) %>% 
   pivot_longer(cols=c(candy_mean_accuracy), names_to = 'cat2', values_to = 'accuracy') %>% 
   separate(cat2, into = 'category', remove = T) %>%
   mutate(screenName = rep(cfirst_x$screenName), .before = "category") %>%
   mutate(BeforeNoon = cfirst_x$candyBeforeNoon)
 
 
 mfirst_x <- subset(master_data, master_data$maps_first == T)
 mfirst <- mfirst_x %>% ungroup() %>%  
   select(maps_mean_accuracy) %>% 
   pivot_longer(cols=c(maps_mean_accuracy), names_to = 'cat2', values_to = 'accuracy') %>% 
   separate(cat2, into = 'category', remove = T) %>%
   mutate(screenName = rep(mfirst_x$screenName), .before = "category") %>%
   mutate(BeforeNoon = mfirst_x$mapsBeforeNoon)

 
acc_first_only_df <- bind_rows(cfirst, mfirst) 
acc_first_only_df <- acc_first_only_df[-2]
acc_first_only <- lm(accuracy ~ BeforeNoon, data = acc_first_only_df) %>%
  anova()

mean_acc_first <- acc_first_only_df %>%
  group_by(BeforeNoon) %>%
  reframe(mean_accuracy = mean(accuracy), BeforeNoon) %>%
  unique()

acc_first_only_plot <- plot_ly(mean_acc_first,
  x = ~BeforeNoon, color = mean_acc_first$BeforeNoon) %>%
  add_trace(y= ~mean_acc_first$mean_accuracy, type = "bar", showlegend = F)%>%
  layout(yaxis = list(title = "Mean Accuracy for First Test"),
         xaxis = list(title = "Done Before Noon?"))

acc_first_only_plot

#language test=================================================================

acc_lang_df <- master_data %>% ungroup() %>%  select(candy_mean_accuracy, maps_mean_accuracy) %>% 
  pivot_longer(cols=c(candy_mean_accuracy, maps_mean_accuracy), names_to = 'cat2', values_to = 'accuracy') %>% 
  separate(cat2, into = 'category', remove = T) %>%
  mutate(screenName = rep(master_data$screenName, each = 2), .before = "category") %>%
  mutate(language = rep(master_data$language, each = 2)) %>%
  na.omit() %>%
  group_by(screenName) %>%
  summarize(accuracy = mean(accuracy), language) %>%
  unique()

acc_lang <- lm(accuracy ~ language, data = acc_lang_df) %>%
  anova()

mean_acc = reframe(acc_lang_df %>% group_by(language), 
                "Mean Accuracy" = mean(accuracy), language) %>%
  unique()

acc_lang_plot <- plot_ly(mean_acc, x = ~language, color = mean_acc$language) %>%
  add_trace(y= ~mean_acc$`Mean Accuracy`, type = "bar", showlegend = F)%>%
  layout(yaxis = list(title = "Mean Accuracy Across Tests"),
         xaxis = list(title = "Language Category"))

acc_lang_plot

# test_table <- knitr::kable(acc_lang) 
# print(test_table)

#language just mono or bi======================================================
#this is p-hacking lolololol
mono_bi <- acc_lang_df %>%
  subset(language != "None of the above")

acc_mono_bi_lang <- lm(accuracy ~ language, data = mono_bi) %>%
  anova()
 

#need to put error bars and do rof
 
 

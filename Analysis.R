


#Load in required libraries
library(tidyverse)
library(tidytext)
# library(reshape2)
# library(stringi)
# library(stringi)
# library(rmarkdown)
# library(knitr)
# library(eeptools)
library(lubridate)
#library(textdata)


#Load all files and rbind
#Load all files and rbind
files = list.files(paste(getwd(), "/data", sep = ""), pattern=".csv", full.names=T)
files

# files[1]
# str_detect(files[1], as.character(Sys.Date()))
#
# strsplit(files[1], split = " ")

#Loop over each file, read and rbind each of them
files_n <- c()


#iterate to fetch all files made today
for (i in 1:length(files)){
  if (str_detect(files[i], as.character(Sys.Date())) == T){
    files_n[i] = files[i]
  }
}

#drop nas
a <- na.omit(files_n) %>%
  as.vector()

a


df <- do.call("rbind",lapply(a, read.csv))


##
#temp data
#df <- read.csv("C:\\Users\\User\\Documents\\GitHub\\DataOpsRetry\\data\\bbnreunion 2021-06-29 23-39-26.csv")


#Get the distinct tweets
df <- df %>%
  rename(tweet = text) %>%
  distinct(tweet, .keep_all = T) #This is to remove all duplicate tweets


#Regular expression (Regex) function for extracting handles mentioned
users <- function(x, ...){
  xx <- strsplit(x, " ")
  lapply(xx, function(xx)xx[grepl("@[[:alnum:]]", xx)])
}
#Most mention words
removeURL2 <- function(x) gsub("([[:alpha:]])(?=\\1)", "", x, perl = TRUE)
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)


#Extract the most mentioned handles
users(df$tweet) %>%
  unlist() %>%
  tolower() %>%
  as_tibble() %>%
  count(value, sort = T) %>%
  top_n(30) %>%
  mutate(date = Sys.Date()) %>%
  write.table(.,
              "Analysisdata/TopHandles.csv", sep = ",",
              col.names = !file.exists("Analysisdata/TopHandles.csv"),
              append = T, row.names = F)


#Most mention words
df %>%
  mutate(text = tolower(tweet)) %>%
  #mutate(text = removeURL2(text)) %>%
  mutate(text = removeNumPunct(text)) %>%
  mutate(text = gsub("brt", "", text)) %>%
  mutate(text = gsub("nultimateloveng", "ultimateloveng", text)) %>%
  mutate(text = gsub("bultimateloveng", "ultimateloveng", text)) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(word, sort = T) %>%
  slice_max(order_by = n, n = 30) %>%
  mutate(date = Sys.Date()) %>%
  write.table(.,"Analysisdata/TopWords.csv",
              sep = ",",
              col.names = !file.exists("Analysisdata/TopWords.csv"),
              append = T, row.names = F)


#read nrc
nrc <- read.csv("nrc/nrc.csv") %>%
  select(3:4)
#rm(nrc)
#Reactions on comments
df %>%
  mutate(text = tolower(tweet)) %>%
  mutate(text = removeURL2(text)) %>%
  mutate(text = gsub("brt", "", text)) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  inner_join(nrc) %>%
  count(word, sentiment, sort = T) %>%
  distinct(word, .keep_all = T) %>%
  ungroup() %>%
  group_by(sentiment) %>%
  summarise(n = sum(n)) %>%
  mutate(date = Sys.Date()) %>%
  write.table(.,"Analysisdata/Reactions.csv",
              sep = ",",
              col.names = !file.exists("Analysisdata/Reactions.csv"),
              append = T, row.names = F)


#Find general daily trend
df %>%
  separate(created, into = c("date", "time"), sep = " ") %>%
  mutate(date = ymd(date)) %>%
  mutate(hr = hour(hms(time))) %>%
  mutate(tm = ifelse(hr < 12, "am", "pm")) %>%
  group_by(date) %>%
  count() %>%
  write.table(.,"Analysisdata/dailytrend.csv",
              sep = ",",
              col.names = !file.exists("Analysisdata/dailytrend.csv"),
              append = T, row.names = F)

#Analysis for an hourly trend
df %>%
  separate(created, into = c("date", "time"), sep = " ") %>%
  mutate(date = ymd(date)) %>%
  mutate(hr = hour(hms(time))) %>%
  mutate(tm = ifelse(hr < 12, "am", "pm")) %>%
  unite(time, hr, tm, sep = " ") %>%
  group_by(time) %>%
  count() %>% write.table(.,"Analysisdata/hrtrend.csv",
                          sep = ",",
                          col.names = !file.exists("Analysisdata/hrtrend.csv"),
                          append = T, row.names = F)



#Week day trend
df %>%
  separate(created, into = c("date", "time"), sep = " ") %>%
  mutate(date = ymd(date)) %>%
  mutate(day = weekdays(date)) %>%
  group_by( day) %>%
  count() %>%
  write.table(.,"Analysisdata/day_tweets.csv",
              sep = ",",
              col.names = !file.exists("Analysisdata/day_tweets.csv"),
              append = T, row.names = F)

##Overall bing trend
df %>%
  mutate(tweet = removeURL2(tweet)) %>%
  mutate(tweet = removeNumPunct(tweet)) %>%
  mutate(tweet = tolower(tweet)) %>%
  mutate(tweet = gsub("wil", "", tweet)) %>%
  mutate(tweet = gsub("ben", "", tweet)) %>%
  mutate(tweet = gsub("al", "", tweet)) %>%
  mutate(tweet = gsub("ned", "", tweet)) %>%
  unnest_tokens(word, tweet) %>%
  anti_join(stop_words) %>%
  inner_join(get_sentiments("bing")) %>%
  separate(created, into = c("date", "time"), sep = " ") %>%
  group_by(sentiment, date) %>%
  count() %>%
  filter(nchar(date) >0) %>% 
  write.table(.,"Analysisdata/bing_trend.csv",
              sep = ",",
              col.names = !file.exists("Analysisdata/bing_trend.csv"),
              append = T, row.names = F)


#
#install.packages("twitteR")

library(twitteR)
#install.packages("tidyverse")
#library(tidyverse)
library(magrittr)
#Setup
consumerKey <- "U6JAH49FbjrBFzTFmcekc23yd"

consumerSecret <- "ZUHLYDigrPweCKCcDH155DAI52Fwiy7OHWE7h0OkO0iBYlmRAz"

accessToken <- "1097552545411678208-y5rc6032HoAJkZVLuHPcoGmHcNqBci"

accessTokenSecret <-  "llvmFhTYzrqnKMr0cIoYprXVT7K8wCOk4NEUjirzY1y6R"

#set up
setup_twitter_oauth(consumerKey,consumerSecret,accessToken,accessTokenSecret)


#demo data
name <- "#bbnreunion"

#scrape
# bbn <- searchTwitter(name, n = 1000,
#                       resultType = "recent", lang = "en") %>%
#   twitteR::twListToDF()



#now time - Streams for 60 seconds
now <- Sys.time() + 60 #* 5

#set data name
dataname <- gsub(":", "-",paste("data/","bbnreunion ", as.character(Sys.time()), ".csv", sep = ""))


###stream in tweets
while (now >= Sys.time()){
  bbn <- searchTwitter(name, n = 100,
                       resultType = "recent", lang = "en") %>%
    twitteR::twListToDF()
  write.table(bbn,
              dataname, sep = ",",
              col.names = !file.exists(dataname),
              append = T, row.names = F)

}


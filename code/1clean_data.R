library(readr)
library(stringi)
library(tidyr)

df <- read_csv("E:/Data/VK/vk_10m_profiles_2018-02-14.csv", n_max = 1000000)

text_fields <- c("interests", "music", "movies", "tv", "books",
                 "career.position")

## Lowercase and cut long string fields ####

lowercase_column <- function(column){
  sapply(column, function(string){
    ifelse(nchar(string) > 200, NA, tolower(string))
  })
}


df[,text_fields] <- apply(df[,text_fields], 2, lowercase_column)



temp <- freqtab(df$music, 1000)

## Clean birthyear ####
df$byear <- df$bdate
df$byear[nchar(df$byear) < 6] <- NA
df$byear <- as.numeric(substr(df$byear,
                              nchar(df$byear)-3,
                              nchar(df$byear)))

## Working with music #####

df_music <- df[,c("id", "first_name", "last_name", "sex", "city.title", "country.title",
                  "personal.political", "career.position", "byear" ,"music")]

df_music <- separate_rows(df_music, music)

freqtab(df_music$music, 100)

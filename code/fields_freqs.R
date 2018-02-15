library(readr)

df <- read_csv("data/vk_10000000profiles_2018-02-14.csv")

source("https://raw.githubusercontent.com/alexeyknorre/stir/master/table_output.R")

df2 <- df[sample(nrow(df), 100000), ]

freqtab(df$first_name)

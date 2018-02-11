library(vkR)
library(testthat)
options(scipen = 999)

setwd("./data/")

at <- "d75deb97e8e79b03fb86565adb1353e23e2c7dd0cb44106cdc55f05227ad0b3e64e152a685456ac4cc421"
setAccessToken(access_token = at)

# Parameters
number <- 10000000

# Set fields to get
fields <- paste("city, country, bdate, sex, last_seen",
                "personal, education, home_town",
                "books, music, games, interests, movies, tv",
                sep = ", ")

get_profiles <- function(fields,
                         n,
                         batch_size = 10000,
                         n_try = 5) {
  start_time <- Sys.time()
  
  # Generate random ids
  ids <- sample(1:430e6, n)
  
  # Hook if n is small
  if(n <= batch_size){
  data <- getUsersExecute(ids, fields = fields, flatten = T)
  return(data)
  }

  batched_ids <- split(ids,
                       ceiling(
                         seq_along(ids)/batch_size))
  
  # First iteration
  print("Batches:")
  print(paste0("1/",length(batched_ids),"..."))
  
  start_ids <- as.numeric(unlist(batched_ids[1]))
  try_again(n_try, data <- getUsersExecute(start_ids, fields = fields,flatten = T))
  c <- 2
  
  
  for (i in batched_ids[2:length(batched_ids)]){
      print(paste0(c,"/", length(batched_ids),"..." ))
      try_again(n_try, d <- getUsersExecute(i, fields = fields,flatten = T,progress_bar = T))
      rownames(d) <- d$id
      data <- rbind(data,d)
      c <- c + 1
  }
  
  # Print used time
  print("Time spent:")
  print(Sys.time() - start_time)
  
  return(data)
}

save_profiles <- function(dataframe,
                          gzip = T) {
  if(gzip == T)
  {
    z <- gzfile(paste0("vk_",nrow(dataframe),"profiles_", Sys.Date(),".csv.gz"))
    write.csv(dataframe, z, row.names = F)
  }
  else{
    write.csv(dataframe, paste0("vk_",nrow(dataframe),"profiles_", Sys.Date(),".csv"),
              row.names = F)
  }
}

df <- get_profiles(fields, n = number)

save_profiles(df, gzip = F)

#data <- read.csv("vk_100profiles_2018-02-07.csv")




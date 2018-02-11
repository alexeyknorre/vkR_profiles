library(vkR)
library(testthat)
options(scipen = 999)

setwd("./data/")

at <- "d75deb97e8e79b03fb86565adb1353e23e2c7dd0cb44106cdc55f05227ad0b3e64e152a685456ac4cc421"
setAccessToken(access_token = at)

df <- data.frame(n=NA, batch=NA, time_seconds=NA)


# Set fields to get
fields <- paste("city, country, bdate, sex, last_seen",
                "home_town",
                "books, music, games", sep = ", ")

get_profiles_test <- function(fields,
                         n,
                         batch_size) {
  start_time <- Sys.time()
  
  # Generate random ids
  ids <- sample(1:430e6, n)
  
  # Hook if n is small
  if(n <= batch_size){
     try_again(5, data <-getUsersExecute(ids, fields = fields, flatten = T))
    return(as.numeric(Sys.time() - start_time, units = "secs"))
  }
  
  batched_ids <- split(ids,
                       ceiling(
                         seq_along(ids)/batch_size))
  
  # First iteration
  #print("Batches:")
  #print(paste0("1/",length(batched_ids),"..."))
  
  start_ids <- as.numeric(unlist(batched_ids[1]))
  try_again(5, data <- getUsersExecute(start_ids, fields = fields, flatten = T))
  c <- 2
  
  
  for (i in batched_ids[2:length(batched_ids)]){
    #print(paste0(c,"/", length(batched_ids),"..." ))
    try_again(5, d <- getUsersExecute(i, fields = fields,flatten = T))
    rownames(d) <- d$id
    data <- rbind(data,d)
    c <- c + 1
  }
  
  # Print used time
  return(as.numeric(Sys.time() - start_time, units = "secs"))
}

#n_seq <- c(10000, 50000, 100000, 500000, 1000000)
n_seq <- c(500000, 1000000)
batch_seq <- c(1000, 5000, 10000, 50000, 100000, 200000, 500000)

print("Start evaluating query parameters...")
c <- 0
for(i in n_seq){
  for(j in batch_seq){
    print(paste0("Total ", i,", batch ", j))
    c <- c+1
    df[c,] <- c(i,j,get_profiles_test(fields, n = i, batch_size = j))
    
  }
}

df$secs_per_1k <- df$time_seconds / df$n * 1000
df$n_of_batches <- df$n / df$batch
df$n_of_batches[df$n_of_batches < 0] <- 1

write.csv(df, "evaluation_2.csv")




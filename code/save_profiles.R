library(vkR)
library(testthat)
library(data.table)
options(scipen = 999)

at <- "d75deb97e8e79b03fb86565adb1353e23e2c7dd0cb44106cdc55f05227ad0b3e64e152a685456ac4cc421"
setAccessToken(access_token = at)

## Parameters #####
number <- 10000

# Set fields to get
fields <- paste("city, country, bdate, sex, last_seen",
                "personal, career",
                "books, music, games, interests, movies, tv",
                sep = ", ")

# Set fields to subset
fields_wide <- c("id", "first_name", "last_name", "sex", "bdate", 
                 "interests", "music", "movies", "tv", "books", "games",
                 "city.title", "country.title", "personal.political",
                 "career.position")
### Functions #####

get_profiles <- function(n,
                         #fields,
                         #fields_wide,
                         batch_size = 5000,
                         n_try = 10) {
  start_time <- Sys.time()
  
  #profile_fields <- fields
  #profile_fields_wide <- fields_wide
  
  file_name <- paste0("data/","vk_",n,"profiles_", Sys.Date(),".csv")
  
  # Generate random ids
  ids <- sample(1:430e6, n)
  
  # Hook if n is small
  if(n <= batch_size){
    data <- getUsersExecute(ids, fields = fields, flatten = T, drop = T)
    #save_profiles(data, profile_fields_wide, file_name, file_append = F)
    save_profiles(data, fields_wide, file_name, file_append = F)
  }
  
  batched_ids <- split(ids,
                       ceiling(
                         seq_along(ids)/batch_size))
  
  # First iteration
  print("Batches:")
  print(paste0("1/",length(batched_ids),"..."))
  
  start_ids <- as.numeric(unlist(batched_ids[1]))
  try_again(n_try, data <- getUsersExecute(start_ids, fields,flatten = T, drop = T))
  #data <- getUsersExecute(start_ids, fields = fields,flatten = T)
  data <- as.data.table(data)
  #save_profiles(data, profile_fields_wide, file_name, file_append = F)
  save_profiles(data, file_name, file_append = F)
  c <- 2
  
  
  for (i in batched_ids[2:length(batched_ids)]){
    print(paste0(c,"/", length(batched_ids),"..." ))
    try_again(n_try, d <- getUsersExecute(i, fields = fields,flatten = T, drop = T))
    rownames(d) <- d$id
    d <- as.data.table(d)
    #save_profiles(d, profile_fields_wide, file_name)
    save_profiles(d, file_name)
    c <- c + 1
  }
  
  # Print used time
  print("Time spent:")
  print(Sys.time() - start_time)
}

save_profiles <- function(dataframe,
                          file_path,
                          file_append = T) {
  
  # Fix problems with columns
  dataframe$personal.langs <- lapply(dataframe$personal.langs,
                                     function(x) paste(unlist(x), collapse='|'))
  
  dataframe$career.position <- sapply(dataframe$career,
                                      function(x) paste0(x$position, collapse="|"))
  dataframe$career <- NULL
  
  # Subset fields to maintain structure
  dataframe <- dataframe[,..fields_wide]
  fwrite(dataframe, file_path, append = file_append, na = "", quote = T)
}

### Main call ######

get_profiles(number)

#data <- readr::read_csv("data/vk_10000profiles_2018-02-13.csv")
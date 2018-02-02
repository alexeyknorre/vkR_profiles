library(vkR)
#Sys.setlocale("LC_ALL", "Russian_Russia.1252")
options(scipen = 999)

setwd("./data/")

at <- "d75deb97e8e79b03fb86565adb1353e23e2c7dd0cb44106cdc55f05227ad0b3e64e152a685456ac4cc421"
setAccessToken(access_token = at)

# Set fields to det
fields <- paste('city, country, bdate, sex, last_seen')


# Generate ids
# Number of random ids
n <- 100

ids <- sample(1:430e6, n)


# Batch size
#l <- 2000

#batched_ids <- split(ids, ceiling(seq_along(ids)/l))

# Generate list of ids

#start_ids <- as.numeric(unlist(batched_ids[1]))
start_ids <- as.numeric(unlist(ids))

print(Sys.time())
data <- getUsersExecute(ids, fields = fields, flatten = T)
print(Sys.time())

# c <- 1  
# for (i in batched_ids[2:length(batched_ids)]){
#     print(paste(c, length(batched_ids) ))
#     d <- getUsersExecute(i, fields = fields,flatten = T)
#     rownames(d) <- d$id
#     data <- rbind(data,d)
#     c <- c + 1
# }

z <- gzfile("vk_user_profiles.csv.gz")
write.csv(data, z, row.names = F)
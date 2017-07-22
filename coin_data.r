# Cryptocurrency Price API Calls
# https://www.cryptocompare.com/api/

library(httr)
library(tidyjson)
library(jsonlite)
library(dplyr)

# coin <- GET('https://min-api.cryptocompare.com')

#### Mining Data
# For each key, create dataframe of sub data
# Add dataframe to list, combine all data frames into one, and return
mining_pull <- function(json_content = 'MiningData'){
        
        api_data <- httr::GET('https://www.cryptocompare.com/api/data/miningequipment/')
        x <- httr::content(api_data, type = "text", encoding = "UTF-8")
        
        mining_list <- list() #creat empty list for data frames
        #get list of keys
        key_df <- x %>%
                enter_object(json_content) %>%
                gather_keys()
        key_count <- length(key_df$key) #count of keys
        #for each key, create a dataframe
        for(i in 1:key_count) {
                z <- x %>%
                        enter_object(json_content) %>%
                        enter_object(as.character(key_df[i, 2])) %>%  ### fix this part
                        spread_values(id = jstring('Id'),
                                      company = jstring('Company'),
                                      name = jstring('Name'),
                                      algorithm = jstring('Algorithm'),
                                      hashes_per_sec = jstring('HashesPerSecond'),
                                      cost = jstring('Cost'),
                                      equipment = jstring('EquipmentType'),
                                      coin = jstring('CurrenciesAvailable')
                        )
                z$i <- i #add count
                mining_list[[i]] <- z
                remove(z)
                remove(i)
        }
        mining_df <- do.call(rbind, mining_list) #combine list to single data frame
        return(mining_df) # return
}

test <- mining_pull()



#########################
###  Scratch Area  #####
#######################

# ## this test for a specific key is working
# z <- tidy_test %>%
#         enter_object('MiningData') %>%
#         enter_object('15760') %>%
#         spread_values(id = jstring('Id'),
#                       company = jstring('Company'),
#                       name = jstring('Name'),
#                       algorithm = jstring('Algorithm'),
#                       hashes_per_sec = jstring('HashesPerSecond'),
#                       cost = jstring('Cost'),
#                       equipment = jstring('EquipmentType'),
#                       coin = jstring('CurrenciesAvailable')
#                       )
# 
# 
# # Tidyjson work
# tidy_test %>% prettify  # get a look at the data
# Test <- GET('https://min-api.cryptocompare.com/data/price?fsym=ETH&tsyms=BTC,USD,EUR,ETH')
# # test_content <- content(Test)
# # str(test_content)
# test_df <- data.frame(content(Test))
# str(test_df)
# 
# content(Test)
# 
# Test2 <- httr::GET('https://www.cryptocompare.com/api/data/miningequipment/')
# x <- httr::content(Test2, type = "text", encoding = "UTF-8")
# 
# tidy_test <- x
# 
# tidy_test %>%
#         gather_keys()
# 
# y <- tidy_test %>%
#         enter_object('MiningData') %>%
#         gather_keys()
# str(y)
# names(y)

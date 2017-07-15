# Cryptocurrency Price API Calls
# https://www.cryptocompare.com/api/

library('httr')
library(tidyjson)
library(jsonlite)
library(dplyr)

# coin <- GET('https://min-api.cryptocompare.com')
# str(content(coin))
# 
# coin_content <- content(coin)
# coin_content$AvailableCalls$Price
# content_df <- data.frame(coin_content$AvailableCalls$Price)

# coin_bit <- GET("https://min-api.cryptocompare.com/data/histominute?fsym=BTC&tsym=USD&limit=60&aggregate=1")
# content_df <- data.frame(content(coin_bit))


Test <- GET('https://min-api.cryptocompare.com/data/price?fsym=ETH&tsyms=BTC,USD,EUR,ETH')
# test_content <- content(Test)
# str(test_content)
test_df <- data.frame(content(Test))
str(test_df)

content(Test)

Test2 <- httr::GET('https://www.cryptocompare.com/api/data/miningequipment/')
str(Test2)
content(Test2)
x <- httr::content(Test2, type = "text", encoding = "UTF-8")
head(x)

# Tidyjson work
tidy_test %>% prettify  # get a look at the data
tidy_test %>% gather_keys %>% json_types() %>% json_lengths() # overview of structure
#tidy_test %>% gather_keys %>% json_types() %>% group_by(key, type) %>% tally

y <- tidy_test %>%
        spread_values(MiningD = jstring('MiningData')) %>%
        gather_keys()

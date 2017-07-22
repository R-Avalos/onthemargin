# Cryptocurrency Price API Calls
# https://www.cryptocompare.com/api/

library(httr)
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

###
tidy_test <- x

tidy_test %>%
        gather_keys()

y <- tidy_test %>%
        enter_object('MiningData') %>%
        gather_keys()
str(y)
names(y)


### Fix this
y2 <- tidy_test %>%
        enter_object('MiningData') %>%
        spread_values(key = jstring('key ')) %>%
        enter_object('key ') %>%  ### this is failing... neeed to go rowwise
        gather_array() %>%
        spread_values(id = jstring('Id'),
                      company = jstring('Company'),
                      name = jstring('Name'),
                      algorithm = jstring('Algorithm'),
                      hashes_per_sec = jstring('HashesPerSecond'),
                      cost = jstring('Cost'),
                      equipment = jstring('EquipmentType'),
                      coin = jstring('CurrenciesAvailable')
        )

#### There are 62 keys!!!!  
tidy_test %>% as.tbl_json() %>%
        enter_object('MiningData') %>%
        gather_keys()

tidy_test2 <- tidy_test %>% as.tbl_json() %>%
        enter_object('MiningData')
tidy_test2 %>% gather_keys()
tidy_test2 %>% 
        spread_values(id = jstring('Id'))

#### Okay, for each keys .. e.g '35204', "16080" in the list... save to data frame and then stack the data frames... fun stuff


## 
tidy_test %>% as.tbl_json() %>%
        gather_keys()
tidy_test %>% as.tbl_json() %>% str()
tidy_test[[1]]

json <- '[{"country":"us","city":"Portland","topics":[{"urlkey":"videogame","name":"Video Games","id":4471},{"urlkey":"board-games","name":"Board Games","id":19585},{"urlkey":"computer-programming","name":"Computer programming","id":48471},{"urlkey":"opensource","name":"Open Source","id":563}],"joined":1416349237000,"link":"http://www.meetup.com/members/156440062","bio":"Analytics engineer.  Primarily work in the Hadoop space.","lon":-122.65,"other_services":{},"name":"Aaron Wirick","visited":1443078098000,"self":{"common":{}},"id":156440062,"state":"OR","lat":45.56,"status":"active"}]'

json %>% as.tbl_json() %>% gather_array() %>%
        gather_keys()
        
        spread_values(country = jstring('country')) %>%
        enter_object('topics') %>%
        gather_array() %>%
        gather_keys()


## this test for a specific key is working
z <- tidy_test %>%
        enter_object('MiningData') %>%
        enter_object('15760') %>%
        spread_values(id = jstring('Id'),
                      company = jstring('Company'),
                      name = jstring('Name'),
                      algorithm = jstring('Algorithm'),
                      hashes_per_sec = jstring('HashesPerSecond'),
                      cost = jstring('Cost'),
                      equipment = jstring('EquipmentType'),
                      coin = jstring('CurrenciesAvailable')
                      )


# Tidyjson work
tidy_test %>% prettify  # get a look at the data
tidy_test %>% minify
tidy_test %>% gather_keys %>% json_types() %>% json_lengths() # overview of structure
#tidy_test %>% gather_keys %>% json_types() %>% group_by(key, type) %>% tally


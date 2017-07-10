# Cryptocurrency Price API Calls
# https://www.cryptocompare.com/api/

library('httr')

coin <- GET('https://min-api.cryptocompare.com')
str(content(coin))

coin_content <- content(coin)
coin_content$AvailableCalls$Price
content_df <- data.frame(coin_content$AvailableCalls$Price)


coin_bit <- GET("https://min-api.cryptocompare.com/data/histominute?fsym=BTC&tsym=USD&limit=60&aggregate=1")
content_df <- data.frame(content(coin_bit))

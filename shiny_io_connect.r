### Dashboard for website

# Initial token and secret setup
# install.packages('rsconnect')
# library(rsconnect)
# rsconnect::setAccountInfo(name='randy-avalos',
#                           token='Token',
#                           secret='secret') # setting up connection to shinyapps.io

library(rsconnect)
rsconnect::deployApp('path/to/your/app')
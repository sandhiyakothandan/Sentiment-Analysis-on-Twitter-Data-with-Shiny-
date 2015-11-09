rm(list=ls())
library("twitteR")
library("ROAuth")
library(base64enc)
#I am using the direct authentication method here , so incase if there is an error with the oauth please run this again and
#choose "1" for YES in case of direct authentication 

consumer_key = "4JTnQWS4GTjiAUMAip9cOOgTN"
consumer_secret = "YrvhqyjBqisvPY3rPYKJDbsj51ZwDMCVMv5xQabuE5uPu7qCaS"
access_token <- "113685444-c9IyRYDFjzT3aWVQHfIurw2BKfR2YNX79TsNBkBP"
access_secret <- "rW0S53xj24z1dHZAq5hLEEApeQ09si0nhIdolQxtNYvoN"
#Credentials$handshake()
#Credentials$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))

cred <- setup_twitter_oauth(consumer_key, consumer_secret, access_token  , access_secret)

#specifiy the proper path to store the credentials 

save(cred ,file="C:\\Users\\Sandhya\\Documents\\shiny_1\\credentials.RData")



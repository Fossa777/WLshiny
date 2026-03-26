install.packages('rsconnect')
rsconnect::setAccountInfo(name='fossa7',
                          token='A82B9E9A6C8DA359B5381739A6372ABB',
                          secret='cZg7DDY1bZPjeunp3ihpEVmkq0wMJX2mVt2sUtiP')

library(rsconnect)
rsconnect::deployApp('C:/Users/stepankov_ap/Documents/WLshiny/')

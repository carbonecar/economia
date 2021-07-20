library("httr")
library("jsonlite")

#Get to call: 
#http GET https://api-v2.intrinio.com/companies/AAPL?api_key=OmNkZTViY2YzNmI3YTFjMDZjMjNiN2Y3ZWU4ZWU5NjZh

base <- "https://api-v2.intrinio.com/"
endpoint <- "companies/"
stock <- "AAPL"
api_key="OmNkZTViY2YzNmI3YTFjMDZjMjNiN2Y3ZWU4ZWU5NjZh"

call1 <- paste(base,endpoint,stock,"?","api_key","=", api_key, sep="")
call1

get_prices<-GET(call1)

company_json<-content(get_prices)

company_json



#######################################
######### Autorization with header#####
######################################
bcra_base <- "https://api.estadisticasbcra.com/"
endpoint_base_monetaria_pesos <- "base"
bcraToken="eyJhbGciOiJIUzUxMiIsInR5cCI6IkpXVCJ9.eyJleHAiOjE2NDc5MTM4MTksInR5cGUiOiJleHRlcm5hbCIsInVzZXIiOiJjYXJib25lY2FyQGdtYWlsLmNvbSJ9.E9YuyrzpEpS6Jy322r4EZq04H8WvWmPOAuopfB5RLcps7sSWDqFLnByi8yAFsZIXLkqMG7I-FJgH9ELkCiXGmg"
baseMonetariaPesos <- paste(bcra_base,endpoint_base_monetaria_pesos,sep="")
baseMonetariaPesos

baseMonetaria_response=GET(baseMonetariaPesos,add_headers(Authorization=paste("Bearer", bcraToken)))

base_monetaria_json<-content(baseMonetaria_response)



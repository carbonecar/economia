library(quantmod)


vstr_crypto <- "
code        ,    name
BTC-USD     ,    Bitcoin USD        
ETH-USD     ,    Ethereum USD
USDT-USD    ,    Tether USD
USDC-USD    ,    USD Coin USD
BNB-USD     ,    Binance Coin USD
ADA-USD     ,    Cardano USD
XRP-USD     ,    XRP USD
BUSD-USD    ,    Binance USD USD
SOL-USD     ,    Solana USD
HEX-USD     ,    HEX USD
DOT-USD     ,    Polkadot USD
DOGE-USD    ,    Dogecoin USD
AVAX-USD    ,    Avalanche USD
MATIC-USD   ,    Polygon USD
DAI-USD     ,    Dai USD
WTRX-USD    ,    Wrapped TRON USD
SHIB-USD    ,    SHIBA INU USD
STETH-USD   ,    Lido stETH USD
UNI1-USD    ,    Uniswap USD
TRX-USD     ,    TRON USD
ETC-USD     ,    Ethereum Classic USD
WBTC-USD    ,    Wrapped Bitcoin USD
LEO-USD     ,    UNUS SED LEO USD
LTC-USD     ,    Litecoin USD
NEAR-USD    ,    NEAR-USD
"

#——————————————-
# split symbols and make vector
#——————————————-
df      <- read.table(text = vstr_crypto, 
                      sep = ",", header = TRUE)
df      <- as.data.frame(df)
df$code <- gsub("[\t\r\n ,]","",df$code)
df$name <- gsub("[\t\r\n ,]","",df$name)
df
nc <- nrow(df) # number of crypto

#——————————————-
# read price information
#——————————————-

# limitation of data length
# BTC                 : from 2014-09-17
# ETH and some coins  : from 2017-11-09
# others              : short period

sdate <- as.Date("2017-11-09")
edate <- as.Date("2023-07-27")
getSymbols(df$code,from=sdate,to=edate)

#——————————————-
# collect only adjusted prices
#——————————————-
price <- NULL
for(i in 1:nc) {
  eval(parse(text=paste0(
    "price <- cbind(price,`",
         df$code[i],"`[,6])")))
}
 
# modify column name as only symbol

colnames(price) <- gsub(".USD.Adjusted", "", 
                        colnames(price))
 
#——————————————-
# print time series of daily prices
#——————————————-

head(price)

tail(price)

typeof(price)

mi_xts_tres_columnas <- price[, c("BTC", "DAI")]

plot.xts(mi_xts_tres_columnas, main = "BTC y DAI")

plot.xts(price[,c("DAI")], main = "DAI-USD")
plot.xts(price[,c("BTC")], main = "BTC-USD")


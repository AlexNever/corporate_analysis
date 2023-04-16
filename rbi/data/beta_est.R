# RBI Risk Return Analysis
library(dplyr)
library(ggplot2)
library(jtools)
library(jsonlite)

key = "&apikey=N5CGNQ7E4WTF457Z"
url = "https://www.alphavantage.co/query?"
func = "function="
sym = "&symbol="

returns = function(v){
  c(0,(v[-1]/rev(c(rev(v),0)[-1])[-1]) - 1)
}
left_join_df <- function(x, y) {
  dplyr::left_join(x, y, by = "V1")
}

# Read RBI and S&P500 data from repo
repo = "https://raw.githubusercontent.com/AlexNever/corporate_analysis/main/rbi/data/"
files = c("qsr_2018-2023.csv", "sp500_2018-2023.csv", "1year_tbills.csv", 'qsr_restaurants.csv')

# Left-join data 
temp = list()

## Read all data files
for ( i in files){
  temp[[which(files == i)]] = read.csv(paste(repo, i, sep = ""), skip = 0, header = FALSE)
}

## Clean stock and index file
joined_df <- Reduce(left_join_df, temp)
temp_href = joined_df[1,]
df = joined_df[-1,c(1,2,8)]
colnames(df) = c("date", "rbi_price", "sp500_price")
df = data.frame((lapply(df, function(x) {gsub(",", "", x)})))
df$date = as.Date(gsub("/", "-", df$date), format = "%m-%d-%Y")
for (i in c(2,ncol(df))){
  df[,i] = as.numeric(df[,i])
}

## Calculate 
df["rbi_ret"] = returns(df$rbi_price)
df["sp500_ret"] = returns(df$sp500_price)

## Find beta
beta_reg = lm(rbi_ret ~ sp500_ret, data = df)
summ(beta_reg, digits = 4)

## Find relevant monthly risk-free rate
risk_free = temp[[3]][-1,]
colnames(risk_free) = c("date", "rate")
risk_free$date = as.Date(gsub("/", "-", risk_free$date), format = "%m-%d-%Y")
rf_rel = risk_free[which(risk_free$date >= min(df$date) & risk_free$date <= max(df$date)),]
rf_avg = mean(as.numeric(rf_rel$rate))/100
rf_avg_monthly = rf_avg/12
paste(round(rf_avg,4)*100, round(rf_avg_monthly,4)*100)

## Jensens Alpha
alpha = c(beta_reg$coefficients)[[1]]
beta = c(beta_reg$coefficients)[[2]]

j_alpha = alpha - rf_avg_monthly*(1-beta)

plot(df$sp500_ret, df$rbi_ret)
